use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use crate::symbol::{
    CFGFirstSet, ContextFreeGrammar, NonTerminalHandle, ProductionHandle, SymbolHandle,
    TerminalHandle,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct CanonicalItem {
    pub production: ProductionHandle,
    pub lookahead: Option<TerminalHandle>,
    pub dot_position: usize,
}

impl CanonicalItem {
    pub fn new(
        production: ProductionHandle,
        lookahead: Option<TerminalHandle>,
        dot_position: usize,
    ) -> Self {
        Self {
            production,
            lookahead,
            dot_position,
        }
    }
}

impl PartialOrd for CanonicalItem {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CanonicalItem {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.production.cmp(&other.production) {
            std::cmp::Ordering::Equal => match self.lookahead.cmp(&other.lookahead) {
                std::cmp::Ordering::Equal => self.dot_position.cmp(&other.dot_position),
                ord => ord,
            },
            ord => ord,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct CanonicalItemSet {
    pub items: BTreeSet<CanonicalItem>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct CanonicalCollectionHandle(usize);

impl CanonicalCollectionHandle {
    pub fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn idx(&self) -> usize {
        self.0
    }
}

/// Actions in the LR parsing table
/// - Shift to a new canonical collection
/// - Reduce by a production
/// We ignore Accept actions, treating them as a special case of Reduce with lookahead None.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum CanonicalCollectionAction {
    Shift(CanonicalCollectionHandle),
    Reduce(ProductionHandle),
}

pub struct CanonicalCollection {
    pub item_set: Rc<CanonicalItemSet>,
    pub actions: BTreeMap<TerminalHandle, CanonicalCollectionAction>,
    pub eof_reduce: Option<ProductionHandle>,
    pub goto: BTreeMap<NonTerminalHandle, CanonicalCollectionHandle>,
}

impl CanonicalCollection {
    pub fn get_action(
        &self,
        lookahead: Option<TerminalHandle>,
    ) -> Option<CanonicalCollectionAction> {
        match lookahead {
            Some(t_handle) => self.actions.get(&t_handle).cloned(),
            None => self
                .eof_reduce
                .map(|prod_handle| CanonicalCollectionAction::Reduce(prod_handle)),
        }
    }
}

pub struct LRTable {
    pub states: Vec<CanonicalCollection>,
    pub start_state: CanonicalCollectionHandle,
}

impl LRTable {
    pub fn state(&self, handle: CanonicalCollectionHandle) -> &CanonicalCollection {
        &self.states[handle.idx()]
    }
}

pub(crate) struct LRTableBuilder<'a, TToken>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
{
    pub cfg: &'a ContextFreeGrammar<TToken>,
    pub first_set: CFGFirstSet,
    pub states: Vec<CanonicalCollection>,
    pub cc_lut: HashMap<Rc<CanonicalItemSet>, CanonicalCollectionHandle>,
    pub worklist: Vec<CanonicalCollectionHandle>,
}

impl<'a, TToken> LRTableBuilder<'a, TToken>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
{
    pub fn new(cfg: &'a ContextFreeGrammar<TToken>) -> Self {
        let first_set = cfg.first_set();
        Self {
            cfg,
            first_set,
            states: Vec::new(),
            cc_lut: HashMap::new(),
            worklist: Vec::new(),
        }
    }

    fn closure(&self, item: CanonicalItem) -> BTreeSet<CanonicalItem> {
        let mut closure_set = BTreeSet::new();
        let mut worklist = Vec::new();
        closure_set.insert(item.clone());
        worklist.push(item);
        while let Some(item) = worklist.pop() {
            if let Some(symbol) = self
                .cfg
                .production(item.production)
                .symbols
                .get(item.dot_position)
            {
                match symbol {
                    SymbolHandle::NonTerminal(nt_handle) => {
                        for (handle, _) in self.cfg.iter_productions(*nt_handle) {
                            let lookahead_terminals = if let Some(lookahead_symbol) = self
                                .cfg
                                .production(item.production)
                                .symbols
                                .get(item.dot_position + 1)
                            {
                                match lookahead_symbol {
                                    SymbolHandle::Terminal(t_handle) => vec![Some(*t_handle)],
                                    SymbolHandle::NonTerminal(nt_handle) => self
                                        .first_set
                                        .get_first_set(nt_handle)
                                        .iter()
                                        .flat_map(|set| set.iter())
                                        .map(|t_handle| Some(*t_handle))
                                        .collect(),
                                }
                            } else {
                                vec![item.lookahead]
                            };
                            for lookahead in lookahead_terminals {
                                let new_item = CanonicalItem::new(handle, lookahead, 0);
                                if !closure_set.contains(&new_item) {
                                    closure_set.insert(new_item.clone());
                                    worklist.push(new_item);
                                }
                            }
                        }
                    }
                    SymbolHandle::Terminal(_) => {}
                }
            }
        }
        closure_set
    }

    pub fn build(mut self) -> LRTable {
        let root_cc_handle = self.initialize_root_state();
        self.process_worklist();
        LRTable {
            states: self.states,
            start_state: root_cc_handle,
        }
    }

    fn initialize_root_state(&mut self) -> CanonicalCollectionHandle {
        let root_symbol = self.cfg.root;
        let root_item_set: BTreeSet<_> = self
            .cfg
            .iter_productions(root_symbol)
            .flat_map(|(handle, _)| {
                let item = CanonicalItem::new(handle, None, 0);
                self.closure(item)
            })
            .collect();
        let root_item_set_rc = Rc::new(CanonicalItemSet {
            items: root_item_set,
        });
        let root_cc_handle = CanonicalCollectionHandle(0);
        let root_cc = CanonicalCollection {
            item_set: root_item_set_rc.clone(),
            actions: BTreeMap::new(),
            eof_reduce: None,
            goto: BTreeMap::new(),
        };
        self.states.push(root_cc);
        self.cc_lut.insert(root_item_set_rc, root_cc_handle);
        self.worklist.push(root_cc_handle);
        root_cc_handle
    }

    fn process_worklist(&mut self) {
        while let Some(cc_handle) = self.worklist.pop() {
            let (next_symbols, lookaheads) = self.cc_next(cc_handle);
            for symbol in next_symbols {
                if let Some(next_item_set_rc) = self.goto(cc_handle, symbol) {
                    let next_cc_handle = if let Some(handle) = self.cc_lut.get(&next_item_set_rc) {
                        *handle
                    } else {
                        let handle = CanonicalCollectionHandle(self.states.len());
                        let cc = CanonicalCollection {
                            item_set: next_item_set_rc.clone(),
                            actions: BTreeMap::new(),
                            eof_reduce: None,
                            goto: BTreeMap::new(),
                        };
                        self.states.push(cc);
                        self.cc_lut.insert(next_item_set_rc.clone(), handle);
                        self.worklist.push(handle);
                        handle
                    };
                    let cc = &mut self.states[cc_handle.idx()];
                    match symbol {
                        SymbolHandle::Terminal(t_handle) => {
                            let cc_action = CanonicalCollectionAction::Shift(next_cc_handle);
                            if let Some(old) = cc.actions.insert(t_handle, cc_action.clone()) {
                                panic!(
                                    "Conflict detected in LR table at state {:?} for lookahead {:?}: existing action {:?}, new action {:?}",
                                    cc_handle, t_handle, old, cc_action
                                );
                            }
                        }
                        SymbolHandle::NonTerminal(nt_handle) => {
                            if let Some(old) = cc.goto.insert(nt_handle, next_cc_handle) {
                                panic!(
                                    "Conflict detected in LR table GOTO at state {:?} for non-terminal {:?}: existing goto to {:?}, new goto to {:?}",
                                    cc_handle, nt_handle, old, next_cc_handle
                                );
                            }
                        }
                    }
                }
            }
            for lookahead in lookaheads {
                if let Some(production_handle) = self.reduce(cc_handle, lookahead) {
                    let cc = &mut self.states[cc_handle.idx()];
                    let cc_action = CanonicalCollectionAction::Reduce(production_handle);
                    match lookahead {
                        Some(lookahead) => {
                            if let Some(old) = cc.actions.insert(lookahead, cc_action.clone()) {
                                panic!(
                                    "Conflict detected in LR table at state {:?} for lookahead {:?}: existing action {:?}, new action {:?}",
                                    cc_handle, lookahead, old, cc_action
                                );
                            }
                        }
                        None => {
                            if let Some(old) = cc.eof_reduce.replace(production_handle) {
                                panic!(
                                    "Conflict detected in LR table at state {:?} for EOF lookahead: existing reduce by {:?}, new reduce by {:?}",
                                    cc_handle, old, production_handle
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    fn cc_next(
        &self,
        cc: CanonicalCollectionHandle,
    ) -> (HashSet<SymbolHandle>, HashSet<Option<TerminalHandle>>) {
        let mut symbols = HashSet::new();
        let mut lookaheads = HashSet::new();
        let cc = &self.states[cc.idx()];
        for item in &cc.item_set.items {
            if let Some(symbol) = self
                .cfg
                .production(item.production)
                .symbols
                .get(item.dot_position)
            {
                symbols.insert(*symbol);
            } else {
                lookaheads.insert(item.lookahead);
            }
        }
        (symbols, lookaheads)
    }

    fn goto(
        &self,
        cc: CanonicalCollectionHandle,
        symbol: SymbolHandle,
    ) -> Option<Rc<CanonicalItemSet>> {
        let cc = &self.states[cc.idx()];
        // Collect kernel items (items where we advance the dot past the given symbol)
        let kernel_items: Vec<_> = cc
            .item_set
            .items
            .iter()
            .filter_map(|item| {
                self.cfg
                    .production(item.production)
                    .symbols
                    .get(item.dot_position)
                    .and_then(|sym| {
                        if *sym == symbol {
                            Some(CanonicalItem::new(
                                item.production,
                                item.lookahead,
                                item.dot_position + 1,
                            ))
                        } else {
                            None
                        }
                    })
            })
            .collect();
        if kernel_items.is_empty() {
            None
        } else {
            // Compute closure over the kernel items
            let item_set: BTreeSet<_> = kernel_items
                .into_iter()
                .flat_map(|item| self.closure(item))
                .collect();
            Some(Rc::new(CanonicalItemSet { items: item_set }))
        }
    }

    fn reduce(
        &self,
        cc: CanonicalCollectionHandle,
        lookahead: Option<TerminalHandle>,
    ) -> Option<ProductionHandle> {
        let cc = &self.states[cc.idx()];
        for item in &cc.item_set.items {
            if item.dot_position == self.cfg.production(item.production).symbols.len() {
                if item.lookahead == lookahead {
                    return Some(item.production);
                }
            }
        }
        None
    }
}
