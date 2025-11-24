use std::collections::{BTreeMap, HashMap, HashSet};
use std::hash::Hash;
use std::rc::Rc;
use std::{collections::BTreeSet, fmt::Debug};

use crate::symbol::{
    CFGFirstSet, ContextFreeGrammar, NonTerminalHandle, ProductionHandle, SymbolHandle,
    TerminalHandle, TokenType,
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
    pub actions: BTreeMap<Option<TerminalHandle>, CanonicalCollectionAction>,
    pub goto: BTreeMap<NonTerminalHandle, CanonicalCollectionHandle>,
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

struct LRTableBuilder<'a, TToken>
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
                            if let Some(old) = cc.actions.insert(Some(t_handle), cc_action.clone())
                            {
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
                    if let Some(old) = cc.actions.insert(lookahead, cc_action.clone()) {
                        panic!(
                            "Conflict detected in LR table at state {:?} for lookahead {:?}: existing action {:?}, new action {:?}",
                            cc_handle, lookahead, old, cc_action
                        );
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
        let item_set: BTreeSet<_> = cc
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
        if item_set.is_empty() {
            None
        } else {
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
                if let Some(la) = item.lookahead {
                    if Some(la) == lookahead {
                        return Some(item.production);
                    }
                }
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
pub enum TokenTree<'a, Token>
where
    Token: Debug + Clone + Eq + PartialEq + Hash,
{
    Leaf(&'a Token),
    Node {
        non_terminal_name: String,
        children: Vec<TokenTree<'a, Token>>,
    },
}

pub struct PlainLRTableParser<'a, TToken>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
{
    pub lr_table: LRTable,
    pub cfg: &'a ContextFreeGrammar<TToken>,
    pub token_lut: HashMap<TToken, TerminalHandle>,
}

impl<'a, TToken> PlainLRTableParser<'a, TToken>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
{
    pub fn new(cfg: &'a ContextFreeGrammar<TToken>) -> Self {
        let builder = LRTableBuilder::new(cfg);
        let lr_table = builder.build();
        let token_lut = cfg
            .iter_terminals()
            .map(|(handle, token)| (token.clone(), handle))
            .collect();
        Self {
            lr_table,
            cfg,
            token_lut,
        }
    }

    pub fn parse<'t, T: TokenType<TToken = TToken> + Debug + Clone + Eq + Hash>(&self, tokens: &'t [T]) -> TokenTree<'t, T> {
        let cursor = TokenCursor::new(tokens, self);
        let parse_tree = cursor.parse();
        parse_tree
    }
}

struct TokenCursor<'p, 't, TToken, Token>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
    Token: TokenType<TToken = TToken> + Debug + Clone + Eq + Hash,
{
    pub tokens: &'t [Token],
    pub position: usize,
    parser: &'p PlainLRTableParser<'p, TToken>,
    start_state: CanonicalCollectionHandle,
    stated_symbol_stack: Vec<(CanonicalCollectionHandle, TokenTree<'t, Token>)>,
}

impl<'p, 't, TToken, Token> TokenCursor<'p, 't, TToken, Token>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
    Token: TokenType<TToken = TToken> + Debug + Clone + Eq + Hash,
{
    pub fn new(tokens: &'t [Token], parser: &'p PlainLRTableParser<'p, TToken>) -> Self {
        let start_state = parser.lr_table.start_state;
        Self {
            tokens,
            position: 0,
            parser,
            start_state,
            stated_symbol_stack: Vec::new(),
        }
    }

    fn top_state(&self) -> CanonicalCollectionHandle {
        self.stated_symbol_stack
            .last()
            .map(|(state, _)| *state)
            .unwrap_or(self.start_state)
    }

    fn consume(&mut self) -> Option<&'t Token> {
        if self.position < self.tokens.len() {
            let token = &self.tokens[self.position];
            self.position += 1;
            Some(token)
        } else {
            None
        }
    }

    fn peek(&self) -> Option<&'t Token> {
        self.tokens.get(self.position)
    }

    fn reduce(&mut self, production_handle: ProductionHandle) {
        let production = self.parser.cfg.production(production_handle);
        let mut children = Vec::new();
        for _ in 0..production.symbols.len() {
            let (_, child) = self
                .stated_symbol_stack
                .pop()
                .expect("Stack underflow during reduce");
            children.push(child);
        }
        children.reverse();
        let non_terminal = self
            .parser
            .cfg
            .non_terminal(production_handle.non_terminal_handle());
        let non_terminal_tree = TokenTree::Node {
            non_terminal_name: non_terminal.name.clone(),
            children,
        };
        let top_state = self.top_state();
        let state = self.parser.lr_table.state(top_state);
        match state.goto.get(&production_handle.non_terminal_handle()) {
            Some(target) => {
                self.stated_symbol_stack.push((*target, non_terminal_tree));
            }
            None => panic!(
                "No GOTO entry for non-terminal {:?} in state {:?}",
                production_handle.non_terminal_handle(),
                top_state
            ),
        }
    }

    pub fn parse(mut self) -> TokenTree<'t, Token> {
        loop {
            let state = self.parser.lr_table.state(self.top_state());
            let lookahead = self.peek();
            let terminal_handle = lookahead.map(|token| {
                self.parser
                    .token_lut
                    .get(&token.token_type())
                    .expect(&format!(
                        "Token type {:?} not found in terminal lookup",
                        token.token_type()
                    ))
                    .clone()
            });
            match state.actions.get(&terminal_handle) {
                Some(CanonicalCollectionAction::Shift(target)) => {
                    let token = self
                        .consume()
                        .expect("Token should be available for shift action");
                    self.stated_symbol_stack
                        .push((*target, TokenTree::Leaf(token)));
                }
                Some(CanonicalCollectionAction::Reduce(production_handle)) => {
                    self.reduce(*production_handle);

                    // Check for accept condition:
                    // - We've reduced to the start symbol
                    // - No more input tokens
                    // - Stack has exactly one item (the start symbol)
                    if production_handle.non_terminal_handle() == self.parser.cfg.root
                        && self.peek().is_none()
                        && self.stated_symbol_stack.len() == 1
                    {
                        // Accept - return the final parse tree
                        return self
                            .stated_symbol_stack
                            .pop()
                            .expect("Stack should have the final parse tree")
                            .1;
                    }
                }
                None => panic!(
                    "No action for lookahead {:?} in state {:?}",
                    terminal_handle,
                    self.top_state()
                ),
            }
        }
    }
}
