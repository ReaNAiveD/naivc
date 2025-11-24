use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;

#[cfg(feature = "lexer")]
use naivc_lexer::token::Token;

pub trait TokenType {
    type TToken;

    fn token_type(&self) -> Self::TToken;
}

#[cfg(feature = "lexer")]
impl<TToken> TokenType for Token<TToken>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
{
    type TToken = TToken;

    fn token_type(&self) -> Self::TToken {
        self.kind.clone()
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum SymbolHandle {
    Terminal(TerminalHandle),
    NonTerminal(NonTerminalHandle),
}

impl PartialOrd for SymbolHandle {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SymbolHandle {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (SymbolHandle::Terminal(a), SymbolHandle::Terminal(b)) => a.cmp(b),
            (SymbolHandle::NonTerminal(a), SymbolHandle::NonTerminal(b)) => a.cmp(b),
            (SymbolHandle::Terminal(_), SymbolHandle::NonTerminal(_)) => std::cmp::Ordering::Less,
            (SymbolHandle::NonTerminal(_), SymbolHandle::Terminal(_)) => {
                std::cmp::Ordering::Greater
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct TerminalHandle(usize);

impl TerminalHandle {
    pub fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn idx(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct NonTerminalHandle(usize);

impl NonTerminalHandle {
    pub fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn idx(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct NonTerminal {
    pub name: String,
    pub productions: Vec<Production>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Symbol<'a, TToken>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
{
    Terminal(&'a TToken),
    NonTerminal(&'a NonTerminal),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct ProductionHandle {
    non_terminal_handle: NonTerminalHandle,
    production_idx: usize,
}

impl ProductionHandle {
    pub fn new(symbol_handle: NonTerminalHandle, production_idx: usize) -> Self {
        Self {
            non_terminal_handle: symbol_handle,
            production_idx,
        }
    }

    pub fn symbol_handle(&self) -> SymbolHandle {
        SymbolHandle::NonTerminal(self.non_terminal_handle)
    }

    pub fn non_terminal_handle(&self) -> NonTerminalHandle {
        self.non_terminal_handle
    }

    pub fn production_idx(&self) -> usize {
        self.production_idx
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Production {
    pub symbols: Vec<SymbolHandle>,
}

#[derive(Debug, Clone)]
pub struct ContextFreeGrammar<TToken>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
{
    pub root: NonTerminalHandle,
    pub terminals: Vec<TToken>,
    pub non_terminals: Vec<NonTerminal>,
}

impl<TToken> ContextFreeGrammar<TToken>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
{
    pub fn new(
        root: NonTerminalHandle,
        terminals: Vec<TToken>,
        non_terminals: Vec<NonTerminal>,
    ) -> Self {
        Self {
            root,
            terminals,
            non_terminals,
        }
    }

    pub fn symbol(&'_ self, handle: SymbolHandle) -> Symbol<'_, TToken> {
        match handle {
            SymbolHandle::Terminal(t_handle) => {
                let token = &self.terminals[t_handle.idx()];
                Symbol::Terminal(token)
            }
            SymbolHandle::NonTerminal(nt_handle) => {
                let non_terminal = &self.non_terminals[nt_handle.idx()];
                Symbol::NonTerminal(non_terminal)
            }
        }
    }

    pub fn iter_symbols(&'_ self) -> impl Iterator<Item = (SymbolHandle, Symbol<'_, TToken>)> {
        let terminal_iter = self
            .iter_terminals()
            .map(|(t_handle, token)| (SymbolHandle::Terminal(t_handle), Symbol::Terminal(token)));
        let non_terminal_iter = self.iter_non_terminals().map(|(nt_handle, non_terminal)| {
            (
                SymbolHandle::NonTerminal(nt_handle),
                Symbol::NonTerminal(non_terminal),
            )
        });
        terminal_iter.chain(non_terminal_iter)
    }

    pub fn terminal(&self, handle: TerminalHandle) -> &TToken {
        &self.terminals[handle.idx()]
    }

    pub fn iter_terminals(&self) -> impl Iterator<Item = (TerminalHandle, &TToken)> {
        self.terminals
            .iter()
            .enumerate()
            .map(|(idx, token)| (TerminalHandle::new(idx), token))
    }

    pub fn non_terminal(&self, handle: NonTerminalHandle) -> &NonTerminal {
        &self.non_terminals[handle.idx()]
    }

    pub fn iter_non_terminals(&self) -> impl Iterator<Item = (NonTerminalHandle, &NonTerminal)> {
        self.non_terminals
            .iter()
            .enumerate()
            .map(|(idx, non_terminal)| (NonTerminalHandle::new(idx), non_terminal))
    }

    pub fn production(&self, handle: ProductionHandle) -> &Production {
        &self.non_terminals[handle.non_terminal_handle.idx()].productions[handle.production_idx]
    }

    pub fn iter_productions(
        &self,
        symbol: NonTerminalHandle,
    ) -> impl Iterator<Item = (ProductionHandle, &Production)> {
        self.non_terminals[symbol.idx()]
            .productions
            .iter()
            .enumerate()
            .map(move |(idx, production)| (ProductionHandle::new(symbol, idx), production))
    }

    pub fn first_set(&self) -> CFGFirstSet {
        let mut first_set = CFGFirstSet::empty(self.non_terminals.len());
        loop {
            let mut updated = false;
            for (handle, non_terminal) in self.iter_non_terminals() {
                let children_firsts: HashSet<_> = non_terminal
                    .productions
                    .iter()
                    .filter_map(|production| {
                        production
                            .symbols
                            .first()
                            .and_then(|symbol_handle| match symbol_handle {
                                SymbolHandle::Terminal(t_handle) => {
                                    Some(HashSet::from([*t_handle]))
                                }
                                SymbolHandle::NonTerminal(nt_handle) => {
                                    first_set.get_first_set(nt_handle).cloned()
                                }
                            })
                    })
                    .flatten()
                    .collect();
                updated = first_set.union(&handle, &children_firsts) || updated;
            }
            if !updated {
                // No more updates can be made
                break;
            }
        }
        first_set
    }
}

pub struct CFGFirstSet {
    pub firsts: Vec<HashSet<TerminalHandle>>,
}

impl CFGFirstSet {
    pub fn empty(symbol_size: usize) -> Self {
        Self {
            firsts: vec![HashSet::new(); symbol_size],
        }
    }

    pub fn get_first_set(
        &self,
        symbol_handle: &NonTerminalHandle,
    ) -> Option<&HashSet<TerminalHandle>> {
        self.firsts.get(symbol_handle.idx())
    }

    pub fn insert(&mut self, symbol_handle: &NonTerminalHandle, token: TerminalHandle) -> bool {
        self.firsts
            .get_mut(symbol_handle.idx())
            .expect("Invalid symbol handle")
            .insert(token)
    }

    pub fn union(
        &mut self,
        symbol_handle: &NonTerminalHandle,
        other_set: &HashSet<TerminalHandle>,
    ) -> bool {
        let first_set = self
            .firsts
            .get_mut(symbol_handle.idx())
            .expect("Invalid symbol handle");
        let initial_len = first_set.len();
        first_set.extend(other_set.iter().cloned());
        first_set.len() > initial_len
    }
}
