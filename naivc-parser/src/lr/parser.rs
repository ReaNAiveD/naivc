use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

use crate::symbol::{ContextFreeGrammar, ProductionHandle, TerminalHandle, TokenType};

use super::error_recovery::ErrorRecoveryStrategy;
use super::table::{
    CanonicalCollectionAction, CanonicalCollectionHandle, LRTable, LRTableBuilder,
};

#[derive(Debug, Clone)]
pub struct LRParseError<'t, Token>
where
    Token: Debug + Clone + Eq + PartialEq + Hash,
{
    /// The token that caused the error (if any)
    pub token: Option<&'t Token>,
    /// Position in the token stream
    pub position: usize,
    /// Potential tokens at this position (expected or insertable for recovery)
    pub potential_tokens: Vec<TerminalHandle>,
    /// Skipped tokens leading up to the error
    pub skipped: Vec<&'t Token>,
    /// Description of the error
    pub message: String,
}

/// A clean syntax tree produced by successful parsing (no error recovery).
/// Contains only valid syntactic structures.
#[derive(Debug, Clone)]
pub enum SyntaxTree<'a, Token>
where
    Token: Debug + Clone + Eq + PartialEq + Hash,
{
    Leaf(&'a Token),
    Node {
        production_handle: ProductionHandle,
        children: Vec<SyntaxTree<'a, Token>>,
    },
}

/// A recoverable tree that may contain error nodes from error recovery.
/// Used when parsing with error recovery enabled.
#[derive(Debug, Clone)]
pub enum RecoverableTree<'a, Token>
where
    Token: Debug + Clone + Eq + PartialEq + Hash,
{
    Leaf(&'a Token),
    Node {
        production_handle: ProductionHandle,
        children: Vec<RecoverableTree<'a, Token>>,
    },
    Error {
        found: Option<&'a Token>,
        potential_tokens: Vec<TerminalHandle>,
        skipped: Vec<&'a Token>,
    },
}

pub struct PlainLRTableParser<TToken>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
{
    pub lr_table: LRTable,
    pub cfg: ContextFreeGrammar<TToken>,
    pub token_lut: HashMap<TToken, TerminalHandle>,
}

impl<TToken> PlainLRTableParser<TToken>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
{
    pub fn new(cfg: ContextFreeGrammar<TToken>) -> Self {
        let builder = LRTableBuilder::new(&cfg);
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

    pub fn parse<'t, T: TokenType<TToken = TToken> + Debug + Clone + Eq + Hash>(
        &self,
        tokens: &'t [T],
    ) -> SyntaxTree<'t, T> {
        let cursor = TokenCursor::new(tokens, self);
        let parse_tree = cursor.parse();
        parse_tree
    }

    pub fn parse_with_recovery<'t, T, S>(
        &self,
        tokens: &'t [T],
        recovery_strategy: &[S],
    ) -> (RecoverableTree<'t, T>, Vec<LRParseError<'t, T>>)
    where
        T: TokenType<TToken = TToken> + Debug + Clone + Eq + Hash,
        S: ErrorRecoveryStrategy,
    {
        let cursor = RecoverableTokenCursor::new(tokens, self);
        let parse_tree = cursor.parse_with_recovery(recovery_strategy);
        parse_tree
    }
}

pub struct TokenCursor<'p, 't, TToken, Token>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
    Token: TokenType<TToken = TToken> + Debug + Clone + Eq + Hash,
{
    pub tokens: &'t [Token],
    pub position: usize,
    pub parser: &'p PlainLRTableParser<TToken>,
    pub start_state: CanonicalCollectionHandle,
    pub stated_symbol_stack: Vec<(CanonicalCollectionHandle, SyntaxTree<'t, Token>)>,
}

impl<'p, 't, TToken, Token> TokenCursor<'p, 't, TToken, Token>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
    Token: TokenType<TToken = TToken> + Debug + Clone + Eq + Hash,
{
    pub fn new(tokens: &'t [Token], parser: &'p PlainLRTableParser<TToken>) -> Self {
        let start_state = parser.lr_table.start_state;
        Self {
            tokens,
            position: 0,
            parser,
            start_state,
            stated_symbol_stack: Vec::new(),
        }
    }

    pub fn top_state(&self) -> CanonicalCollectionHandle {
        self.stated_symbol_stack
            .last()
            .map(|(state, _)| *state)
            .unwrap_or(self.start_state)
    }

    pub fn consume(&mut self) -> Option<&'t Token> {
        if self.position < self.tokens.len() {
            let token = &self.tokens[self.position];
            self.position += 1;
            Some(token)
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<&'t Token> {
        self.tokens.get(self.position)
    }

    /// Peek with an offset from the current position
    ///
    /// Returns Ok(&Token) if within bounds, Err(usize) with the out-of-bounds index otherwise
    /// That is, Err(0) means the offseted position is exactly at the end of the token stream
    pub fn peek_with_offset(&self, offset: usize) -> Result<&'t Token, usize> {
        self.tokens
            .get(self.position + offset)
            .ok_or(self.position + offset - self.tokens.len())
    }

    fn shift(&mut self, target_state: CanonicalCollectionHandle) {
        let token = self
            .consume()
            .expect("Token should be available for shift action");
        self.stated_symbol_stack
            .push((target_state, SyntaxTree::Leaf(token)));
    }

    /// Reduce using the given production handle
    ///
    /// Returns Some(SyntaxTree) only if we have completed parsing (i.e., reduced to the start symbol with no more input)
    pub fn reduce(&mut self, production_handle: ProductionHandle) -> SyntaxTree<'t, Token> {
        let production = self.parser.cfg.production(production_handle);
        let mut children = Vec::new();

        for _ in 0..production.symbols.len() {
            let (_, node) = self
                .stated_symbol_stack
                .pop()
                .expect("Stack underflow during reduce");
            children.push(node);
        }
        children.reverse();
        SyntaxTree::Node {
            production_handle,
            children,
        }
    }

    pub fn goto(
        &mut self,
        non_terminal_handle: crate::symbol::NonTerminalHandle,
        non_terminal_tree: SyntaxTree<'t, Token>,
    ) {
        let top_state = self.top_state();
        let state = self.parser.lr_table.state(top_state);
        match state.goto.get(&non_terminal_handle) {
            Some(target) => {
                self.stated_symbol_stack.push((*target, non_terminal_tree));
            }
            None => {
                panic!(
                    "No GOTO entry for non-terminal {:?} in state {:?}",
                    non_terminal_handle, top_state
                )
            }
        }
    }

    fn accepted(&self, production_handle: ProductionHandle) -> bool {
        production_handle.non_terminal_handle() == self.parser.cfg.root
            && self.peek().is_none()
            && self.stated_symbol_stack.is_empty()
    }

    pub fn parse(mut self) -> SyntaxTree<'t, Token> {
        loop {
            let state = self.parser.lr_table.state(self.top_state());
            let lookahead = self.peek();
            let terminal_handle = lookahead.map(|token| self.token_handle(token));
            match state.get_action(terminal_handle) {
                Some(CanonicalCollectionAction::Shift(target)) => {
                    self.shift(target);
                }
                Some(CanonicalCollectionAction::Reduce(production_handle)) => {
                    let non_terminal_tree = self.reduce(production_handle);

                    if self.accepted(production_handle) {
                        return non_terminal_tree;
                    }
                    self.goto(production_handle.non_terminal_handle(), non_terminal_tree);
                }
                None => panic!(
                    "No action for lookahead {:?} in state {:?}",
                    terminal_handle,
                    self.top_state()
                ),
            }
        }
    }

    pub fn token_handle(&self, token: &Token) -> TerminalHandle {
        self.parser
            .token_lut
            .get(&token.token_type())
            .expect(&format!(
                "Token type {:?} not found in terminal lookup",
                token.token_type()
            ))
            .clone()
    }
}

/// A cursor for parsing with error recovery support.
/// Uses RecoverableTree which can contain error nodes.
pub struct RecoverableTokenCursor<'p, 't, TToken, Token>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
    Token: TokenType<TToken = TToken> + Debug + Clone + Eq + Hash,
{
    pub tokens: &'t [Token],
    pub position: usize,
    pub parser: &'p PlainLRTableParser<TToken>,
    pub start_state: CanonicalCollectionHandle,
    pub stated_symbol_stack: Vec<(CanonicalCollectionHandle, RecoverableTree<'t, Token>)>,
}

impl<'p, 't, TToken, Token> RecoverableTokenCursor<'p, 't, TToken, Token>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
    Token: TokenType<TToken = TToken> + Debug + Clone + Eq + Hash,
{
    pub fn new(tokens: &'t [Token], parser: &'p PlainLRTableParser<TToken>) -> Self {
        let start_state = parser.lr_table.start_state;
        Self {
            tokens,
            position: 0,
            parser,
            start_state,
            stated_symbol_stack: Vec::new(),
        }
    }

    pub fn top_state(&self) -> CanonicalCollectionHandle {
        self.stated_symbol_stack
            .last()
            .map(|(state, _)| *state)
            .unwrap_or(self.start_state)
    }

    pub fn consume(&mut self) -> Option<&'t Token> {
        if self.position < self.tokens.len() {
            let token = &self.tokens[self.position];
            self.position += 1;
            Some(token)
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<&'t Token> {
        self.tokens.get(self.position)
    }

    /// Peek with an offset from the current position
    ///
    /// Returns Ok(&Token) if within bounds, Err(usize) with the out-of-bounds index otherwise
    /// That is, Err(0) means the offseted position is exactly at the end of the token stream
    pub fn peek_with_offset(&self, offset: usize) -> Result<&'t Token, usize> {
        self.tokens
            .get(self.position + offset)
            .ok_or(self.position + offset - self.tokens.len())
    }

    fn shift(&mut self, target_state: CanonicalCollectionHandle) {
        let token = self
            .consume()
            .expect("Token should be available for shift action");
        self.stated_symbol_stack
            .push((target_state, RecoverableTree::Leaf(token)));
    }

    /// Reduce using the given production handle
    ///
    /// Returns Some(RecoverableTree) only if we have completed parsing (i.e., reduced to the start symbol with no more input)
    pub fn reduce(&mut self, production_handle: ProductionHandle) -> RecoverableTree<'t, Token> {
        let production = self.parser.cfg.production(production_handle);
        let mut children = Vec::new();

        let mut remaining = production.symbols.len();
        while remaining > 0
            || matches!(self.stated_symbol_stack.last(), Some((_, RecoverableTree::Error { potential_tokens, .. })) if potential_tokens.is_empty())
        {
            let (_, node) = self
                .stated_symbol_stack
                .pop()
                .expect("Stack underflow during reduce");
            let counts = !matches!(&node, RecoverableTree::Error { potential_tokens, .. } if potential_tokens.is_empty());
            if counts && remaining > 0 {
                remaining -= 1;
            }
            children.push(node);
        }
        children.reverse();
        RecoverableTree::Node {
            production_handle,
            children,
        }
    }

    pub fn goto(
        &mut self,
        non_terminal_handle: crate::symbol::NonTerminalHandle,
        non_terminal_tree: RecoverableTree<'t, Token>,
    ) {
        let top_state = self.top_state();
        let state = self.parser.lr_table.state(top_state);
        match state.goto.get(&non_terminal_handle) {
            Some(target) => {
                self.stated_symbol_stack.push((*target, non_terminal_tree));
            }
            None => {
                panic!(
                    "No GOTO entry for non-terminal {:?} in state {:?}",
                    non_terminal_handle, top_state
                )
            }
        }
    }

    fn accepted(&self, production_handle: ProductionHandle) -> bool {
        production_handle.non_terminal_handle() == self.parser.cfg.root
            && self.peek().is_none()
            && self.stated_symbol_stack.is_empty()
    }

    pub fn parse_with_recovery<S>(mut self, recovery_strategy: &[S]) -> (RecoverableTree<'t, Token>, Vec<LRParseError<'t, Token>>)
    where
        S: ErrorRecoveryStrategy,
    {
        let mut errors = vec![];
        loop {
            let state = self.parser.lr_table.state(self.top_state());
            let lookahead = self.peek();
            let terminal_handle = lookahead.map(|token| self.token_handle(token));
            match state.get_action(terminal_handle) {
                Some(CanonicalCollectionAction::Shift(target)) => {
                    self.shift(target);
                }
                Some(CanonicalCollectionAction::Reduce(production_handle)) => {
                    let non_terminal_tree = self.reduce(production_handle);

                    if self.accepted(production_handle) {
                        return (non_terminal_tree, errors);
                    }
                    self.goto(production_handle.non_terminal_handle(), non_terminal_tree);
                }
                None => {
                    let mut recovered = false;
                    for strategy in recovery_strategy {
                        if let Some(error) = strategy.recover(&mut self) {
                            errors.push(error);
                            recovered = true;
                            break;
                        }
                    }
                    if !recovered {
                        panic!(
                            "No action for lookahead {:?} in state {:?}, and no recovery possible",
                            terminal_handle,
                            self.top_state()
                        );
                    }
                }
            }
        }
    }

    pub fn token_handle(&self, token: &Token) -> TerminalHandle {
        self.parser
            .token_lut
            .get(&token.token_type())
            .expect(&format!(
                "Token type {:?} not found in terminal lookup",
                token.token_type()
            ))
            .clone()
    }
}
