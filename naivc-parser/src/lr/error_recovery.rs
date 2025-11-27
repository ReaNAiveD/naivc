use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;

use crate::symbol::{TerminalHandle, TokenType};

use super::parser::{LRParseError, TokenCursor, TokenTree};
use super::table::CanonicalCollectionAction;

pub trait ErrorRecoveryStrategy {
    fn recover<'p, 't, TToken, Token>(
        &self,
        cursor: &mut TokenCursor<'p, 't, TToken, Token>,
    ) -> Option<LRParseError<'t, Token>>
    where
        TToken: Debug + Clone + Eq + PartialEq + Hash,
        Token: TokenType<TToken = TToken> + Debug + Clone + Eq + Hash;
}

pub struct DeleteRecovery;

impl ErrorRecoveryStrategy for DeleteRecovery {
    fn recover<'p, 't, TToken, Token>(
        &self,
        cursor: &mut TokenCursor<'p, 't, TToken, Token>,
    ) -> Option<LRParseError<'t, Token>>
    where
        TToken: Debug + Clone + Eq + PartialEq + Hash,
        Token: TokenType<TToken = TToken> + Debug + Clone + Eq + Hash,
    {
        if let Ok(followed_token) = cursor.peek_with_offset(1) {
            let top_state = cursor.top_state();
            let state = cursor.parser.lr_table.state(top_state);
            let terminal_handle = cursor.token_handle(followed_token);
            if state.actions.contains_key(&terminal_handle) {
                // Deleting the current token allows parsing to continue
                let error_position = cursor.position;
                let error_token = cursor.consume();
                return Some(LRParseError {
                    token: error_token,
                    position: error_position,
                    potential_tokens: Vec::new(),
                    skipped: error_token.into_iter().collect(),
                    message: format!("Unexpected token",),
                });
            }
        }
        None
    }
}

pub struct InsertRecovery {
    pub insertable_tokens: Vec<TerminalHandle>,
}

impl ErrorRecoveryStrategy for InsertRecovery {
    fn recover<'p, 't, TToken, Token>(
        &self,
        cursor: &mut TokenCursor<'p, 't, TToken, Token>,
    ) -> Option<LRParseError<'t, Token>>
    where
        TToken: Debug + Clone + Eq + PartialEq + Hash,
        Token: TokenType<TToken = TToken> + Debug + Clone + Eq + Hash,
    {
        let top_state = cursor.top_state();
        let state = cursor.parser.lr_table.state(top_state);
        let mut potential_tokens = HashSet::new();
        for token_handle in &self.insertable_tokens {
            if state.actions.contains_key(&token_handle) {
                potential_tokens.insert(*token_handle);
            }
        }
        if let Some(token_handle) = potential_tokens.iter().next() {
            loop {
                let cc_action = state
                    .actions
                    .get(token_handle)
                    .expect("Checked action should exist");
                match cc_action {
                    CanonicalCollectionAction::Shift(target) => {
                        cursor.stated_symbol_stack.push((
                            *target,
                            TokenTree::Error {
                                found: cursor.peek(),
                                potential_tokens: potential_tokens.iter().cloned().collect(),
                                skipped: Vec::new(),
                            },
                        ));
                        return Some(LRParseError {
                            token: cursor.peek(),
                            position: cursor.position,
                            potential_tokens: potential_tokens.iter().cloned().collect(),
                            skipped: Vec::new(),
                            message: format!("Inserted missing token {:?}", token_handle),
                        });
                    }
                    CanonicalCollectionAction::Reduce(production) => {
                        let non_terminal_tree = cursor.reduce(*production);
                        cursor.goto(production.non_terminal_handle(), non_terminal_tree);
                    }
                }
            }
        }
        None
    }
}

pub struct PanicRecovery {
    pub max_panic_skip: Option<usize>,
    pub sync_tokens: HashSet<TerminalHandle>,
}

impl PanicRecovery {
    pub fn skippable(&self, offset: usize) -> bool {
        if let Some(max_skip) = self.max_panic_skip {
            offset < max_skip
        } else {
            true
        }
    }
}

impl ErrorRecoveryStrategy for PanicRecovery {
    fn recover<'p, 't, TToken, Token>(
        &self,
        cursor: &mut TokenCursor<'p, 't, TToken, Token>,
    ) -> Option<LRParseError<'t, Token>>
    where
        TToken: Debug + Clone + Eq + PartialEq + Hash,
        Token: TokenType<TToken = TToken> + Debug + Clone + Eq + Hash,
    {
        let mut skipped = vec![];
        while self.skippable(skipped.len()) {
            if let Some(token) = cursor.peek() {
                let token_handle = cursor.token_handle(token);
                if !self.sync_tokens.contains(&token_handle) {
                    skipped.push(token);
                    cursor.consume();
                }
            }
            break;
        }
        while let Some((state, _)) = cursor.stated_symbol_stack.last() {
            let token_handle = cursor.peek().map(|token| cursor.token_handle(token));
            let state = cursor.parser.lr_table.state(*state);
            match state.get_action(token_handle) {
                Some(_) => {
                    let skipped_num = skipped.len();
                    return Some(LRParseError {
                        token: cursor.peek(),
                        position: cursor.position,
                        potential_tokens: Vec::new(),
                        skipped,
                        message: format!("Panic recovery skipped {} tokens", skipped_num),
                    });
                }
                None => {
                    cursor.stated_symbol_stack.pop();
                }
            }
        }
        None
    }
}
