mod plain;

pub use plain::PlainLexer;

use crate::token::Token;
use std::{fmt::Debug, hash::Hash};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct UmatchedError {
    pub content: String,
    pub start_index: usize,
    pub end_index: usize,
}

pub trait Lexer: Debug + Clone + Eq + PartialEq + Hash {
    fn tokenize<I>(chars: I) -> impl Iterator<Item = Result<Token<Self>, UmatchedError>>
    where
        I: Iterator<Item = char>;
}
