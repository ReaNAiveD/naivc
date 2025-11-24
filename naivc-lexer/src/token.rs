use std::{fmt::Debug, hash::Hash};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenType {
    pub name: String,
    pub regex: String,
}

impl TokenType {
    pub fn new(name: String, regex: String) -> Self {
        Self { name, regex }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Token<TToken>
where
    TToken: Debug + Clone + Eq + PartialEq + Hash,
{
    pub kind: TToken,
    pub lexeme: String,
    pub start_index: usize,
    pub end_index: usize,
}
