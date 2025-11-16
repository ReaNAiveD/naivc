pub mod capture_group;
pub mod char_class;
pub mod quantifier;

use std::ops::ControlFlow;

pub use capture_group::CaptureGroupBuilder;
pub use char_class::CharClassBuilder;
pub use quantifier::QuantifierBuilder;

use crate::{nfa::NondeterministicFiniteAutomaton, regex::{RegexLexer, RegexTokenKind}};

pub struct RegexCompiler<'a> {
    regex: RegexLexer<'a>,
}

impl<'a> RegexCompiler<'a> {
    pub fn from_regex(regex: &'a str) -> Self {
        let lexer = RegexLexer::new(regex);
        Self { regex: lexer, }
    }

    pub fn new(regex: RegexLexer<'a>) -> Self {
        Self { regex, }
    }

    pub fn compile(mut self) -> NondeterministicFiniteAutomaton {
        let mut is_first = true;
        let mut last_token_was_anchor = false;
        let mut builder = CaptureGroupBuilder::new(true);
        while let Some(token) = self.regex.next() {
            last_token_was_anchor = false;
            match token.kind {
                RegexTokenKind::Caret if is_first => {
                    panic!("Anchors not supported yet")
                },
                RegexTokenKind::DollarAnchor => {
                    last_token_was_anchor = true;
                },
                _ => {}
            }
            match builder.process_token(&token) {
                ControlFlow::Break(_) => {
                    panic!("Root capture group completed unexpectedly");
                }
                ControlFlow::Continue(_) => {
                    builder = builder;
                }
            };
            is_first = false;
        }
        if last_token_was_anchor {
            panic!("Anchors not supported yet")
        }
        builder.handle_end_of_regex()
    }
}