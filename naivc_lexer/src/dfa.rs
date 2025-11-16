mod range_partition;
mod rdfa;
mod rrdfa;
pub mod transltion;

use std::{char, collections::BTreeSet, rc::Rc};

pub use range_partition::{RangePartitioner, SortedRangePartitioner};
pub use transltion::LabelledTransition;

use crate::{dfa::rdfa::ReversedDeterministicFiniteAutomaton, token::TokenType};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct PriorizedTokenType {
    pub token_type: Rc<TokenType>,
    /// Lower values indicate higher priority
    pub priority: usize,
}

impl Ord for PriorizedTokenType {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.priority.cmp(&other.priority) {
            std::cmp::Ordering::Equal => match self.token_type.name.cmp(&other.token_type.name) {
                std::cmp::Ordering::Equal => self.token_type.regex.cmp(&other.token_type.regex),
                ord => ord,
            },
            ord => ord,
        }
    }
}

impl PartialOrd for PriorizedTokenType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct DFAStateHandle(usize);

impl DFAStateHandle {
    pub fn new(index: usize) -> Self {
        Self(index)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct DFAState {
    pub transitions: BTreeSet<LabelledTransition<DFAStateHandle>>,
    pub bind_patterns: BTreeSet<PriorizedTokenType>,
    pub accept_patterns: BTreeSet<PriorizedTokenType>,
    pub accept_pattern: Option<Rc<TokenType>>,
}

impl DFAState {
    pub fn next_state(&self, input: char) -> Option<DFAStateHandle> {
        for transition in &self.transitions {
            if transition.range.contains(&input) {
                return Some(transition.target);
            } else if *transition.range.start() > input {
                break;
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
pub struct DFAWalkState {
    pub current_state: DFAStateHandle,
    pub accepted_pattern: Option<Rc<TokenType>>,
}

#[derive(Debug, Clone)]
pub struct DeterministicFiniteAutomaton {
    dfa_states: Vec<DFAState>,
    start_state: DFAStateHandle,
}

impl DeterministicFiniteAutomaton {
    pub fn from_lexer_spec(lexer_spec: &[Rc<TokenType>]) -> Self {
        let priorized_token_types: Vec<PriorizedTokenType> = lexer_spec
            .iter()
            .enumerate()
            .map(|(priority, token_type)| PriorizedTokenType {
                token_type: Rc::clone(token_type),
                priority,
            })
            .collect();
        let rdfa = ReversedDeterministicFiniteAutomaton::from_prioritized_token_types(
            priorized_token_types.into_iter(),
        );
        let rrdfa = rdfa.reverse();
        rrdfa.subset_reachable()
    }

    pub fn start_state(&self) -> DFAStateHandle {
        self.start_state
    }

    pub fn next_state(&self, current_state: DFAStateHandle, input: char) -> Option<DFAWalkState> {
        let next_state = self.dfa_states[current_state.index()].next_state(input);
        next_state.map(|state_handle| {
            let accept_pattern = self.dfa_states[state_handle.index()]
                .accept_pattern
                .as_ref()
                .map(Rc::clone);
            DFAWalkState {
                current_state: state_handle,
                accepted_pattern: accept_pattern,
            }
        })
    }

    pub fn start_with(&self, char: char) -> Option<DFAWalkState> {
        self.next_state(self.start_state, char)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;

    fn build_dfa(specs: &[(&str, &str)]) -> (DeterministicFiniteAutomaton, Vec<Rc<TokenType>>) {
        let tokens: Vec<Rc<TokenType>> = specs
            .iter()
            .map(|(name, regex)| Rc::new(TokenType::new((*name).into(), (*regex).into())))
            .collect();
        let dfa = DeterministicFiniteAutomaton::from_lexer_spec(&tokens);
        println!("Constructed DFA: {:#?}", dfa);
        (dfa, tokens)
    }

    fn run_dfa(dfa: &DeterministicFiniteAutomaton, input: &str) -> Option<Rc<TokenType>> {
        let mut walk_state: Option<DFAWalkState> = None;
        for (idx, ch) in input.chars().enumerate() {
            let next_state = if idx == 0 {
                dfa.start_with(ch)?
            } else {
                let current_handle = walk_state.as_ref().unwrap().current_state;
                dfa.next_state(current_handle, ch)?
            };
            walk_state = Some(next_state);
        }
        walk_state.and_then(|state| state.accepted_pattern)
    }

    #[test]
    fn literal_token_accepts_exact_char() {
        let (dfa, tokens) = build_dfa(&[("LETTER_A", "a")]);

        let accepted = run_dfa(&dfa, "a").expect("literal token should accept 'a'");
        assert!(Rc::ptr_eq(&accepted, &tokens[0]));

        assert!(
            run_dfa(&dfa, "b").is_none(),
            "unrelated char must be rejected"
        );
        assert!(
            run_dfa(&dfa, "aa").is_none(),
            "longer input than literal should fail"
        );
    }

    #[test]
    fn overlapping_tokens_use_priority() {
        let (dfa, tokens) = build_dfa(&[("IF", "if"), ("IDENT", "[a-z]+")]);

        let mut walk = dfa.start_with('i').expect("first char should transition");
        let identifier = walk
            .accepted_pattern
            .expect("[a-z]+ should accept single letter");
        assert!(Rc::ptr_eq(&identifier, &tokens[1]));

        walk = dfa
            .next_state(walk.current_state, 'f')
            .expect("second char should continue");
        let keyword = walk
            .accepted_pattern
            .expect("'if' should produce a keyword match");
        assert!(Rc::ptr_eq(&keyword, &tokens[0]));
    }

    #[test]
    fn supports_char_classes_and_quantifiers() {
        let (dfa, tokens) = build_dfa(&[("HEX_PREFIX", "0x"), ("NUMBER", "\\d+")]);

        let number = run_dfa(&dfa, "12345").expect("\\d+ should accept digits");
        assert!(Rc::ptr_eq(&number, &tokens[1]));

        let hex = run_dfa(&dfa, "0x").expect("literal 0x should be accepted");
        assert!(Rc::ptr_eq(&hex, &tokens[0]));

        assert!(
            run_dfa(&dfa, "0z").is_none(),
            "invalid suffix should be rejected"
        );
    }

    #[test]
    fn unicode_literals_and_ranges_are_supported() {
        let (dfa, tokens) = build_dfa(&[
            ("EM_DASH", "—"),
            ("GREEK_WORD", "[α-ω]+"),
            ("EMOJI_SEQUENCE", "😀+"),
        ]);

        let dash = run_dfa(&dfa, "—").expect("em dash literal should be accepted");
        assert!(Rc::ptr_eq(&dash, &tokens[0]));

        let greek = run_dfa(&dfa, "αβγ").expect("[α-ω]+ should accept greek letters");
        assert!(Rc::ptr_eq(&greek, &tokens[1]));

        let emoji = run_dfa(&dfa, "😀😀").expect("😀+ should accept repeated emoji");
        assert!(Rc::ptr_eq(&emoji, &tokens[2]));

        assert!(
            run_dfa(&dfa, " ").is_none(),
            "whitespace should be rejected"
        );
    }

    #[test]
    fn unicode_tokens_respect_priority() {
        let (dfa, tokens) = build_dfa(&[("GREEK_KEYWORD", "αβ"), ("GREEK_IDENT", "[α-ω]+")]);

        let keyword = run_dfa(&dfa, "αβ").expect("exact keyword should match");
        assert!(Rc::ptr_eq(&keyword, &tokens[0]));

        let single_letter = run_dfa(&dfa, "α").expect("single greek letter should be an ident");
        assert!(Rc::ptr_eq(&single_letter, &tokens[1]));

        let longer_word = run_dfa(&dfa, "αβγ").expect("longer sequence should keep identifier");
        assert!(Rc::ptr_eq(&longer_word, &tokens[1]));
    }
}
