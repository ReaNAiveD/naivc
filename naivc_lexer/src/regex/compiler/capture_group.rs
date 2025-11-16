use std::ops::ControlFlow;

use crate::{
    nfa::NondeterministicFiniteAutomaton,
    regex::{
        RangeSet, RegexToken, RegexTokenKind, UnicodePropertyType,
        compiler::{CharClassBuilder, QuantifierBuilder},
    },
};

#[derive(Debug, Clone)]
pub enum NestedBuilder {
    CaptureGroup(Box<CaptureGroupBuilder>),
    CharClass(CharClassBuilder),
    Quantifier(QuantifierBuilder),
}

#[derive(Debug, Clone)]
pub struct CaptureGroupBuilder {
    processed_alternatives: Vec<NondeterministicFiniteAutomaton>,
    current_sequence: Option<NondeterministicFiniteAutomaton>,
    last_factor: Option<NondeterministicFiniteAutomaton>,
    nested_builder: Option<NestedBuilder>,
    is_root: bool,
}

impl CaptureGroupBuilder {
    pub fn new(is_root: bool) -> Self {
        Self {
            processed_alternatives: Vec::new(),
            current_sequence: None,
            last_factor: None,
            nested_builder: None,
            is_root,
        }
    }

    pub fn process_token(
        &mut self,
        token: &RegexToken<RegexTokenKind>,
    ) -> ControlFlow<NondeterministicFiniteAutomaton, &mut Self> {
        // First, check if we have a nested builder that should handle this token
        if let Some(nested) = self.nested_builder.take() {
            match nested {
                NestedBuilder::CaptureGroup(mut builder) => {
                    match builder.process_token(token) {
                        ControlFlow::Break(nfa) => {
                            // Nested group completed, treat it as a new factor
                            self.flush_last_factor();
                            self.last_factor = Some(nfa);
                        }
                        ControlFlow::Continue(_) => {
                            // Continue processing with nested builder
                            self.nested_builder = Some(NestedBuilder::CaptureGroup(builder));
                        }
                    }
                    return ControlFlow::Continue(self);
                }
                NestedBuilder::CharClass(builder) => {
                    match builder.process_token(token) {
                        ControlFlow::Break(range_set) => {
                            // Character class completed, create NFA from range set
                            let nfa = NondeterministicFiniteAutomaton::from_range_set(range_set);
                            self.flush_last_factor();
                            self.last_factor = Some(nfa);
                        }
                        ControlFlow::Continue(updated_builder) => {
                            // Continue processing with updated builder
                            self.nested_builder = Some(NestedBuilder::CharClass(updated_builder));
                        }
                    }
                    return ControlFlow::Continue(self);
                }
                NestedBuilder::Quantifier(builder) => {
                    match builder.process_token(token) {
                        ControlFlow::Break((min, max)) => {
                            // Quantifier completed, apply to last factor
                            if let Some(factor) = self.last_factor.take() {
                                self.last_factor = Some(factor.quantify(min, max));
                                self.flush_last_factor();
                            } else {
                                panic!("Quantifier without preceding factor");
                            }
                        }
                        ControlFlow::Continue(updated_builder) => {
                            // Continue processing with updated builder
                            self.nested_builder = Some(NestedBuilder::Quantifier(updated_builder));
                        }
                    }
                    return ControlFlow::Continue(self);
                }
            }
        }

        // Process token normally if no nested builder
        match &token.kind {
            RegexTokenKind::OpenParen => {
                // Flush any pending factor before starting nested group
                self.flush_last_factor();
                // Create and store nested capture group builder
                self.nested_builder = Some(NestedBuilder::CaptureGroup(Box::new(
                    CaptureGroupBuilder::new(false),
                )));
            }
            RegexTokenKind::OpenBracket => {
                // Flush any pending factor before starting character class
                self.flush_last_factor();
                // Create and store nested character class builder
                self.nested_builder = Some(NestedBuilder::CharClass(CharClassBuilder::new()));
            }
            RegexTokenKind::OpenBrace => {
                // Start quantifier - don't flush last factor since we'll quantify it
                if self.last_factor.is_none() {
                    panic!("Quantifier without preceding factor");
                }
                self.nested_builder = Some(NestedBuilder::Quantifier(QuantifierBuilder::new()));
            }
            RegexTokenKind::Star => {
                // Apply * quantifier directly
                if let Some(factor) = self.last_factor.take() {
                    self.last_factor = Some(factor.closure());
                    self.flush_last_factor();
                } else {
                    panic!("* quantifier without preceding factor");
                }
            }
            RegexTokenKind::Plus => {
                // Apply + quantifier directly
                if let Some(factor) = self.last_factor.take() {
                    self.last_factor = Some(factor.positive_closure());
                    self.flush_last_factor();
                } else {
                    panic!("+ quantifier without preceding factor");
                }
            }
            RegexTokenKind::Question => {
                // Apply ? quantifier directly
                if let Some(factor) = self.last_factor.take() {
                    self.last_factor = Some(factor.optional());
                    self.flush_last_factor();
                } else {
                    panic!("? quantifier without preceding factor");
                }
            }
            RegexTokenKind::Or => {
                self.flush_last_factor();
                if let Some(seq) = self.current_sequence.take() {
                    self.processed_alternatives.push(seq);
                }
            }
            RegexTokenKind::CloseParen => {
                if self.is_root {
                    panic!("Unmatched closing parenthesis {:?}", &token);
                }
                self.flush_last_factor();
                if let Some(seq) = self.current_sequence.take() {
                    self.processed_alternatives.push(seq);
                }
                let result = NondeterministicFiniteAutomaton::union_many(
                    self.processed_alternatives.clone(),
                );
                return ControlFlow::Break(result);
            }
            RegexTokenKind::Literal(c) => {
                self.handle_literal(*c);
            }
            RegexTokenKind::Dot => {
                self.handle_dot();
            }
            RegexTokenKind::DigitClass => {
                self.handle_character_class(RangeSet::from_range('0', '9'));
            }
            RegexTokenKind::NonDigitClass => {
                self.handle_character_class(RangeSet::from_negated_ranges(vec!['0'..='9']));
            }
            RegexTokenKind::WordClass => {
                let word_set =
                    RangeSet::from_ranges(vec!['a'..='z', 'A'..='Z', '0'..='9', '_'..='_']);
                self.handle_character_class(word_set);
            }
            RegexTokenKind::NonWordClass => {
                let word_set =
                    RangeSet::from_ranges(vec!['a'..='z', 'A'..='Z', '0'..='9', '_'..='_']);
                self.handle_character_class(word_set.negate());
            }
            RegexTokenKind::SpaceClass => {
                let space_set = RangeSet::from_ranges(vec![
                    ' '..=' ',
                    '\t'..='\t',
                    '\r'..='\r',
                    '\n'..='\n',
                    '\x0C'..='\x0C',
                ]);
                self.handle_character_class(space_set);
            }
            RegexTokenKind::NonSpaceClass => {
                let space_set = RangeSet::from_negated_ranges(vec![
                    ' '..=' ',
                    '\t'..='\t',
                    '\r'..='\r',
                    '\n'..='\n',
                    '\x0C'..='\x0C',
                ]);
                self.handle_character_class(space_set);
            }
            RegexTokenKind::UnicodeProperty(property) => {
                self.handle_unicode_property(property, false);
            }
            RegexTokenKind::NegatedUnicodeProperty(property) => {
                self.handle_unicode_property(property, true);
            }
            RegexTokenKind::Minus => {
                self.handle_literal('-');
            }
            RegexTokenKind::Caret => {
                self.handle_literal('^');
            }
            RegexTokenKind::DollarAnchor => {
                self.handle_literal('$');
            }
            RegexTokenKind::CloseBracket => {
                panic!("Unmatched closing bracket {:?}", &token);
            }
            RegexTokenKind::CloseBrace => {
                panic!("Unmatched closing brace {:?}", &token);
            }
        };
        ControlFlow::Continue(self)
    }

    pub fn handle_literal(&mut self, c: char) {
        self.flush_last_factor();
        self.last_factor = Some(NondeterministicFiniteAutomaton::from_char(c));
    }

    pub fn handle_dot(&mut self) {
        self.flush_last_factor();
        // Dot matches any character except line terminators
        let dot_range = RangeSet::from_negated_ranges(vec![
            '\n'..='\n',
            '\r'..='\r',
            '\u{0085}'..='\u{0085}',
            '\u{2028}'..='\u{2029}',
        ]);
        self.last_factor = Some(NondeterministicFiniteAutomaton::from_range_set(dot_range));
    }

    pub fn handle_character_class(&mut self, range_set: RangeSet) {
        self.flush_last_factor();
        self.last_factor = Some(NondeterministicFiniteAutomaton::from_range_set(range_set));
    }

    pub fn handle_unicode_property(&mut self, property: &UnicodePropertyType, negated: bool) {
        self.flush_last_factor();
        let range_set = match property {
            UnicodePropertyType::Letter => RangeSet::unicode_letter(),
            UnicodePropertyType::Digit => RangeSet::unicode_digit(),
            UnicodePropertyType::Space => RangeSet::unicode_space(),
            UnicodePropertyType::XIDStart => RangeSet::unicode_xid_start(),
            UnicodePropertyType::XIDContinue => RangeSet::unicode_xid_continue(),
            UnicodePropertyType::Upper => RangeSet::unicode_upper(),
            UnicodePropertyType::Lower => RangeSet::unicode_lower(),
            UnicodePropertyType::Other(_) => {
                panic!("Unsupported Unicode property");
            }
        };
        let final_set = if negated {
            range_set.negate()
        } else {
            range_set
        };
        self.last_factor = Some(NondeterministicFiniteAutomaton::from_range_set(final_set));
    }

    pub fn flush_last_factor(&mut self) {
        if let Some(factor) = self.last_factor.take() {
            self.current_sequence = match self.current_sequence.take() {
                Some(seq) => Some(seq.concat(factor)),
                None => Some(factor),
            };
        }
    }

    pub fn handle_end_of_regex(mut self) -> NondeterministicFiniteAutomaton {
        if !self.is_root {
            panic!("Only root capture group can handle end of regex");
        }
        match self.nested_builder {
            Some(NestedBuilder::CaptureGroup(_)) => {
                panic!("Unmatched opening parenthesis at end of regex");
            }
            Some(NestedBuilder::CharClass(_)) => {
                panic!("Unmatched opening bracket at end of regex");
            }
            Some(NestedBuilder::Quantifier(_)) => {
                panic!("Unmatched opening brace at end of regex");
            }
            None => {}
        }
        self.flush_last_factor();
        if let Some(seq) = self.current_sequence.take() {
            self.processed_alternatives.push(seq);
        }
        NondeterministicFiniteAutomaton::union_many(self.processed_alternatives)
    }
}
