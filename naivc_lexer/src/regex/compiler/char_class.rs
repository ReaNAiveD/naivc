use std::ops::ControlFlow;

use crate::regex::{RangeSet, RegexToken, RegexTokenKind, UnicodePropertyType};

/// Helper struct for building a RangeSet from character class tokens
#[derive(Debug, Clone)]
pub struct CharClassBuilder {
    range_set: RangeSet,
    is_negated: bool,
    is_first: bool,
    last_literal: Option<char>,
    range_start: Option<char>,
}

impl CharClassBuilder {
    pub fn new() -> Self {
        Self {
            range_set: RangeSet::empty(),
            is_negated: false,
            is_first: true,
            last_literal: None,
            range_start: None,
        }
    }

    /// Returns ControlFlow::Break with the finalized RangeSet when complete,
    /// or ControlFlow::Continue with self to continue processing
    pub fn process_token(
        mut self,
        token: &RegexToken<RegexTokenKind>,
    ) -> ControlFlow<RangeSet, Self> {
        match &token.kind {
            RegexTokenKind::Caret if self.is_first => {
                self.is_negated = true;
            }
            RegexTokenKind::Caret => {
                self.handle_literal('^');
            }
            RegexTokenKind::Literal(c) => {
                self.handle_literal(*c);
            }
            RegexTokenKind::Minus => {
                self.handle_minus();
            }
            RegexTokenKind::CloseBracket => {
                // Finalize and build the RangeSet
                self.flush_last_literal();
                let range_set = if self.is_negated {
                    self.range_set.negate()
                } else {
                    self.range_set
                };
                return ControlFlow::Break(range_set); // Signal end of character class and return built RangeSet
            }
            RegexTokenKind::DigitClass => {
                self.add_digit_class();
            }
            RegexTokenKind::NonDigitClass => {
                self.add_non_digit_class();
            }
            RegexTokenKind::WordClass => {
                self.add_word_class();
            }
            RegexTokenKind::NonWordClass => {
                self.add_non_word_class();
            }
            RegexTokenKind::SpaceClass => {
                self.add_space_class();
            }
            RegexTokenKind::NonSpaceClass => {
                self.add_non_space_class();
            }
            RegexTokenKind::Dot => {
                self.handle_literal('.');
            }
            RegexTokenKind::Or => {
                self.handle_literal('|');
            }
            RegexTokenKind::OpenParen => {
                self.handle_literal('(');
            }
            RegexTokenKind::CloseParen => {
                self.handle_literal(')');
            }
            RegexTokenKind::OpenBracket => {
                self.handle_literal('[');
            }
            RegexTokenKind::Star => {
                self.handle_literal('*');
            }
            RegexTokenKind::Question => {
                self.handle_literal('?');
            }
            RegexTokenKind::Plus => {
                self.handle_literal('+');
            }
            RegexTokenKind::DollarAnchor => {
                self.handle_literal('$');
            }
            RegexTokenKind::OpenBrace => {
                self.handle_literal('{');
            }
            RegexTokenKind::CloseBrace => {
                self.handle_literal('}');
            }
            RegexTokenKind::UnicodeProperty(property) => {
                self.add_unicode_class(property);
            }
            RegexTokenKind::NegatedUnicodeProperty(property) => {
                self.add_negated_unicode_class(property);
            }
        }
        self.is_first = false;
        ControlFlow::Continue(self) // Continue processing with updated builder
    }

    fn handle_literal(&mut self, c: char) {
        if let Some(start_char) = self.range_start.take() {
            // We are in a range
            if start_char > c {
                panic!("Invalid character range: {}-{}", start_char, c);
            }
            self.range_set = self.range_set.add_range(start_char, c);
        } else if let Some(last_char) = self.last_literal.replace(c) {
            self.range_set = self.range_set.add_char(last_char);
        }
    }

    fn handle_minus(&mut self) {
        if let Some(last_char) = self.last_literal.take() {
            // Start of a range
            self.range_start = Some(last_char);
        } else {
            // Literal '-'
            self.range_set = self.range_set.add_char('-');
        }
    }

    fn add_digit_class(&mut self) {
        self.flush_last_literal();
        self.range_set = self.range_set.add_range('0', '9');
    }

    fn add_non_digit_class(&mut self) {
        self.flush_last_literal();
        self.range_set = self.range_set.add_negated(RangeSet::from_range('0', '9'));
    }

    fn add_word_class(&mut self) {
        self.flush_last_literal();
        self.range_set = self
            .range_set
            .add_range('a', 'z')
            .add_range('A', 'Z')
            .add_range('0', '9')
            .add_char('_');
    }

    fn add_non_word_class(&mut self) {
        self.flush_last_literal();
        self.range_set = self.range_set.add_negated(RangeSet::from_ranges(vec![
            'a'..='z',
            'A'..='Z',
            '0'..='9',
            '_'..='_',
        ]));
    }

    fn add_space_class(&mut self) {
        self.flush_last_literal();
        self.range_set = self.range_set.add(RangeSet::from_ranges(vec![
            ' '..=' ',
            '\t'..='\t',
            '\r'..='\r',
            '\n'..='\n',
            '\x0C'..='\x0C',
        ]));
    }

    fn add_non_space_class(&mut self) {
        self.flush_last_literal();
        self.range_set = self.range_set.add_negated(RangeSet::from_ranges(vec![
            ' '..=' ',
            '\t'..='\t',
            '\r'..='\r',
            '\n'..='\n',
            '\x0C'..='\x0C',
        ]));
    }

    fn add_unicode_class(&mut self, property: &UnicodePropertyType) {
        self.flush_last_literal();
        match property {
            UnicodePropertyType::Letter => {
                self.range_set = self.range_set.add(RangeSet::unicode_letter())
            }
            UnicodePropertyType::Digit => {
                self.range_set = self.range_set.add(RangeSet::unicode_digit())
            }
            UnicodePropertyType::Space => {
                self.range_set = self.range_set.add(RangeSet::unicode_space())
            }
            UnicodePropertyType::XIDStart => {
                self.range_set = self.range_set.add(RangeSet::unicode_xid_start())
            }
            UnicodePropertyType::XIDContinue => {
                self.range_set = self.range_set.add(RangeSet::unicode_xid_continue())
            }
            UnicodePropertyType::Upper => {
                self.range_set = self.range_set.add(RangeSet::unicode_upper())
            }
            UnicodePropertyType::Lower => {
                self.range_set = self.range_set.add(RangeSet::unicode_lower())
            }
            UnicodePropertyType::Other(_) => {
                panic!("Unsupported Unicode property in character class");
            }
        }
    }

    fn add_negated_unicode_class(&mut self, property: &UnicodePropertyType) {
        self.flush_last_literal();
        match property {
            UnicodePropertyType::Letter => {
                self.range_set = self.range_set.add_negated(RangeSet::unicode_letter())
            }
            UnicodePropertyType::Digit => {
                self.range_set = self.range_set.add_negated(RangeSet::unicode_digit())
            }
            UnicodePropertyType::Space => {
                self.range_set = self.range_set.add_negated(RangeSet::unicode_space())
            }
            UnicodePropertyType::XIDStart => {
                self.range_set = self.range_set.add_negated(RangeSet::unicode_xid_start())
            }
            UnicodePropertyType::XIDContinue => {
                self.range_set = self.range_set.add_negated(RangeSet::unicode_xid_continue())
            }
            UnicodePropertyType::Upper => {
                self.range_set = self.range_set.add_negated(RangeSet::unicode_upper())
            }
            UnicodePropertyType::Lower => {
                self.range_set = self.range_set.add_negated(RangeSet::unicode_lower())
            }
            UnicodePropertyType::Other(_) => {
                panic!("Unsupported Unicode property in character class");
            }
        }
    }

    fn flush_last_literal(&mut self) {
        if let Some(start_char) = self.range_start.take() {
            // Trailing minus should be treated as literal
            self.range_set = self.range_set.add_char(start_char).add_char('-');
        }
        if let Some(last_char) = self.last_literal.take() {
            self.range_set = self.range_set.add_char(last_char);
        }
    }
}
