use std::ops::ControlFlow;

use crate::regex::{RegexToken, RegexTokenKind};

/// Helper struct for building quantifier bounds from tokens
#[derive(Debug, Clone)]
pub struct QuantifierBuilder {
    min: Option<usize>,
    max: Option<usize>,
    met_comma: bool,
}

impl QuantifierBuilder {
    pub fn new() -> Self {
        Self {
            min: None,
            max: None,
            met_comma: false,
        }
    }

    /// Returns ControlFlow::Break with the finalized quantifier bounds when complete,
    /// or ControlFlow::Continue with self to continue processing
    pub fn process_token(mut self, token: &RegexToken<RegexTokenKind>) -> ControlFlow<(Option<usize>, Option<usize>), Self> {
        match &token.kind {
            RegexTokenKind::Literal(c) => {
                if c.is_ascii_digit() {
                    self.handle_digit(*c);
                } else if *c == ',' {
                    self.handle_comma();
                } else {
                    panic!("Invalid character in quantifier: {}", c);
                }
            }
            RegexTokenKind::CloseBrace => {
                // Finalize and return the quantifier bounds
                self.finalize();
                return ControlFlow::Break((self.min, self.max));
            }
            _ => {
                panic!("Unexpected token in quantifier: {:?}", token.kind);
            }
        }
        ControlFlow::Continue(self)
    }

    fn handle_digit(&mut self, c: char) {
        let digit = c.to_digit(10).unwrap() as usize;
        if !self.met_comma {
            self.min = Some(
                self.min.unwrap_or(0)
                    .checked_mul(10)
                    .and_then(|n| n.checked_add(digit))
                    .expect("Quantifier minimum value too large")
            );
        } else {
            self.max = Some(
                self.max.unwrap_or(0)
                    .checked_mul(10)
                    .and_then(|n| n.checked_add(digit))
                    .expect("Quantifier maximum value too large")
            );
        }
    }

    fn handle_comma(&mut self) {
        if self.met_comma {
            panic!("Multiple commas in quantifier");
        }
        self.met_comma = true;
    }

    fn finalize(&mut self) {
        if !self.met_comma {
            if self.min.is_none() {
                panic!("Empty quantifier");
            }
            self.max = self.min;
        } else if self.min.is_none() && self.max.is_none() {
            panic!("Invalid quantifier: missing min and max");
        } else if self.min.is_none() {
            // {,n} - from 0 to n (some flavors support this)
            self.min = Some(0);
        }
        // Validate min <= max when both are present
        if let (Some(min_val), Some(max_val)) = (self.min, self.max) {
            if min_val > max_val {
                panic!("Invalid quantifier: min ({}) cannot be greater than max ({})", 
                       min_val, max_val);
            }
        }
    }
}