use std::rc::Rc;

use crate::{Token, UmatchedError, dfa::{DFAState, DFAStateHandle, DeterministicFiniteAutomaton}, token::TokenType};

#[derive(Debug, Clone)]
pub struct PlainLexer {
    lexer_spec: Vec<Rc<TokenType>>,
    dfa: DeterministicFiniteAutomaton,
}

impl PlainLexer {
    pub fn new(lexer_spec: Vec<Rc<TokenType>>) -> Self {
        let dfa = DeterministicFiniteAutomaton::from_lexer_spec(&lexer_spec);
        Self { lexer_spec, dfa }
    }

    pub fn tokenize<'a, I>(
        &'a self,
        chars: I,
    ) -> impl Iterator<Item = Result<Token<Rc<TokenType>>, UmatchedError>> + 'a
    where
        I: Iterator<Item = char> + 'a,
    {
        PlainLexerIter::new(&self.dfa, chars)
    }

    pub fn iter_states(&self) -> impl Iterator<Item = (DFAStateHandle, &DFAState)> {
        self.dfa.iter_states()
    }
}

pub struct AcceptRecord {
    pub token_type: Rc<TokenType>,
    pub state: DFAStateHandle,
}

pub struct PlainLexerIter<'a, I: Iterator<Item = char>> {
    dfa: &'a DeterministicFiniteAutomaton,
    chars: I,
    buffer: Vec<char>,
    next_index: usize,
}

impl<'a, I: Iterator<Item = char>> PlainLexerIter<'a, I> {
    fn new(dfa: &'a DeterministicFiniteAutomaton, chars: I) -> Self {
        Self {
            dfa,
            chars,
            buffer: Vec::new(),
            next_index: 0,
        }
    }

    fn finalize_token(
        &mut self,
        last_accept: Option<AcceptRecord>,
        lexeme: String,
        unidentified: Vec<char>,
        buf_index: usize,
        start_index: usize,
    ) -> Option<Result<Token<Rc<TokenType>>, UmatchedError>> {
        if let Some(accept) = last_accept {
            // Successfully matched a token
            self.update_buffer(unidentified, buf_index);
            Some(Ok(Token {
                kind: accept.token_type,
                lexeme,
                start_index,
                end_index: self.next_index,
            }))
        } else {
            // No match found, return error
            let error_content: String = unidentified.into_iter().collect();
            let error_len = error_content.len();
            self.next_index += error_len;
            self.update_buffer(Vec::new(), buf_index);
            Some(Err(UmatchedError {
                content: error_content,
                start_index,
                end_index: self.next_index,
            }))
        }
    }

    fn finalize_end_of_input(
        &mut self,
        last_accept: Option<AcceptRecord>,
        mut lexeme: String,
        unidentified: Vec<char>,
        start_index: usize,
    ) -> Option<Result<Token<Rc<TokenType>>, UmatchedError>> {
        if let Some(accept) = last_accept {
            // Return the last accepted token
            self.buffer = unidentified;
            Some(Ok(Token {
                kind: accept.token_type,
                lexeme,
                start_index,
                end_index: self.next_index,
            }))
        } else if !unidentified.is_empty() {
            // Return unmatched content as error
            lexeme.extend(unidentified);
            let error_len = lexeme.len();
            self.next_index += error_len;
            self.buffer.clear();
            Some(Err(UmatchedError {
                content: lexeme,
                start_index,
                end_index: self.next_index,
            }))
        } else {
            // Nothing left to process
            None
        }
    }

    fn update_buffer(&mut self, mut unidentified: Vec<char>, buf_index: usize) {
        if buf_index < self.buffer.len() {
            unidentified.extend_from_slice(&self.buffer[buf_index..]);
        }
        self.buffer = unidentified;
    }
}

impl<'a, I: Iterator<Item = char>> Iterator for PlainLexerIter<'a, I> {
    type Item = Result<Token<Rc<TokenType>>, UmatchedError>;

    fn next(&mut self) -> Option<Self::Item> {
        let start_index = self.next_index;
        let mut current_state = self.dfa.start_state();
        let mut last_accept: Option<AcceptRecord> = None;
        let mut buf_index = 0;
        let mut lexeme = String::new();
        let mut unidentified = Vec::new();

        loop {
            // Get next character from buffer or input stream
            let next_char = if buf_index < self.buffer.len() {
                let ch = self.buffer[buf_index];
                buf_index += 1;
                Some(ch)
            } else {
                self.chars.next()
            };

            match next_char {
                Some(ch) => {
                    unidentified.push(ch);

                    if let Some(next_state) = self.dfa.next_state(current_state, ch) {
                        // Continue DFA traversal
                        current_state = next_state.current_state;
                        if let Some(token_type) = next_state.accepted_pattern {
                            last_accept = Some(AcceptRecord {
                                token_type,
                                state: current_state,
                            });
                            self.next_index += unidentified.len();
                            lexeme.extend(unidentified.drain(..));
                        }
                    } else {
                        // DFA cannot continue, finalize current token or error
                        return self.finalize_token(
                            last_accept,
                            lexeme,
                            unidentified,
                            buf_index,
                            start_index,
                        );
                    }
                }
                None => {
                    // End of input
                    return self.finalize_end_of_input(
                        last_accept,
                        lexeme,
                        unidentified,
                        start_index,
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_python_lexer_with_unicode() {
        // Define a Python-like lexer that supports Unicode identifiers
        let lexer_spec = vec![
            // Keywords
            Rc::new(TokenType::new("IF".to_string(), r"if".to_string())),
            Rc::new(TokenType::new("ELSE".to_string(), r"else".to_string())),
            Rc::new(TokenType::new("DEF".to_string(), r"def".to_string())),
            Rc::new(TokenType::new("RETURN".to_string(), r"return".to_string())),
            // Unicode identifiers (Python 3 style - allows Unicode letters)
            Rc::new(TokenType::new(
                "IDENTIFIER".to_string(),
                r"[\p{L}_][\p{L}\p{Nd}_]*".to_string(),
            )),
            // Numbers
            Rc::new(TokenType::new("NUMBER".to_string(), r"[0-9]+".to_string())),
            // String literals with Unicode
            Rc::new(TokenType::new(
                "STRING".to_string(),
                r#""[^"]*""#.to_string(),
            )),
            // Operators and delimiters
            Rc::new(TokenType::new("PLUS".to_string(), r"\+".to_string())),
            Rc::new(TokenType::new("MINUS".to_string(), r"-".to_string())),
            Rc::new(TokenType::new("ASSIGN".to_string(), r"=".to_string())),
            Rc::new(TokenType::new("LPAREN".to_string(), r"\(".to_string())),
            Rc::new(TokenType::new("RPAREN".to_string(), r"\)".to_string())),
            Rc::new(TokenType::new("COLON".to_string(), r":".to_string())),
            // Whitespace
            Rc::new(TokenType::new(
                "WHITESPACE".to_string(),
                r"[ \t\n]+".to_string(),
            )),
        ];

        let lexer = PlainLexer::new(lexer_spec);

        // Test Unicode identifiers and strings
        let code = r#"def 计算(数字一, 数字二):
    结果 = 数字一 + 数字二
    return "答案是" + 结果"#;

        let tokens: Result<Vec<_>, _> = lexer.tokenize(code.chars()).collect();

        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();

        // Verify Unicode identifiers are correctly tokenized
        assert_eq!(tokens[0].kind.name, "DEF");
        assert_eq!(tokens[2].kind.name, "IDENTIFIER");
        assert_eq!(tokens[2].lexeme, "计算");
        assert_eq!(tokens[4].kind.name, "IDENTIFIER");
        assert_eq!(tokens[4].lexeme, "数字一");
        assert_eq!(tokens[6].kind.name, "IDENTIFIER");
        assert_eq!(tokens[6].lexeme, "数字二");

        // Find and verify the Unicode string
        let string_tokens: Vec<_> = tokens.iter().filter(|t| t.kind.name == "STRING").collect();
        assert!(!string_tokens.is_empty());
        assert_eq!(string_tokens[0].lexeme, r#""答案是""#);
    }

    #[test]
    fn test_mixed_scripts_and_emoji() {
        let lexer_spec = vec![
            // Identifier supporting various Unicode scripts
            Rc::new(TokenType::new(
                "IDENTIFIER".to_string(),
                r"[\p{L}_][\p{L}\p{Nd}_]*".to_string(),
            )),
            // Emoji as operators (for fun!)
            Rc::new(TokenType::new("EMOJI_PLUS".to_string(), r"➕".to_string())),
            Rc::new(TokenType::new("EMOJI_MINUS".to_string(), r"➖".to_string())),
            Rc::new(TokenType::new(
                "EMOJI_MULTIPLY".to_string(),
                r"✖️".to_string(),
            )),
            // Numbers
            Rc::new(TokenType::new("NUMBER".to_string(), r"[0-9]+".to_string())),
            // Assignment
            Rc::new(TokenType::new("ASSIGN".to_string(), r"=".to_string())),
            // Whitespace
            Rc::new(TokenType::new(
                "WHITESPACE".to_string(),
                r"[ \t]+".to_string(),
            )),
        ];

        let lexer = PlainLexer::new(lexer_spec);

        // Test with mixed scripts: Latin, Greek, Cyrillic, Arabic, Japanese
        let code = "αλφα = 42 ➕ число ➖ العدد ➕ 数";
        let tokens: Result<Vec<_>, _> = lexer.tokenize(code.chars()).collect();

        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();

        // Greek identifier
        assert_eq!(tokens[0].kind.name, "IDENTIFIER");
        assert_eq!(tokens[0].lexeme, "αλφα");

        // Number
        assert_eq!(tokens[4].kind.name, "NUMBER");
        assert_eq!(tokens[4].lexeme, "42");

        // Emoji operator
        assert_eq!(tokens[6].kind.name, "EMOJI_PLUS");
        assert_eq!(tokens[6].lexeme, "➕");

        // Cyrillic identifier
        assert_eq!(tokens[8].kind.name, "IDENTIFIER");
        assert_eq!(tokens[8].lexeme, "число");

        // Another emoji operator
        assert_eq!(tokens[10].kind.name, "EMOJI_MINUS");
        assert_eq!(tokens[10].lexeme, "➖");

        // Arabic identifier
        assert_eq!(tokens[12].kind.name, "IDENTIFIER");
        assert_eq!(tokens[12].lexeme, "العدد");

        // Chinese character identifier
        let last_identifier = tokens
            .iter()
            .filter(|t| t.kind.name == "IDENTIFIER")
            .last()
            .unwrap();
        assert_eq!(last_identifier.lexeme, "数");
    }

    #[test]
    fn test_unicode_comments_and_special_chars() {
        let lexer_spec = vec![
            // Single-line comment with various Unicode chars
            Rc::new(TokenType::new(
                "COMMENT".to_string(),
                r"#[^\n]*".to_string(),
            )),
            // Identifiers
            Rc::new(TokenType::new(
                "IDENTIFIER".to_string(),
                r"[\p{L}_][\p{L}\p{Nd}_]*".to_string(),
            )),
            // Special Unicode brackets
            Rc::new(TokenType::new(
                "LEFT_BRACKET".to_string(),
                r"【".to_string(),
            )),
            Rc::new(TokenType::new(
                "RIGHT_BRACKET".to_string(),
                r"】".to_string(),
            )),
            // Unicode arrows as operators
            Rc::new(TokenType::new("ARROW_RIGHT".to_string(), r"→".to_string())),
            Rc::new(TokenType::new("ARROW_LEFT".to_string(), r"←".to_string())),
            // Numbers
            Rc::new(TokenType::new("NUMBER".to_string(), r"[0-9]+".to_string())),
            // Whitespace
            Rc::new(TokenType::new(
                "WHITESPACE".to_string(),
                r"[ \t\n]+".to_string(),
            )),
        ];

        let lexer = PlainLexer::new(lexer_spec);

        let code = "# こんにちは世界 🌍 Comment with emojis! 😊\n【配列】 → 123 ← result";
        let tokens: Result<Vec<_>, _> = lexer.tokenize(code.chars()).collect();

        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();

        // Verify comment with Unicode and emojis
        assert_eq!(tokens[0].kind.name, "COMMENT");
        assert!(tokens[0].lexeme.contains("こんにちは"));
        assert!(tokens[0].lexeme.contains("🌍"));
        assert!(tokens[0].lexeme.contains("😊"));

        // Unicode brackets
        assert_eq!(tokens[2].kind.name, "LEFT_BRACKET");
        assert_eq!(tokens[2].lexeme, "【");
        assert_eq!(tokens[3].kind.name, "IDENTIFIER");
        assert_eq!(tokens[3].lexeme, "配列");
        assert_eq!(tokens[4].kind.name, "RIGHT_BRACKET");
        assert_eq!(tokens[4].lexeme, "】");

        // Unicode arrows
        assert_eq!(tokens[6].kind.name, "ARROW_RIGHT");
        assert_eq!(tokens[6].lexeme, "→");
        assert_eq!(tokens[10].kind.name, "ARROW_LEFT");
        assert_eq!(tokens[10].lexeme, "←");
    }

    #[test]
    fn test_unicode_unmatched_error() {
        let lexer_spec = vec![
            Rc::new(TokenType::new(
                "IDENTIFIER".to_string(),
                r"[\p{L}_][\p{L}\p{Nd}_]*".to_string(),
            )),
            Rc::new(TokenType::new("NUMBER".to_string(), r"[0-9]+".to_string())),
            Rc::new(TokenType::new(
                "WHITESPACE".to_string(),
                r"[ \t]+".to_string(),
            )),
        ];

        let lexer = PlainLexer::new(lexer_spec);

        // Test with symbols that aren't recognized (mathematical symbols, emojis)
        let code = "var ∑ 123 ∏ test 🚀";
        let results: Vec<_> = lexer.tokenize(code.chars()).collect();

        // Should have errors for ∑, ∏, and 🚀
        let errors: Vec<_> = results.iter().filter_map(|r| r.as_ref().err()).collect();

        assert!(!errors.is_empty());

        // Check that mathematical symbols cause errors
        let error_contents: Vec<_> = errors.iter().map(|e| e.content.as_str()).collect();

        assert!(error_contents.contains(&"∑"));
        assert!(error_contents.contains(&"∏"));
        assert!(error_contents.contains(&"🚀"));

        // Check that identifiers are still recognized
        let tokens: Vec<_> = results.iter().filter_map(|r| r.as_ref().ok()).collect();

        let identifiers: Vec<_> = tokens
            .iter()
            .filter(|t| t.kind.name == "IDENTIFIER")
            .map(|t| t.lexeme.as_str())
            .collect();

        assert!(identifiers.contains(&"var"));
        assert!(identifiers.contains(&"test"));
    }

    #[test]
    fn test_identifier_with_digits() {
        let lexer_spec = vec![
            // Identifier that can contain decimal digits from various scripts
            Rc::new(TokenType::new(
                "IDENTIFIER".to_string(),
                r"[\p{L}_][\p{L}\p{Nd}_]*".to_string(),
            )),
            // Numbers (only ASCII digits for simplicity)
            Rc::new(TokenType::new("NUMBER".to_string(), r"[0-9]+".to_string())),
            // Whitespace
            Rc::new(TokenType::new(
                "WHITESPACE".to_string(),
                r"[ \t]+".to_string(),
            )),
        ];

        let lexer = PlainLexer::new(lexer_spec);

        // Test identifiers with various decimal digits
        let code = "var123 test_456 数字789 переменная০১২";
        let tokens: Result<Vec<_>, _> = lexer.tokenize(code.chars()).collect();

        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();

        // Check all identifiers with digits are recognized
        assert_eq!(tokens[0].kind.name, "IDENTIFIER");
        assert_eq!(tokens[0].lexeme, "var123");

        assert_eq!(tokens[2].kind.name, "IDENTIFIER");
        assert_eq!(tokens[2].lexeme, "test_456");

        assert_eq!(tokens[4].kind.name, "IDENTIFIER");
        assert_eq!(tokens[4].lexeme, "数字789");

        assert_eq!(tokens[6].kind.name, "IDENTIFIER");
        assert_eq!(tokens[6].lexeme, "переменная০১২"); // Bengali digits
    }
}
