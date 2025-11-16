use std::fmt::Debug;

#[derive(Debug, Clone, PartialEq)]
pub enum UnicodePropertyType {
    /// `\p{L}` - Any letter (most common)
    Letter,
    /// `\p{Nd}` - Decimal digits
    Digit,
    /// `\p{Z}` - Separators/spaces
    Space,

    /// `\p{XID_Start}` - Valid identifier start
    XIDStart,
    /// `\p{XID_Continue}` - Valid identifier continuation
    XIDContinue,

    /// `\p{Lu}` - Uppercase
    Upper,
    /// `\p{Ll}` - Lowercase
    Lower,

    /// For any other property we don't explicitly handle
    Other(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Literal(char),
    /// `.`
    Dot,
    /// `|`
    Or,
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
    /// `-`
    Minus,
    /// `^`
    Caret,
    /// `*`
    Star,
    /// `?`
    Question,
    /// `+`
    Plus,
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
    /// `$`
    DollarAnchor,

    /// `\d` = [0-9]
    DigitClass,
    /// `\D` = [^0-9]
    NonDigitClass,
    /// `\w` = [a-zA-Z0-9_]
    WordClass,
    /// `\W` = [^a-zA-Z0-9_]
    NonWordClass,
    /// `\s` = [ \t\r\n\f]
    SpaceClass,
    /// `\S` = [^ \t\r\n\f]
    NonSpaceClass,

    /// `\p{...}` - Unicode property
    UnicodeProperty(UnicodePropertyType),
    /// `\P{...}` - Negated Unicode property
    NegatedUnicodeProperty(UnicodePropertyType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<K> where K: Debug + Clone + PartialEq {
    pub kind: K,
    pub span: (usize, usize),
}

impl<K> Token<K> where K: Debug + Clone + PartialEq {
    pub fn new(kind: K, span: (usize, usize)) -> Self {
        Self { kind, span }
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, position: 0 }
    }

    fn peek(&self) -> Option<char> {
        self.input[self.position..].chars().next()
    }
    
    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.position += ch.len_utf8();
        Some(ch)
    }
    
    fn next_token(&mut self) -> Option<Token<TokenKind>> {
        let start = self.position;
        let ch = self.advance()?;
        
        let kind = match ch {
            '.' => TokenKind::Dot,
            '|' => TokenKind::Or,
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            '-' => TokenKind::Minus,
            '^' => TokenKind::Caret,
            '*' => TokenKind::Star,
            '?' => TokenKind::Question,
            '+' => TokenKind::Plus,
            '$' => TokenKind::DollarAnchor,
            '\\' => {
                // Handle escape sequences
                if let Some(escaped) = self.advance() {
                    match escaped {
                        // Whitespace escapes
                        'n' => TokenKind::Literal('\n'),  // Newline
                        't' => TokenKind::Literal('\t'),  // Tab
                        'r' => TokenKind::Literal('\r'),  // Carriage return
                        'f' => TokenKind::Literal('\x0C'), // Form feed
                        'v' => TokenKind::Literal('\x0B'), // Vertical tab
                        
                        // Regex metacharacters
                        '\\' => TokenKind::Literal('\\'), // Backslash
                        '.' => TokenKind::Literal('.'),   // Dot
                        '|' => TokenKind::Literal('|'),   // Or
                        '(' => TokenKind::Literal('('),   // Open paren
                        ')' => TokenKind::Literal(')'),   // Close paren
                        '{' => TokenKind::Literal('{'),   // Open brace
                        '}' => TokenKind::Literal('}'),   // Close brace
                        '[' => TokenKind::Literal('['),   // Open bracket
                        ']' => TokenKind::Literal(']'),   // Close bracket
                        '*' => TokenKind::Literal('*'),   // Star
                        '+' => TokenKind::Literal('+'),   // Plus
                        '?' => TokenKind::Literal('?'),   // Question
                        '^' => TokenKind::Literal('^'),   // Caret
                        '$' => TokenKind::Literal('$'),   // Dollar
                        '-' => TokenKind::Literal('-'),   // Minus/dash
                        
                        // Quote escapes
                        '\'' => TokenKind::Literal('\''), // Single quote
                        '"' => TokenKind::Literal('"'),   // Double quote
                        
                        // Character class shortcuts (these need special handling)
                        'd' => TokenKind::DigitClass,     // \d = [0-9]
                        'D' => TokenKind::NonDigitClass,  // \D = [^0-9]
                        'w' => TokenKind::WordClass,      // \w = [a-zA-Z0-9_]
                        'W' => TokenKind::NonWordClass,   // \W = [^a-zA-Z0-9_]
                        's' => TokenKind::SpaceClass,     // \s = [ \t\r\n\f]
                        'S' => TokenKind::NonSpaceClass,  // \S = [^ \t\r\n\f]
                        
                        // Unicode escapes (need more complex parsing)
                        'u' => {
                            // Parse \u{XXXX} or \uXXXX
                            match self.peek() {
                                Some('{') => {
                                    self.advance(); // consume '{'
                                    let mut codepoint = String::new();
                                    while let Some(c) = self.peek() {
                                        if c == '}' {
                                            self.advance(); // consume '}'
                                            break;
                                        }
                                        codepoint.push(c);
                                        self.advance();
                                    }
                                    // Convert codepoint string to char
                                    if let Ok(cp) = u32::from_str_radix(&codepoint, 16) {
                                        if let Some(ch) = std::char::from_u32(cp) {
                                            TokenKind::Literal(ch)
                                        } else {
                                            // Invalid codepoint
                                            TokenKind::Literal('\u{FFFD}') // Replacement character
                                        }
                                    } else {
                                        // Invalid hex
                                        TokenKind::Literal('\u{FFFD}') // Replacement character
                                    }
                                }
                                Some(c1) if c1.is_ascii_hexdigit() => {
                                    let mut hex = String::new();
                                    hex.push(c1);
                                    self.advance();
                                    for _ in 0..3 {
                                        if let Some(cn) = self.peek() {
                                            if cn.is_ascii_hexdigit() {
                                                hex.push(cn);
                                                self.advance();
                                            } else {
                                                break;
                                            }
                                        }
                                    }
                                    if let Ok(cp) = u32::from_str_radix(&hex, 16) {
                                        if let Some(ch) = std::char::from_u32(cp) {
                                            TokenKind::Literal(ch)
                                        } else {
                                            TokenKind::Literal('\u{FFFD}')
                                        }
                                    } else {
                                        TokenKind::Literal('\u{FFFD}')
                                    }
                                }
                                _ => TokenKind::Literal('u'),
                            }
                        },
                        'x' => {
                            // Parse \xHH (hex escape)
                            let mut hex = String::new();
                            for _ in 0..2 {
                                if let Some(c) = self.peek() {
                                    if c.is_ascii_hexdigit() {
                                        hex.push(c);
                                        self.advance();
                                    } else {
                                        break;
                                    }
                                }
                            }
                            if let Ok(byte) = u8::from_str_radix(&hex, 16) {
                                TokenKind::Literal(byte as char)
                            } else {
                                TokenKind::Literal('\u{FFFD}') // Replacement character
                            }
                        },
                        '0'..='7' => {
                            // Only if parsing C-style strings
                            let mut octal = String::new();
                            octal.push(escaped);
                            for _ in 0..2 {
                                if let Some(c) = self.peek() {
                                    if c >= '0' && c <= '7' {
                                        octal.push(c);
                                        self.advance();
                                    } else {
                                        break;
                                    }
                                }
                            }
                            if let Ok(byte) = u8::from_str_radix(&octal, 8) {
                                TokenKind::Literal(byte as char)
                            } else {
                                TokenKind::Literal('\u{FFFD}') // Replacement character
                            }
                        },
                        'p' | 'P' => {
                            // Unicode property escapes \p{...} or \P{...}
                            let is_negated = escaped == 'P';
                            if let Some('{') = self.peek() {
                                self.advance(); // consume '{'
                                let mut property = String::new();
                                while let Some(c) = self.peek() {
                                    if c == '}' {
                                        self.advance(); // consume '}'
                                        break;
                                    }
                                    property.push(c);
                                    self.advance();
                                }
                                
                                // Map to simplified property types
                                let prop_type = match property.as_str() {
                                    "L" | "Letter" => UnicodePropertyType::Letter,
                                    "Nd" | "Digit" | "Decimal_Number" => UnicodePropertyType::Digit,
                                    "Z" | "Space" | "Separator" => UnicodePropertyType::Space,
                                    "Lu" | "Uppercase" | "Uppercase_Letter" => UnicodePropertyType::Upper,
                                    "Ll" | "Lowercase" | "Lowercase_Letter" => UnicodePropertyType::Lower,
                                    "XID_Start" => UnicodePropertyType::XIDStart,
                                    "XID_Continue" => UnicodePropertyType::XIDContinue,
                                    _ => UnicodePropertyType::Other(property),
                                };
                                
                                if is_negated {
                                    TokenKind::NegatedUnicodeProperty(prop_type)
                                } else {
                                    TokenKind::UnicodeProperty(prop_type)
                                }
                            } else {
                                // Invalid \p or \P usage
                                TokenKind::Literal(escaped)
                            }
                        },
                        // Default: treat as literal
                        other => TokenKind::Literal(other),
                    }
                } else {
                    TokenKind::Literal('\\')
                }
            },
            _ => TokenKind::Literal(ch),
        };
        
        Some(Token::new(kind, (start, self.position)))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<TokenKind>;
    
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_token_kind(input: &str, expected: TokenKind) {
        let mut lexer = Lexer::new(input);
        let token = lexer.next().expect("Expected a token");
        assert_eq!(token.kind, expected, "Expected {:?}, got {:?}", expected, token.kind);
    }

    fn assert_tokens(input: &str, expected: Vec<TokenKind>) {
        let lexer = Lexer::new(input);
        let tokens: Vec<TokenKind> = lexer.map(|t| t.kind).collect();
        assert_eq!(tokens.len(), expected.len(), "Token count mismatch for input '{}'", input);
        for (i, (actual, expected)) in tokens.iter().zip(expected.iter()).enumerate() {
            assert!(
                std::mem::discriminant(actual) == std::mem::discriminant(expected),
                "Token {} mismatch for input '{}': expected {:?}, got {:?}",
                i, input, expected, actual
            );
        }
    }

    #[test]
    fn test_literal_characters() {
        assert_token_kind("a", TokenKind::Literal('a'));
        assert_token_kind("z", TokenKind::Literal('z'));
        assert_token_kind("0", TokenKind::Literal('0'));
        assert_token_kind("9", TokenKind::Literal('9'));
        assert_token_kind(" ", TokenKind::Literal(' '));
        assert_token_kind("@", TokenKind::Literal('@'));
    }

    #[test]
    fn test_special_characters() {
        assert_token_kind(".", TokenKind::Dot);
        assert_token_kind("|", TokenKind::Or);
        assert_token_kind("(", TokenKind::OpenParen);
        assert_token_kind(")", TokenKind::CloseParen);
        assert_token_kind("[", TokenKind::OpenBracket);
        assert_token_kind("]", TokenKind::CloseBracket);
        assert_token_kind("-", TokenKind::Minus);
        assert_token_kind("^", TokenKind::Caret);
        assert_token_kind("*", TokenKind::Star);
        assert_token_kind("?", TokenKind::Question);
        assert_token_kind("+", TokenKind::Plus);
        assert_token_kind("$", TokenKind::DollarAnchor);
    }

    #[test]
    fn test_escape_sequences_whitespace() {
        assert_token_kind("\\n", TokenKind::Literal('\n'));
        assert_token_kind("\\t", TokenKind::Literal('\t'));
        assert_token_kind("\\r", TokenKind::Literal('\r'));
        assert_token_kind("\\f", TokenKind::Literal('\x0C'));
        assert_token_kind("\\v", TokenKind::Literal('\x0B'));
        assert_token_kind("\\0", TokenKind::Literal('\0'));
    }

    #[test]
    fn test_escape_sequences_metacharacters() {
        assert_token_kind("\\\\", TokenKind::Literal('\\'));
        assert_token_kind("\\.", TokenKind::Literal('.'));
        assert_token_kind("\\|", TokenKind::Literal('|'));
        assert_token_kind("\\(", TokenKind::Literal('('));
        assert_token_kind("\\)", TokenKind::Literal(')'));
        assert_token_kind("\\{", TokenKind::Literal('{'));
        assert_token_kind("\\}", TokenKind::Literal('}'));
        assert_token_kind("\\[", TokenKind::Literal('['));
        assert_token_kind("\\]", TokenKind::Literal(']'));
        assert_token_kind("\\*", TokenKind::Literal('*'));
        assert_token_kind("\\+", TokenKind::Literal('+'));
        assert_token_kind("\\?", TokenKind::Literal('?'));
        assert_token_kind("\\^", TokenKind::Literal('^'));
        assert_token_kind("\\$", TokenKind::Literal('$'));
        assert_token_kind("\\-", TokenKind::Literal('-'));
    }

    #[test]
    fn test_escape_sequences_quotes() {
        assert_token_kind("\\'", TokenKind::Literal('\''));
        assert_token_kind("\\\"", TokenKind::Literal('"'));
    }

    #[test]
    fn test_character_classes() {
        assert_token_kind("\\d", TokenKind::DigitClass);
        assert_token_kind("\\D", TokenKind::NonDigitClass);
        assert_token_kind("\\w", TokenKind::WordClass);
        assert_token_kind("\\W", TokenKind::NonWordClass);
        assert_token_kind("\\s", TokenKind::SpaceClass);
        assert_token_kind("\\S", TokenKind::NonSpaceClass);
    }

    #[test]
    fn test_hex_escapes() {
        assert_token_kind("\\x41", TokenKind::Literal('A')); // 0x41 = 'A'
        assert_token_kind("\\x20", TokenKind::Literal(' ')); // 0x20 = space
    }

    #[test]
    fn test_octal_escapes() {
        assert_token_kind("\\101", TokenKind::Literal('A')); // 0o101 = 65 = 'A'
        assert_token_kind("\\040", TokenKind::Literal(' ')); // 0o040 = 32 = space
    }

    #[test]
    fn test_unicode_escapes_braces() {
        assert_token_kind("\\u{41}", TokenKind::Literal('A'));
        assert_token_kind("\\u{1F600}", TokenKind::Literal('😀'));
        assert_token_kind("\\u{03B1}", TokenKind::Literal('α')); // Greek alpha
    }

    #[test]
    fn test_unicode_escapes_fixed_width() {
        assert_token_kind("\\u0041", TokenKind::Literal('A'));
        assert_token_kind("\\u03B1", TokenKind::Literal('α')); // Greek alpha
    }

    #[test]
    fn test_unicode_properties() {
        let mut lexer = Lexer::new("\\p{L}");
        let token = lexer.next().unwrap();
        assert!(matches!(token.kind, TokenKind::UnicodeProperty(UnicodePropertyType::Letter)));

        let mut lexer = Lexer::new("\\p{Nd}");
        let token = lexer.next().unwrap();
        assert!(matches!(token.kind, TokenKind::UnicodeProperty(UnicodePropertyType::Digit)));

        let mut lexer = Lexer::new("\\p{Z}");
        let token = lexer.next().unwrap();
        assert!(matches!(token.kind, TokenKind::UnicodeProperty(UnicodePropertyType::Space)));

        let mut lexer = Lexer::new("\\p{Lu}");
        let token = lexer.next().unwrap();
        assert!(matches!(token.kind, TokenKind::UnicodeProperty(UnicodePropertyType::Upper)));

        let mut lexer = Lexer::new("\\p{Ll}");
        let token = lexer.next().unwrap();
        assert!(matches!(token.kind, TokenKind::UnicodeProperty(UnicodePropertyType::Lower)));

        let mut lexer = Lexer::new("\\p{XID_Start}");
        let token = lexer.next().unwrap();
        assert!(matches!(token.kind, TokenKind::UnicodeProperty(UnicodePropertyType::XIDStart)));
    }

    #[test]
    fn test_negated_unicode_properties() {
        let mut lexer = Lexer::new("\\P{L}");
        let token = lexer.next().unwrap();
        assert!(matches!(token.kind, TokenKind::NegatedUnicodeProperty(UnicodePropertyType::Letter)));

        let mut lexer = Lexer::new("\\P{Nd}");
        let token = lexer.next().unwrap();
        assert!(matches!(token.kind, TokenKind::NegatedUnicodeProperty(UnicodePropertyType::Digit)));
    }

    #[test]
    fn test_complex_patterns() {
        // Test pattern: a|b
        assert_tokens("a|b", vec![
            TokenKind::Literal('a'),
            TokenKind::Or,
            TokenKind::Literal('b'),
        ]);

        // Test pattern: (abc)
        assert_tokens("(abc)", vec![
            TokenKind::OpenParen,
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::Literal('c'),
            TokenKind::CloseParen,
        ]);

        // Test pattern: [a-z]
        assert_tokens("[a-z]", vec![
            TokenKind::OpenBracket,
            TokenKind::Literal('a'),
            TokenKind::Minus,
            TokenKind::Literal('z'),
            TokenKind::CloseBracket,
        ]);

        // Test pattern: a*
        assert_tokens("a*", vec![
            TokenKind::Literal('a'),
            TokenKind::Star,
        ]);

        // Test pattern: a+
        assert_tokens("a+", vec![
            TokenKind::Literal('a'),
            TokenKind::Plus,
        ]);

        // Test pattern: a?
        assert_tokens("a?", vec![
            TokenKind::Literal('a'),
            TokenKind::Question,
        ]);
    }

    #[test]
    fn test_complex_regex_patterns() {
        // Email-like pattern: \w+@\w+\.\w+
        assert_tokens("\\w+@\\w+\\.\\w+", vec![
            TokenKind::WordClass,
            TokenKind::Plus,
            TokenKind::Literal('@'),
            TokenKind::WordClass,
            TokenKind::Plus,
            TokenKind::Literal('.'),
            TokenKind::WordClass,
            TokenKind::Plus,
        ]);

        // Anchored pattern: ^abc$
        assert_tokens("^abc$", vec![
            TokenKind::Caret,
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::Literal('c'),
            TokenKind::DollarAnchor,
        ]);

        // Character class with negation: [^a-z]
        assert_tokens("[^a-z]", vec![
            TokenKind::OpenBracket,
            TokenKind::Caret,
            TokenKind::Literal('a'),
            TokenKind::Minus,
            TokenKind::Literal('z'),
            TokenKind::CloseBracket,
        ]);
    }

    #[test]
    fn test_token_spans() {
        let mut lexer = Lexer::new("abc");
        
        let token = lexer.next().unwrap();
        assert_eq!(token.span, (0, 1));
        
        let token = lexer.next().unwrap();
        assert_eq!(token.span, (1, 2));
        
        let token = lexer.next().unwrap();
        assert_eq!(token.span, (2, 3));
    }

    #[test]
    fn test_escaped_character_spans() {
        let mut lexer = Lexer::new("\\n\\t");
        
        let token = lexer.next().unwrap();
        assert_eq!(token.span, (0, 2)); // \n takes 2 bytes
        
        let token = lexer.next().unwrap();
        assert_eq!(token.span, (2, 4)); // \t takes 2 bytes
    }

    #[test]
    fn test_quantifier_spans() {
        let mut lexer = Lexer::new("a{3,5}b");
        
        let token = lexer.next().unwrap(); // 'a'
        assert_eq!(token.span, (0, 1));
        
        let token = lexer.next().unwrap(); // {3,5}
        assert_eq!(token.span, (1, 6));
        
        let token = lexer.next().unwrap(); // 'b'
        assert_eq!(token.span, (6, 7));
    }

    #[test]
    fn test_empty_input() {
        let mut lexer = Lexer::new("");
        assert!(lexer.next().is_none());
    }

    #[test]
    fn test_unicode_multibyte() {
        // Test emoji
        let mut lexer = Lexer::new("😀");
        let token = lexer.next().unwrap();
        assert!(matches!(token.kind, TokenKind::Literal('😀')));
        
        // Test Chinese character
        let mut lexer = Lexer::new("中");
        let token = lexer.next().unwrap();
        assert!(matches!(token.kind, TokenKind::Literal('中')));
    }

    #[test]
    fn test_iterator_implementation() {
        let lexer = Lexer::new("a+b");
        let tokens: Vec<_> = lexer.collect();
        assert_eq!(tokens.len(), 3);
    }

    #[test]
    fn test_invalid_escape_defaults_to_literal() {
        // Unknown escape sequences should be treated as literals
        let mut lexer = Lexer::new("\\z");
        let token = lexer.next().unwrap();
        assert!(matches!(token.kind, TokenKind::Literal('z')));
    }
}
