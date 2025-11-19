// use naivc_derive::Lexer;

use naivc_lexer::Lexer;

// #[derive(Lexer)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum JsonLexer {
//     #[regex("{")]
    LeftBrace,
//     #[regex("}")]
    RightBrace,
//     #[regex("[")]
    LeftBracket,
//     #[regex("]")]
    RightBracket,
//     #[regex(":")]
    Colon,
//     #[regex(",")]
    Comma,
//     #[regex(r#""([^"\\]|\\.)*""#)]
    String,
//     #[regex(r"-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?")]
    Number,
//     #[regex("true")]
    True,
//     #[regex("false")]
    False,
//     #[regex("null")]
    Null,
//     #[regex(r"[ \t\n\r]+", skip = true)]
    Whitespace,
}

impl Lexer for JsonLexer {
    fn tokenize<I>(
        chars: I,
    ) -> impl Iterator<Item = Result<naivc_lexer::Token<Self>, naivc_lexer::UmatchedError>>
    where I: Iterator<Item = char> {
        LexerIter::new(chars)
    }
}

struct LexerIter<I: Iterator<Item = char>> {
    chars: I,
    buffer: Vec<char>,
    next_index: usize,
}

impl<I: Iterator<Item = char>> LexerIter<I> {
    fn new(chars: I) -> Self {
        Self {
            chars,
            buffer: Vec::new(),
            next_index: 0,
        }
    }

    fn update_buffer(&mut self, mut unidentified: Vec<char>, buf_index: usize) {
        if buf_index < self.buffer.len() {
            unidentified.extend_from_slice(&self.buffer[buf_index..]);
        }
        self.buffer = unidentified;
    }

    fn finalize_token(
        &mut self,
        last_accept: Option<naivc_lexer::Token<JsonLexer>>,
        unidentified: Vec<char>,
        buf_index: usize,
        start_index: usize,
    ) -> Option<Result<naivc_lexer::Token<JsonLexer>, naivc_lexer::UmatchedError>> {
        if let Some(accept) = last_accept {
            // Successfully matched a token
            self.update_buffer(unidentified, buf_index);
            Some(Ok(accept))
        } else {
            // No match found, return error
            let error_content: String = unidentified.into_iter().collect();
            let error_len = error_content.len();
            self.next_index += error_len;
            self.update_buffer(Vec::new(), buf_index);
            Some(Err(naivc_lexer::UmatchedError {
                content: error_content,
                start_index,
                end_index: self.next_index,
            }))
        }
    }

    fn finalize_end_of_input(
        &mut self,
        last_accept: Option<naivc_lexer::Token<JsonLexer>>,
        unidentified: Vec<char>,
        start_index: usize,
    ) -> Option<Result<naivc_lexer::Token<JsonLexer>, naivc_lexer::UmatchedError>> {
        if let Some(accept) = last_accept {
            // Return the last accepted token
            self.buffer = unidentified;
            Some(Ok(accept))
        } else if !unidentified.is_empty() {
            // Return unmatched content as error
            let error_content: String = unidentified.into_iter().collect();
            let error_len = error_content.len();
            self.next_index += error_len;
            self.buffer.clear();
            Some(Err(naivc_lexer::UmatchedError {
                content: error_content,
                start_index,
                end_index: self.next_index,
            }))
        } else {
            // Nothing left to process
            None
        }
    }
}

impl<'a, I: Iterator<Item = char>> Iterator for LexerIter<I> {
    type Item = Result<naivc_lexer::Token<JsonLexer>, naivc_lexer::UmatchedError>;

    fn next(&mut self) -> Option<Self::Item> {
        // Lexer logic would go here
        let start_index = self.next_index;
        let mut current_state = DFAState::start();
        let mut last_accept: Option<naivc_lexer::Token<JsonLexer>> = None;
        let mut buf_index = 0;
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

                    if let Some(next_state) = current_state.on(ch) {
                        // Continue DFA traversal
                        current_state = next_state;
                        if let Some(token_type) = next_state.accept() {
                            self.next_index += unidentified.len();
                            let token = if let Some(mut token) = last_accept {
                                token.lexeme.extend(unidentified.drain(..));
                                token.end_index = self.next_index;
                                token
                            } else {
                                naivc_lexer::Token {
                                    kind: token_type,
                                    lexeme: unidentified.drain(..).collect(),
                                    start_index,
                                    end_index: self.next_index,
                                }
                            };
                            last_accept = Some(token);
                        }
                    } else {
                        // DFA cannot continue, finalize current token or error
                        return self.finalize_token(
                            last_accept,
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
                        unidentified,
                        start_index,
                    );
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum DFAState {
    State0,
    State1,
    State2,
    State3,
    State4,
    State5,
    State6,
    State7,
    State8,
    State9,
    State10,
    State11,
    State12,
    State13,
    State14,
    State15,
    State16,
    State17,
    State18,
    State19,
    State20,
    State21,
    State22,
    State23,
    State24,
    State25,
    State26,
    State27,
    State28,
    State29,
    State30,
    State31,
}

impl DFAState {
    pub fn start() -> Self {
        DFAState::State0
    }

    pub fn on(&self, c: char) -> Option<DFAState> {
        match self {
            DFAState::State0 => match c {
                '\t'..='\n' => Some(Self::State1),
                '\r' => Some(Self::State1),
                ' ' => Some(Self::State1),
                '\"' => Some(Self::State2),
                ',' => Some(Self::State3),
                '-' => Some(Self::State4),
                '0' => Some(Self::State5),
                '1'..='9' => Some(Self::State6),
                ':' => Some(Self::State7),
                '[' => Some(Self::State8),
                ']' => Some(Self::State9),
                'f' => Some(Self::State10),
                'n' => Some(Self::State11),
                't' => Some(Self::State12),
                '{' => Some(Self::State13),
                '}' => Some(Self::State14),
                _ => None,
            },
            DFAState::State1 => {
                match c {
                    '\t'..='\n' => Some(Self::State1),
                    '\r' => Some(Self::State1),
                    ' ' => Some(Self::State1),
                    _ => None,
                }
            },
            DFAState::State2 => {
                match c {
                    '\0'..='\t' => Some(Self::State2),
                    '\n' => Some(Self::State2),
                    '\u{b}'..='\u{c}' => Some(Self::State2),
                    '\r' => Some(Self::State2),
                    '\u{e}'..='!' => Some(Self::State2),
                    '"' => Some(Self::State30),
                    '#'..='[' => Some(Self::State2),
                    '\\' => Some(Self::State31),
                    ']'..='\u{84}' => Some(Self::State2),
                    '\u{85}' => Some(Self::State2),
                    '\u{86}'..='‧' => Some(Self::State2),
                    '\u{2028}'..='\u{2029}' => Some(Self::State2),
                    '\u{202a}'..='\u{10ffff}' => Some(Self::State2),
                    _ => None,
                }
            },
            DFAState::State3 => None,
            DFAState::State4 => {
                match c {
                    '0' => Some(Self::State5),
                    '1'..='9' => Some(Self::State6),
                    _ => None,
                }
            },
            DFAState::State5 => {
                match c {
                    '.' => Some(Self::State25),
                    'E' => Some(Self::State26),
                    'e' => Some(Self::State26),
                    _ => None,
                }
            },
            DFAState::State6 => {
                match c {
                    '.' => Some(Self::State25),
                    '0' => Some(Self::State6),
                    '1'..='9' => Some(Self::State6),
                    'E' => Some(Self::State26),
                    'e' => Some(Self::State26),
                    _ => None,
                }
            },
            DFAState::State7 => None,
            DFAState::State8 => None,
            DFAState::State9 => None,
            DFAState::State10 => {
                match c {
                    'a' => Some(Self::State21),
                    _ => None,
                }
            },
            DFAState::State11 => {
                match c {
                    'u' => Some(Self::State18),
                    _ => None,
                }
            },
            DFAState::State12 => {
                match c {
                    'r' => Some(Self::State15),
                    _ => None,
                }
            },
            DFAState::State13 => None,
            DFAState::State14 => None,
            DFAState::State15 => {
                match c {
                    'u' => Some(Self::State16),
                    _ => None,
                }
            },
            DFAState::State16 => {
                match c {
                    'e' => Some(Self::State17),
                    _ => None,
                }
            },
            DFAState::State17 => None,
            DFAState::State18 => {
                match c {
                    'l' => Some(Self::State19),
                    _ => None,
                }
            },
            DFAState::State19 => {
                match c {
                    'l' => Some(Self::State20),
                    _ => None,
                }
            },
            DFAState::State20 => None,
            DFAState::State21 => {
                match c {
                    'l' => Some(Self::State22),
                    _ => None,
                }
            },
            DFAState::State22 => {
                match c {
                    's' => Some(Self::State23),
                    _ => None,
                }
            },
            DFAState::State23 => {
                match c {
                    'e' => Some(Self::State24),
                    _ => None,
                }
            },
            DFAState::State24 => None,
            DFAState::State25 => {
                match c {
                    '0' => Some(Self::State29),
                    '1'..='9' => Some(Self::State29),
                    _ => None,
                }
            },
            DFAState::State26 => {
                match c {
                    '+' => Some(Self::State27),
                    '-' => Some(Self::State27),
                    '0' => Some(Self::State28),
                    '1'..='9' => Some(Self::State28),
                    _ => None,
                }
            },
            DFAState::State27 => {
                match c {
                    '0' => Some(Self::State28),
                    '1'..='9' => Some(Self::State28),
                    _ => None,
                }
            },
            DFAState::State28 => {
                match c {
                    '0' => Some(Self::State28),
                    '1'..='9' => Some(Self::State28),
                    _ => None,
                }
            },
            DFAState::State29 => {
                match c {
                    '0' => Some(Self::State29),
                    '1'..='9' => Some(Self::State29),
                    'E' => Some(Self::State26),
                    'e' => Some(Self::State26),
                    _ => None,
                }
            },
            DFAState::State30 => None,
            DFAState::State31 => {
                match c {
                    '\0'..='\t' => Some(Self::State2),
                    '\u{b}'..='\u{c}' => Some(Self::State2),
                    '\u{e}'..='!' => Some(Self::State2),
                    '"' => Some(Self::State2),
                    '#'..='[' => Some(Self::State2),
                    '\\' => Some(Self::State2),
                    ']'..='\u{84}' => Some(Self::State2),
                    '\u{86}'..='‧' => Some(Self::State2),
                    '\u{202a}'..='\u{10ffff}' => Some(Self::State2),
                    _ => None,
                }
            },
        }
    }

    pub fn accept(&self) -> Option<JsonLexer> {
        match self {
            DFAState::State1 => Some(JsonLexer::Whitespace),
            DFAState::State3 => Some(JsonLexer::Comma),
            DFAState::State5 => Some(JsonLexer::Number),
            DFAState::State6 => Some(JsonLexer::Number),
            DFAState::State7 => Some(JsonLexer::Colon),
            DFAState::State8 => Some(JsonLexer::LeftBracket),
            DFAState::State9 => Some(JsonLexer::RightBracket),
            DFAState::State13 => Some(JsonLexer::LeftBrace),
            DFAState::State14 => Some(JsonLexer::RightBrace),
            DFAState::State17 => Some(JsonLexer::True),
            DFAState::State20 => Some(JsonLexer::Null),
            DFAState::State24 => Some(JsonLexer::False),
            DFAState::State28 => Some(JsonLexer::Number),
            DFAState::State29 => Some(JsonLexer::Number),
            DFAState::State30 => Some(JsonLexer::String),
            _ => None,
        }
    }
}

fn load_example_json() -> &'static str {
    r#"
    {
        "basicTypes": {
            "string": "Hello, World!",
            "number": 42,
            "float": 3.14159,
            "negative": -123,
            "negativeFloat": -45.67,
            "boolean_true": true,
            "boolean_false": false,
            "null_value": null
        },
        "strings": {
            "empty": "",
            "simple": "test",
            "withSpaces": "hello world",
            "escaped": "Line1\nLine2\tTabbed",
            "quotes": "He said \"Hello\"",
            "backslash": "Path: C:\\Users\\test",
            "unicode": "emoji 😀 and unicode \u0048\u0065\u006C\u006C\u006F",
            "specialChars": "Special: !@#$%^&*()_+-=[]{}|;:',.<>?/`~"
        },
        "numbers": {
            "zero": 0,
            "positive": 12345,
            "negative": -67890,
            "decimal": 123.456,
            "negativeDecimal": -789.012,
            "scientific": 1.23e10,
            "scientificNegative": -4.56e-7,
            "scientificPositiveExp": 7.89E+12,
            "smallDecimal": 0.0001,
            "largeNumber": 9999999999999999,
            "fractionOnly": 0.5
        },
        "arrays": {
            "empty": [],
            "numbers": [1, 2, 3, 4, 5],
            "strings": ["apple", "banana", "cherry"],
            "mixed": [1, "two", 3.0, true, false, null],
            "nested": [
            [1, 2],
            [3, 4],
            [5, 6]
            ],
            "deeplyNested": [[[[[1]]]]]
        },
        "objects": {
            "empty": {},
            "simple": {
            "key": "value"
            },
            "nested": {
            "outer": {
                "inner": {
                "deep": "value"
                }
            }
            },
            "multiple": {
            "first": 1,
            "second": 2,
            "third": 3
            }
        },
        "complexStructures": {
            "arrayOfObjects": [
            {
                "id": 1,
                "name": "Alice",
                "active": true
            },
            {
                "id": 2,
                "name": "Bob",
                "active": false
            },
            {
                "id": 3,
                "name": "Charlie",
                "active": true
            }
            ],
            "objectWithArrays": {
            "tags": ["json", "test", "lexer"],
            "numbers": [1, 2, 3],
            "booleans": [true, false, true]
            },
            "mixedNesting": {
            "level1": {
                "array": [
                {
                    "nested": [1, 2, 3]
                },
                {
                    "nested": [4, 5, 6]
                }
                ]
            }
            }
        },
        "edgeCases": {
            "emptyString": "",
            "singleChar": "x",
            "justZero": 0,
            "justTrue": true,
            "justFalse": false,
            "justNull": null,
            "manyCommas": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
            "manyColons": {
            "a": 1,
            "b": 2,
            "c": 3,
            "d": 4,
            "e": 5
            }
        },
        "whitespace": {
            "normal": "with spaces",
            "value": 123
        },
        "realWorldExample": {
            "user": {
            "id": 12345,
            "username": "john_doe",
            "email": "john@example.com",
            "profile": {
                "firstName": "John",
                "lastName": "Doe",
                "age": 30,
                "address": {
                "street": "123 Main St",
                "city": "Anytown",
                "state": "CA",
                "zip": "12345",
                "country": "USA"
                },
                "phoneNumbers": [
                {
                    "type": "home",
                    "number": "555-1234"
                },
                {
                    "type": "work",
                    "number": "555-5678"
                }
                ]
            },
            "preferences": {
                "theme": "dark",
                "notifications": true,
                "language": "en-US"
            },
            "metadata": {
                "createdAt": "2023-01-15T10:30:00Z",
                "lastLogin": "2024-11-17T08:45:23Z",
                "loginCount": 142,
                "isVerified": true,
                "roles": ["user", "premium", "beta-tester"]
            }
            }
        },
        "specialNumbers": {
            "verySmall": 1e-10,
            "veryLarge": 1e100,
            "preciseDecimal": 0.123456789012345,
            "negativeScientific": -3.14159e-5,
            "positiveScientific": 6.022e23
        },
        "unicodeAndEscapes": {
            "tab": "before\tafter",
            "newline": "line1\nline2",
            "carriageReturn": "before\rafter",
            "backspace": "test\bbackspace",
            "formfeed": "test\fformfeed",
            "mixed": "tab:\t newline:\n quote:\" backslash:\\"
        },
        "boundaryTests": {
            "lastItem": "This is the last top-level item in the JSON object"
        }
    }
    "#
}


fn main()  {
    let json_input = load_example_json();
    let tokens = JsonLexer::tokenize(json_input.chars());
    println!("Tokens:");
    for token in tokens {
        println!("{:?}", token);
    }
}