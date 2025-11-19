use naivc_derive::Lexer;
use naivc_lexer::Lexer;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Lexer)]
enum JsonLexer {
    #[regex(r"\{")]
    LeftBrace,
    #[regex(r"\}")]
    RightBrace,
    #[regex(r"\[")]
    LeftBracket,
    #[regex(r"\]")]
    RightBracket,
    #[regex(":")]
    Colon,
    #[regex(",")]
    Comma,
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,
    #[regex(r"-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?")]
    Number,
    #[regex("true")]
    True,
    #[regex("false")]
    False,
    #[regex("null")]
    Null,
    #[regex(r"[ \t\n\r]+")]
    Whitespace,
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