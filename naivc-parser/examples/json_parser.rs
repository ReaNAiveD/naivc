//! JSON Parser Example
//!
//! This example demonstrates how to use naivc-derive to create a lexer
//! and naivc-parser to build an LR parser for JSON.

use naivc_derive::Lexer;
use naivc_lexer::token::Token;
use naivc_lexer::Lexer;
use naivc_parser::lr::{PlainLRTableParser, TokenTree};
use naivc_parser::symbol::{
    ContextFreeGrammar, NonTerminal, NonTerminalHandle, Production, SymbolHandle, TerminalHandle,
};

/// JSON Lexer using derive macro
#[derive(Debug, Clone, PartialEq, Eq, Hash, Lexer)]
enum JsonToken {
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

/// Build the JSON grammar
///
/// JSON Grammar (simplified for LR(1) parsing):
/// ```text
/// Value       -> Object | Array | String | Number | True | False | Null
/// Object      -> LeftBrace RightBrace | LeftBrace Members RightBrace
/// Members     -> Pair | Members Comma Pair
/// Pair        -> String Colon Value
/// Array       -> LeftBracket RightBracket | LeftBracket Elements RightBracket
/// Elements    -> Value | Elements Comma Value
/// ```
fn build_json_grammar() -> ContextFreeGrammar<JsonToken> {
    // Terminal indices (must match the order in terminals vec)
    const T_LEFT_BRACE: usize = 0;
    const T_RIGHT_BRACE: usize = 1;
    const T_LEFT_BRACKET: usize = 2;
    const T_RIGHT_BRACKET: usize = 3;
    const T_COLON: usize = 4;
    const T_COMMA: usize = 5;
    const T_STRING: usize = 6;
    const T_NUMBER: usize = 7;
    const T_TRUE: usize = 8;
    const T_FALSE: usize = 9;
    const T_NULL: usize = 10;

    // Non-terminal indices
    const NT_VALUE: usize = 0;
    const NT_OBJECT: usize = 1;
    const NT_MEMBERS: usize = 2;
    const NT_PAIR: usize = 3;
    const NT_ARRAY: usize = 4;
    const NT_ELEMENTS: usize = 5;

    // Helper to create terminal symbol handle
    let t = |idx: usize| SymbolHandle::Terminal(TerminalHandle::new(idx));
    // Helper to create non-terminal symbol handle
    let nt = |idx: usize| SymbolHandle::NonTerminal(NonTerminalHandle::new(idx));

    // Define terminals (must match JsonToken enum order, excluding Whitespace)
    let terminals = vec![
        JsonToken::LeftBrace,
        JsonToken::RightBrace,
        JsonToken::LeftBracket,
        JsonToken::RightBracket,
        JsonToken::Colon,
        JsonToken::Comma,
        JsonToken::String,
        JsonToken::Number,
        JsonToken::True,
        JsonToken::False,
        JsonToken::Null,
    ];

    // Define non-terminals with their productions
    let non_terminals = vec![
        // Value -> Object | Array | String | Number | True | False | Null
        NonTerminal {
            name: "Value".to_string(),
            productions: vec![
                Production {
                    symbols: vec![nt(NT_OBJECT)],
                },
                Production {
                    symbols: vec![nt(NT_ARRAY)],
                },
                Production {
                    symbols: vec![t(T_STRING)],
                },
                Production {
                    symbols: vec![t(T_NUMBER)],
                },
                Production {
                    symbols: vec![t(T_TRUE)],
                },
                Production {
                    symbols: vec![t(T_FALSE)],
                },
                Production {
                    symbols: vec![t(T_NULL)],
                },
            ],
        },
        // Object -> LeftBrace RightBrace | LeftBrace Members RightBrace
        NonTerminal {
            name: "Object".to_string(),
            productions: vec![
                Production {
                    symbols: vec![t(T_LEFT_BRACE), t(T_RIGHT_BRACE)],
                },
                Production {
                    symbols: vec![t(T_LEFT_BRACE), nt(NT_MEMBERS), t(T_RIGHT_BRACE)],
                },
            ],
        },
        // Members -> Pair | Members Comma Pair
        NonTerminal {
            name: "Members".to_string(),
            productions: vec![
                Production {
                    symbols: vec![nt(NT_PAIR)],
                },
                Production {
                    symbols: vec![nt(NT_MEMBERS), t(T_COMMA), nt(NT_PAIR)],
                },
            ],
        },
        // Pair -> String Colon Value
        NonTerminal {
            name: "Pair".to_string(),
            productions: vec![Production {
                symbols: vec![t(T_STRING), t(T_COLON), nt(NT_VALUE)],
            }],
        },
        // Array -> LeftBracket RightBracket | LeftBracket Elements RightBracket
        NonTerminal {
            name: "Array".to_string(),
            productions: vec![
                Production {
                    symbols: vec![t(T_LEFT_BRACKET), t(T_RIGHT_BRACKET)],
                },
                Production {
                    symbols: vec![t(T_LEFT_BRACKET), nt(NT_ELEMENTS), t(T_RIGHT_BRACKET)],
                },
            ],
        },
        // Elements -> Value | Elements Comma Value
        NonTerminal {
            name: "Elements".to_string(),
            productions: vec![
                Production {
                    symbols: vec![nt(NT_VALUE)],
                },
                Production {
                    symbols: vec![nt(NT_ELEMENTS), t(T_COMMA), nt(NT_VALUE)],
                },
            ],
        },
    ];

    // Root is Value (index 0)
    ContextFreeGrammar::new(NonTerminalHandle::new(NT_VALUE), terminals, non_terminals)
}

/// Pretty print the parse tree
fn print_tree<Token: std::fmt::Debug + Clone + Eq + std::hash::Hash>(
    tree: &TokenTree<Token>,
    indent: usize,
) {
    let prefix = "  ".repeat(indent);
    match tree {
        TokenTree::Leaf(token) => {
            println!("{}Leaf: {:?}", prefix, token);
        }
        TokenTree::Node {
            non_terminal_name,
            children,
        } => {
            println!("{}Node: {}", prefix, non_terminal_name);
            for child in children {
                print_tree(child, indent + 1);
            }
        }
        TokenTree::Error {
            found,
            potential_tokens,
            skipped,
        } => {
            println!(
                "{}Error: found={:?}, potential={:?}, skipped={:?}",
                prefix, found, potential_tokens, skipped
            );
        }
    }
}

fn main() {
    // Sample JSON inputs to parse
    let simple_json = r#"{"name": "Alice", "age": 30}"#;
    let array_json = r#"[1, 2, 3, "four", true, null]"#;
    let nested_json = r#"{"user": {"name": "Bob", "scores": [100, 95, 88]}}"#;

    // Build the grammar and parser
    println!("Building JSON grammar and LR parser...");
    let grammar = build_json_grammar();
    let parser = PlainLRTableParser::new(&grammar);
    println!("Parser built successfully!\n");

    // Parse each example
    for (name, input) in [
        ("Simple object", simple_json),
        ("Array", array_json),
        ("Nested structure", nested_json),
    ] {
        println!("=== {} ===", name);
        println!("Input: {}", input);

        // Tokenize and filter out whitespace
        let tokens: Vec<Token<JsonToken>> = JsonToken::tokenize(input.chars())
            .filter_map(|result| match result {
                Ok(token) if token.kind != JsonToken::Whitespace => Some(token),
                Ok(_) => None, // Skip whitespace
                Err(e) => {
                    eprintln!("Lexer error: {:?}", e);
                    None
                }
            })
            .collect();

        println!("\nTokens (excluding whitespace):");
        for token in &tokens {
            println!("  {:?}: {:?}", token.kind, token.lexeme);
        }

        // Parse the tokens
        println!("\nParse tree:");
        let tree = parser.parse(&tokens);
        print_tree(&tree, 1);
        println!();
    }
}
