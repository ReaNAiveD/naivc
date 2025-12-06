use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::{any::TypeId, collections::HashSet};

use naivc_derive::Lexer;
use naivc_lexer::{Lexer, Token};
use naivc_parser::lr::{PlainLRTableParser, SyntaxTree};
use naivc_parser::symbol::{ContextFreeGrammar, NonTerminalHandle, SymbolHandle, TerminalHandle, TokenType};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SymbolId<TTerminal>
where
    TTerminal: Debug + Clone + PartialEq + Eq + Hash,
{
    Terminal(TTerminal),
    NonTerminal(TypeId),
}

/// A single production rule: NonTerminal -> [symbols...]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProductionRule<TTerminal>
where
    TTerminal: Debug + Clone + PartialEq + Eq + Hash,
{
    /// The left-hand side (the non-terminal being defined)
    pub lhs: TypeId,
    /// The right-hand side symbols
    pub rhs: Vec<SymbolId<TTerminal>>,
}

/// Collected grammar information from typed symbols
#[derive(Debug, Clone)]
pub struct GrammarInfo<TTerminal>
where
    TTerminal: Debug + Clone + PartialEq + Eq + Hash,
{
    /// All symbols in the grammar
    pub symbols: HashSet<SymbolId<TTerminal>>,
    /// All productions, grouped by non-terminal TypeId
    pub productions: HashMap<TypeId, Vec<Vec<SymbolId<TTerminal>>>>,
}

impl<TTerminal> GrammarInfo<TTerminal>
where
    TTerminal: Debug + Clone + PartialEq + Eq + Hash,
{
    pub fn new() -> Self {
        Self {
            symbols: HashSet::new(),
            productions: HashMap::new(),
        }
    }
}

pub trait TypedSymbol: 'static {
    type Terminal: Debug + Clone + PartialEq + Eq + Hash;

    fn terminal_token_type() -> Option<Self::Terminal> {
        None
    }

    fn direct_terminals() -> Vec<Self::Terminal> {
        vec![]
    }

    /// Returns the productions for this non-terminal symbol.
    /// Each inner Vec represents one production's RHS.
    /// Terminal symbols return an empty vec.
    fn productions() -> Vec<Vec<SymbolId<Self::Terminal>>>;

    /// Returns collector functions for each direct non-terminal dependency
    fn typed_dependencies() -> Vec<fn(&mut GrammarInfo<Self::Terminal>)>;

    // Default implementations for symbol operations (formerly in ParserSymbol)

    fn symbol_id() -> SymbolId<Self::Terminal> {
        Self::terminal_token_type().map_or(SymbolId::NonTerminal(TypeId::of::<Self>()), |t| {
            SymbolId::Terminal(t)
        })
    }

    /// Collects all symbols AND productions recursively
    fn collect_grammar(grammar: &mut GrammarInfo<Self::Terminal>) {
        let self_id = Self::symbol_id();
        
        // Check if already visited
        if !grammar.symbols.insert(self_id.clone()) {
            return;
        }

        // Collect direct terminal dependencies
        for terminal in Self::direct_terminals() {
            grammar.symbols.insert(SymbolId::Terminal(terminal));
        }

        // If this is a non-terminal, collect its productions
        if let SymbolId::NonTerminal(type_id) = &self_id {
            let prods = Self::productions();
            if !prods.is_empty() {
                grammar.productions.insert(*type_id, prods);
            }
        }

        // Recurse into typed dependencies
        for collector in Self::typed_dependencies() {
            collector(grammar);
        }
    }

    fn from_syntax_tree(syntax_tree: &SyntaxTree<'_, Token<Self::Terminal>>) -> Self
    where
        Self: Sized;
}

pub trait TerminalSymbol: 'static {
    type Terminal: Debug + Clone + PartialEq + Eq + Hash;

    const TOKEN_TYPE: Self::Terminal;

    fn from_token(token: &Token<Self::Terminal>) -> Self;
}

impl<T> TypedSymbol for T
where
    T: TerminalSymbol,
{
    type Terminal = T::Terminal;

    fn terminal_token_type() -> Option<Self::Terminal> {
        Some(T::TOKEN_TYPE)
    }

    fn direct_terminals() -> Vec<Self::Terminal> {
        vec![]
    }

    fn typed_dependencies() -> Vec<fn(&mut GrammarInfo<Self::Terminal>)> {
        vec![]
    }
    
    /// Terminal symbols return an empty vec.
    fn productions() -> Vec<Vec<SymbolId<Self::Terminal>>> {
        vec![]
    }

    fn from_syntax_tree(tt: &SyntaxTree<'_, Token<Self::Terminal>>) -> Self
    where
        Self: Sized,
    {
        match tt {
            SyntaxTree::Leaf(token) => T::from_token(token),
            _ => panic!("Expected leaf node for terminal symbol"),
        }
    }
}

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

#[derive(Debug, Clone)]
enum Value {
    Object(Box<Object>),
    Array(Box<Array>),
    String(JsonString),
    // #[Terminal(JsonLexer::Number)]
    Number(Box<Token<JsonLexer>>),
    // #[Terminal(JsonLexer::True)]
    True,
    // #[Terminal(JsonLexer::False)]
    False,
    // #[Terminal(JsonLexer::Null)]
    Null,
}

impl TypedSymbol for Value {
    type Terminal = JsonLexer;

    fn direct_terminals() -> Vec<Self::Terminal> {
        vec![
            JsonLexer::Number,
            JsonLexer::True,
            JsonLexer::False,
            JsonLexer::Null,
        ]
    }

    fn typed_dependencies() -> Vec<fn(&mut GrammarInfo<Self::Terminal>)> {
        vec![
            Object::collect_grammar,
            Array::collect_grammar,
            JsonString::collect_grammar,
        ]
    }

    fn productions() -> Vec<Vec<SymbolId<JsonLexer>>> {
        vec![
            // Value -> Object
            vec![Object::symbol_id()],
            // Value -> Array
            vec![Array::symbol_id()],
            // Value -> String
            vec![JsonString::symbol_id()],
            // Value -> Number
            vec![SymbolId::Terminal(JsonLexer::Number)],
            // Value -> True
            vec![SymbolId::Terminal(JsonLexer::True)],
            // Value -> False
            vec![SymbolId::Terminal(JsonLexer::False)],
            // Value -> Null
            vec![SymbolId::Terminal(JsonLexer::Null)],
        ]
    }

    fn from_syntax_tree(syntax_tree: &SyntaxTree<'_, Token<Self::Terminal>>) -> Self
    where
        Self: Sized,
    {
        match syntax_tree {
            SyntaxTree::Node {
                production_handle,
                children,
            } => {
                match production_handle.production_idx() {
                    0 => {
                        // Value -> Object
                        let object_tree = &children[0];
                        let object = Object::from_syntax_tree(object_tree);
                        Value::Object(Box::new(object))
                    }
                    1 => {
                        // Value -> Array
                        let array_tree = &children[0];
                        let array = Array::from_syntax_tree(array_tree);
                        Value::Array(Box::new(array))
                    }
                    2 => {
                        // Value -> String
                        let string_tree = &children[0];
                        let string = JsonString::from_syntax_tree(string_tree);
                        Value::String(string)
                    }
                    3 => {
                        // Value -> Number
                        let number_token = &children[0];
                        if let SyntaxTree::Leaf(token) = number_token {
                            Value::Number(Box::new((*token).clone()))
                        } else {
                            panic!("Expected leaf node for Number token");
                        }
                    }
                    4 => Value::True,
                    5 => Value::False,
                    6 => Value::Null,
                    _ => panic!("Unknown production index for Value"),
                }
            },
            _ => panic!("Expected node for non-terminal Value"),
        }
    }
}

impl Value {
    pub fn grammar() -> GrammarInfo<JsonLexer> {
        let mut visited = GrammarInfo::new();
        Self::collect_grammar(&mut visited);
        visited
    }

    pub fn cfg() -> ContextFreeGrammar<JsonLexer> {
        let grammar_info = Self::grammar();
        let mut terminals = vec![];
        let mut non_terminals = vec![];
        let mut handler_map = HashMap::new();
        for symbol in &grammar_info.symbols {
            match symbol {
                SymbolId::Terminal(t) => {
                    if !handler_map.contains_key(symbol) {
                        let handle = TerminalHandle::new(terminals.len());
                        terminals.push(t.clone());
                        handler_map.insert(symbol.clone(), SymbolHandle::Terminal(handle));
                    }
                },
                SymbolId::NonTerminal(type_id) => {
                    if !handler_map.contains_key(symbol) {
                        let handle = NonTerminalHandle::new(non_terminals.len());
                        non_terminals.push(naivc_parser::symbol::NonTerminal {
                            productions: vec![],
                        });
                        handler_map.insert(symbol.clone(), SymbolHandle::NonTerminal(handle));
                    }
                }
            }
        }
        for (type_id, prods) in &grammar_info.productions {
            let symbol_id = SymbolId::NonTerminal(*type_id);
            let symbol_handle = handler_map.get(&symbol_id).expect("Symbol handle not found");
            if let SymbolHandle::NonTerminal(nt_handle) = symbol_handle {
                let nt = non_terminals.get_mut(nt_handle.idx()).expect("Non-terminal not found");
                for prod in prods {
                    let mut production_symbols = vec![];
                    for sym_id in prod {
                        let sym_handle = handler_map.get(sym_id).expect("Symbol handle not found");
                        production_symbols.push(sym_handle.clone());
                    }
                    nt.productions.push(naivc_parser::symbol::Production {
                        symbols: production_symbols,
                    });
                }
            }
        }
        let root = handler_map.get(&Self::symbol_id()).expect("Root symbol handle not found");
        let root_handle = match root {
            SymbolHandle::NonTerminal(nt_handle) => *nt_handle,
            _ => panic!("Root symbol is not a non-terminal"),
        };
        ContextFreeGrammar {
            root: root_handle,
            terminals,
            non_terminals,
        }
    }

    pub fn parser() -> PlainLRTableParser<JsonLexer> {
        let cfg = Self::cfg();
        PlainLRTableParser::new(cfg)        
    }
}

pub struct ValueParser {
    parser: PlainLRTableParser<JsonLexer>,
}

impl ValueParser {
    pub fn new() -> Self {
        Self {
            parser: Value::parser(),
        }
    }

    fn parse(&self, tokens: &[Token<JsonLexer>]) -> Value {
        let syntax_tree = self.parser.parse(tokens);
        Value::from_syntax_tree(&syntax_tree)
    }
}

#[derive(Debug, Clone)]
enum Object {
    Empty {
        // #[Terminal(JsonLexer::LeftBrace)]
        left_brace: (),
        // #[Terminal(JsonLexer::RightBrace)]
        right_brace: (),
    },
    Members {
        // #[Terminal(JsonLexer::LeftBrace)]
        left_brace: (),
        members: Box<Members>,
        // #[Terminal(JsonLexer::RightBrace)]
        right_brace: (),
    },
}

impl TypedSymbol for Object {
    type Terminal = JsonLexer;

    fn direct_terminals() -> Vec<Self::Terminal> {
        vec![JsonLexer::LeftBrace, JsonLexer::RightBrace]
    }

    fn typed_dependencies() -> Vec<fn(&mut GrammarInfo<Self::Terminal>)> {
        vec![Members::collect_grammar]
    }

    fn productions() -> Vec<Vec<SymbolId<Self::Terminal>>> {
        vec![
            // Object -> LeftBrace RightBrace
            vec![
                SymbolId::Terminal(JsonLexer::LeftBrace),
                SymbolId::Terminal(JsonLexer::RightBrace),
            ],
            // Object -> LeftBrace Members RightBrace
            vec![
                SymbolId::Terminal(JsonLexer::LeftBrace),
                Members::symbol_id(),
                SymbolId::Terminal(JsonLexer::RightBrace),
            ],
        ]
    }

    fn from_syntax_tree(syntax_tree: &SyntaxTree<'_, Token<Self::Terminal>>) -> Self
    where
        Self: Sized,
    {
        match syntax_tree {
            SyntaxTree::Node {
                production_handle,
                children,
            } => {
                match production_handle.production_idx() {
                    0 => {
                        // Object -> LeftBrace RightBrace
                        Object::Empty {
                            left_brace: (),
                            right_brace: (),
                        }
                    }
                    1 => {
                        // Object -> LeftBrace Members RightBrace
                        let members_tree = &children[1];
                        let members = Members::from_syntax_tree(members_tree);
                        Object::Members {
                            left_brace: (),
                            members: Box::new(members),
                            right_brace: (),
                        }
                    }
                    _ => panic!("Unknown production index for Object"),
                }
            },
            _ => panic!("Expected node for non-terminal Object"),
        }
    }
}

#[derive(Debug, Clone)]
enum Members {
    Pair(Pair),
    MembersCommaPair {
        members: Box<Members>,
        comma: (),
        pair: Pair,
    },
}

impl TypedSymbol for Members {
    type Terminal = JsonLexer;

    fn direct_terminals() -> Vec<Self::Terminal> {
        vec![JsonLexer::Comma]
    }

    fn typed_dependencies() -> Vec<fn(&mut GrammarInfo<Self::Terminal>)> {
        vec![Pair::collect_grammar]
    }

    fn productions() -> Vec<Vec<SymbolId<JsonLexer>>> {
        vec![
            // Members -> Pair
            vec![Pair::symbol_id()],
            // Members -> Members Comma Pair
            vec![
                Members::symbol_id(),
                SymbolId::Terminal(JsonLexer::Comma),
                Pair::symbol_id(),
            ],
        ]
    }

    fn from_syntax_tree(syntax_tree: &SyntaxTree<'_, Token<Self::Terminal>>) -> Self
    where
        Self: Sized,
    {
        match syntax_tree {
            SyntaxTree::Node {
                production_handle,
                children,
            } => {
                match production_handle.production_idx() {
                    0 => {
                        // Members -> Pair
                        let pair_tree = &children[0];
                        let pair = Pair::from_syntax_tree(pair_tree);
                        Members::Pair(pair)
                    }
                    1 => {
                        // Members -> Members Comma Pair
                        let members_tree = &children[0];
                        let members = Members::from_syntax_tree(members_tree);
                        let pair_tree = &children[2];
                        let pair = Pair::from_syntax_tree(pair_tree);
                        Members::MembersCommaPair {
                            members: Box::new(members),
                            comma: (),
                            pair,
                        }
                    }
                    _ => panic!("Unknown production index for Members"),
                }
            },
            _ => panic!("Expected node for non-terminal Members"),
        }
    }
}

#[derive(Debug, Clone)]
struct Pair {
    string: JsonString,
    // #[Terminal(JsonLexer::Colon)]
    colon: (),
    value: Box<Value>,
}

impl TypedSymbol for Pair {
    type Terminal = JsonLexer;

    fn direct_terminals() -> Vec<Self::Terminal> {
        vec![JsonLexer::Colon]
    }

    fn typed_dependencies() -> Vec<fn(&mut GrammarInfo<Self::Terminal>)> {
        vec![Value::collect_grammar]
    }

    fn productions() -> Vec<Vec<SymbolId<Self::Terminal>>> {
        vec![
            // Pair -> String Colon Value
            vec![
                JsonString::symbol_id(),
                SymbolId::Terminal(JsonLexer::Colon),
                Value::symbol_id(),
            ],
        ]
    }

    fn from_syntax_tree(syntax_tree: &SyntaxTree<'_, Token<Self::Terminal>>) -> Self
    where
        Self: Sized,
    {
        match syntax_tree {
            SyntaxTree::Node {
                production_handle,
                children,
            } => {
                // Pair -> String Colon Value
                let string_tree = &children[0];
                let string = JsonString::from_syntax_tree(string_tree);
                let value_tree = &children[2];
                let value = Value::from_syntax_tree(value_tree);
                Pair {
                    string,
                    colon: (),
                    value: Box::new(value),
                }
            },
            _ => panic!("Expected node for non-terminal Pair"),
        }
    }
}

#[derive(Debug, Clone)]
enum Array {
    Empty {
        left_bracket: (),
        right_bracket: (),
    },
    Elements {
        left_bracket: (),
        elements: Box<Elements>,
        right_bracket: (),
    },
}

impl TypedSymbol for Array {
    type Terminal = JsonLexer;

    fn direct_terminals() -> Vec<Self::Terminal> {
        vec![JsonLexer::LeftBracket, JsonLexer::RightBracket]
    }

    fn typed_dependencies() -> Vec<fn(&mut GrammarInfo<Self::Terminal>)> {
        vec![Elements::collect_grammar]
    }

    fn productions() -> Vec<Vec<SymbolId<Self::Terminal>>> {
        vec![
            // Array -> LeftBracket RightBracket
            vec![
                SymbolId::Terminal(JsonLexer::LeftBracket),
                SymbolId::Terminal(JsonLexer::RightBracket),
            ],
            // Array -> LeftBracket Elements RightBracket
            vec![
                SymbolId::Terminal(JsonLexer::LeftBracket),
                Elements::symbol_id(),
                SymbolId::Terminal(JsonLexer::RightBracket),
            ],
        ]
    }

    fn from_syntax_tree(syntax_tree: &SyntaxTree<'_, Token<Self::Terminal>>) -> Self
    where
        Self: Sized,
    {
        match syntax_tree {
            SyntaxTree::Node {
                production_handle,
                children,
            } => {
                match production_handle.production_idx() {
                    0 => {
                        // Array -> LeftBracket RightBracket
                        Array::Empty {
                            left_bracket: (),
                            right_bracket: (),
                        }
                    }
                    1 => {
                        // Array -> LeftBracket Elements RightBracket
                        let elements_tree = &children[1];
                        let elements = Elements::from_syntax_tree(elements_tree);
                        Array::Elements {
                            left_bracket: (),
                            elements: Box::new(elements),
                            right_bracket: (),
                        }
                    }
                    _ => panic!("Unknown production index for Array"),
                }
            },
            _ => panic!("Expected node for non-terminal Array"),
        }
    }
}

impl<'a> From<SyntaxTree<'a, Token<JsonLexer>>> for Array {
    fn from(_tt: SyntaxTree<'a, Token<JsonLexer>>) -> Self {
        unimplemented!()
    }
}

#[derive(Debug, Clone)]
enum Elements {
    Value(Value),
    ElementsCommaValue {
        elements: Box<Elements>,
        comma: (),
        value: Value,
    },
}

impl TypedSymbol for Elements {
    type Terminal = JsonLexer;

    fn direct_terminals() -> Vec<Self::Terminal> {
        vec![JsonLexer::Comma]
    }

    fn typed_dependencies() -> Vec<fn(&mut GrammarInfo<Self::Terminal>)> {
        vec![Value::collect_grammar]
    }

    fn productions() -> Vec<Vec<SymbolId<Self::Terminal>>> {
        vec![
            // Elements -> Value
            vec![Value::symbol_id()],
            // Elements -> Elements Comma Value
            vec![
                Elements::symbol_id(),
                SymbolId::Terminal(JsonLexer::Comma),
                Value::symbol_id(),
            ],
        ]
    }

    fn from_syntax_tree(syntax_tree: &SyntaxTree<'_, Token<Self::Terminal>>) -> Self
    where
        Self: Sized,
    {
        match syntax_tree {
            SyntaxTree::Node {
                production_handle,
                children,
            } => {
                match production_handle.production_idx() {
                    0 => {
                        // Elements -> Value
                        let value_tree = &children[0];
                        let value = Value::from_syntax_tree(value_tree);
                        Elements::Value(value)
                    }
                    1 => {
                        // Elements -> Elements Comma Value
                        let elements_tree = &children[0];
                        let elements = Elements::from_syntax_tree(elements_tree);
                        let value_tree = &children[2];
                        let value = Value::from_syntax_tree(value_tree);
                        Elements::ElementsCommaValue {
                            elements: Box::new(elements),
                            comma: (),
                            value,
                        }
                    }
                    _ => panic!("Unknown production index for Elements"),
                }
            },
            _ => panic!("Expected node for non-terminal Elements"),
        }
    }
}

#[derive(Debug, Clone)]
struct JsonString {
    content: String,
}

impl TerminalSymbol for JsonString {
    type Terminal = JsonLexer;

    const TOKEN_TYPE: Self::Terminal = JsonLexer::String;

    fn from_token(token: &Token<Self::Terminal>) -> Self {
        // Remove the surrounding quotes and unescape characters
        let raw = &token.lexeme;
        let unescaped = &raw[1..raw.len() - 1]; // Simple unescaping for demonstration
        JsonString {
            content: unescaped.to_string(),
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

fn main() {
    let json_input = load_example_json();
    let tokens : Vec<_> = JsonLexer::tokenize(json_input.chars()).filter_map(|result| {
        match result {
            Ok(token) => {
                if token.token_type() != JsonLexer::Whitespace {
                    Some(token)
                } else {
                    None
                }
            }
            Err(e) => {
                eprintln!("Lexing error: {:?}", e);
                None
            }
        }
    }).collect();
    let parser = ValueParser::new();
    let value = parser.parse(&tokens);
    println!("Parsed Value: {:#?}", value);
}
