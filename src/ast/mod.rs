pub mod lexp;
pub mod scan;
pub mod stmt;
pub mod texp;
use std::fmt::Display;

use smallvec::SmallVec;
use stmt::Stmt;

use crate::core::memory::Function;

/// the component that the source code is parsed into using the scanner and is used for building the AST
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// lexeme or a string representation of the token
    pub lexeme: &'static str,
    /// type of the token
    pub token: TokenType,
    /// literal value the token is carrying
    pub lit: Option<LitValue>,
}

/// enum of allowed token types
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    /// keyword token type, for example: `while`, `if` and etc.
    Keyword(&'static str),
    /// single character length sized token
    Char(char),
    /// double character length sized token
    Chars((char, char)),
    /// literal valued token
    Literal(LitValue),
    /// identifier type
    Identifier,
    /// indicator of the end of file
    EoF,
}

/// allowed literal values
#[derive(Debug, Clone, PartialEq)]
pub enum LitValue {
    Vector {
        kind: Vec<LitValue>,
        owner: &'static str,
    },
    /// signed integer type
    Int {
        kind: i64,
        owner: &'static str,
    },
    /// unsigned integer type
    Unt {
        kind: u64,
        owner: &'static str,
    },
    /// floating point number type
    Flt {
        kind: f64,
        owner: &'static str,
    },
    /// string type
    Str {
        kind: String,
        owner: &'static str,
    },
    /// boolean type
    Bool {
        kind: bool,
        owner: &'static str,
    },
    /// void type, can only be returned by functions
    Void,
    /// empty/null value
    Nil,
    HeapRef(usize),
    Fun(Box<Function>),
}
use LitValue::*;

impl Display for LitValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Vector { kind, .. } => {
                write!(f, "[")?;
                for (i, value) in kind.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, "]")
            }
            Int { kind, .. } => write!(f, "{}", kind),
            Unt { kind, .. } => write!(f, "{}", kind),
            Flt { kind, .. } => write!(f, "{}", kind),
            Bool { kind, .. } => write!(f, "{}", kind),
            Str { kind, .. } => write!(f, "{}", kind),
            Void => write!(f, "void"),
            Nil => write!(f, "nil"),
            HeapRef(e) => write!(f, "ref{}*", e),
            Fun(_) => write!(f, "function"),
        }
    }
}

impl LitValue {
    pub fn is_truthy(&self) -> bool {
        match self {
            Nil | Void | HeapRef(_) | Fun(_) => false,
            Flt { kind, .. } => *kind != 0.0,
            Str { kind, .. } => kind.len() != 0,
            Unt { kind, .. } => *kind != 0,
            Bool { kind, .. } => *kind,
            Int { kind, .. } => *kind != 0,
            Vector { kind, .. } => kind.len() != 0,
        }
    }
    pub fn owner(&self) -> &'static str {
        match self {
            Flt { owner, .. }
            | Str { owner, .. }
            | Unt { owner, .. }
            | Int { owner, .. }
            | Bool { owner, .. } => owner,
            _ => "",
        }
    }
    pub fn owned(&self, owner: &'static str) -> LitValue {
        match self {
            LitValue::Flt { kind, .. } => LitValue::Flt { kind: *kind, owner },
            LitValue::Str { kind, .. } => LitValue::Str {
                kind: kind.clone(),
                owner,
            },
            LitValue::Unt { kind, .. } => LitValue::Unt { kind: *kind, owner },
            LitValue::Int { kind, .. } => LitValue::Int { kind: *kind, owner },
            LitValue::Bool { kind, .. } => LitValue::Bool { kind: *kind, owner },
            other => other.clone(),
        }
    }
}

/// list of keywords in the language
pub const KEYWORDS: &[&str] = &[
    // statement keywords
    "if", "else", "while", "return", "break", "print", // type keywords
    "int", "unt", "flt", "bool", "str", "nil", "void", // reserver keywords
    "for", "impl", "as", "to", "use", "trait",
];
/// default buffer size for storing tokens (pre-allocation increases effeciency)
const BUFFER_SIZE: usize = 128;

/// errors that might occur during scanning or parsing
#[derive(Debug)]
pub enum AstError {
    InvalidChar(char),
    UnexpectedToken(&'static str),
    MissingToken(&'static str),
    UnreachableToken(&'static str),
}

/// AST struct for managing tokenization and parsing
#[derive(Debug)]
pub struct Ast {
    /// input source code
    input: &'static str,
    /// input as bytes
    as_bytes: &'static [u8],
    /// collected list of tokens with pre-allocated memory
    tokens: SmallVec<[Token; BUFFER_SIZE]>,
    /// current line
    line: u16,
    /// current position
    pos: u32,
    /// starting token index, used in scanner
    start: usize,
    /// current token index, used in scanner
    current_token: usize,
    /// current expr index, used in parser
    current_expr: usize,
    /// current expr id, used in parser
    id: usize,
}

impl Ast {
    /// initializes AST parser
    pub fn new(input: &'static str) -> Self {
        Self {
            input,
            as_bytes: input.as_bytes(),
            tokens: SmallVec::with_capacity(BUFFER_SIZE),
            line: 1,
            pos: 1,
            start: 0,
            current_token: 0,
            current_expr: 0,
            id: 0,
        }
    }

    /// starts the parsing process
    pub fn start(&mut self) -> Result<Vec<Stmt>, AstError> {
        self.tokenize()?;
        self.parse()
    }

    //
    // parser helper functions
    //

    /// returns new id for the expressions
    #[inline(always)]
    pub fn next_id(&mut self) -> usize {
        let id = self.id;
        self.id += 1;
        id
    }

    #[inline(always)]
    pub fn consumes(&mut self, token: TokenType) -> Result<Token, AstError> {
        if self.check(token) {
            return self.advances();
        }
        Err(AstError::MissingToken(self.peeks()?.lexeme))
    }

    #[inline(always)]
    pub fn check(&self, token: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        if let Some(current_token) = self.tokens.get(self.current_expr) {
            return match token {
                TokenType::Char(c) => current_token.lexeme.chars().next() == Some(c),
                TokenType::Chars((c1, c2)) => current_token.lexeme == &format!("{}{}", c1, c2),
                TokenType::Keyword(kw) => current_token.lexeme == kw,
                _ => current_token.token == token,
            };
        }

        false
    }

    #[inline(always)]
    pub fn match_token(&mut self, token: TokenType) -> bool {
        if self.check(token) {
            let _ = self.advances();
            true
        } else {
            false
        }
    }

    #[inline(always)]
    pub fn match_any(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.match_token(token_type.clone()) {
                return true;
            }
        }
        false
    }

    #[inline(always)]
    pub fn peeks(&self) -> Result<Token, AstError> {
        if let Some(token) = self.tokens.get(self.current_expr).cloned() {
            return Ok(token);
        }
        Err(AstError::UnreachableToken("unknown"))
    }

    /// check if program has reached the end of the file
    #[inline(always)]
    pub fn is_at_end(&self) -> bool {
        self.current_expr >= self.tokens.len()
    }

    #[inline(always)]
    pub fn advances(&mut self) -> Result<Token, AstError> {
        if !self.is_at_end() {
            self.current_expr += 1;
        }
        self.prev(1)
    }

    #[inline(always)]
    pub fn prev(&self, steps: usize) -> Result<Token, AstError> {
        if let Some(token) = self.tokens.get(self.current_expr - steps).cloned() {
            return Ok(token);
        }
        Err(AstError::UnreachableToken("unknown"))
    }
}
