mod scan;
use smallvec::SmallVec;

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
    /// signed integer type
    Int(IntSize),
    /// unsigned integer type
    Unt(UntSize),
    /// floating point number type
    Flt(FltSize),
    /// string type
    Str(String),
    /// boolean type
    Bool(bool),
    /// void type, can only be returned by functions
    Void,
    /// empty/null value
    Nil,
}

/// signed integer type variations
#[derive(Debug, Clone, PartialEq)]
pub enum IntSize {
    I32(i32),
    I64(i64),
    I128(i128),
}

/// unsigned integer type variations
#[derive(Debug, Clone, PartialEq)]
pub enum UntSize {
    U32(u32),
    U64(u64),
    U128(u128),
}

/// floating point number type variations
#[derive(Debug, Clone, PartialEq)]
pub enum FltSize {
    F32(f32),
    F64(f64),
}

/// list of keywords in the language
const KEYWORDS: &[&str] = &[
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
    pub fn start(&mut self) -> Result<(), AstError> {
        self.tokenize()?;

        Ok(())
    }
}
