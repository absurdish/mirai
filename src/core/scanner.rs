use crate::consts::KEYWORDS;
use crate::core::memory::Function;
use coloredpp::Colorize;
use smallvec::SmallVec;
use unicode_xid::UnicodeXID;
use TokenType::*;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType<'a> {
    Keyword(&'a str),
    SingleChar(char),
    DblChar((char, char)),
    Identifier,
    Literal(Value<'a>),
    EoF,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    Int(i32),
    Int64(i64),
    Unt(u32),
    Unt64(u64),
    Flt(f32),
    F64(f64),
    Im32(f32),
    Im64(f64),
    Str(String),
    Chr(char),
    Bol(bool),
    HeapRef(usize),
    Function(Function<'a>),
    Nil,
}

impl<'a> Value<'a> {
    #[inline(always)]
    pub fn get_type(&self) -> &'a str {
        match self {
            Int(_) => "int",
            Int64(_) => "i64",
            Unt(_) => "unt",
            Unt64(_) => "u64",
            Flt(_) => "flt",
            F64(_) => "f64",
            Im32(_) => "im",
            Im64(_) => "im64",
            Str(_) => "str",
            Chr(_) => "chr",
            Bol(_) => "bol",
            HeapRef(_) => "heap_ref",
            Function(_) => "function",
            Nil => "nil",
        }
    }

    #[inline(always)]
    pub fn same_type(&self, v2: &Value) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(v2)
    }

    #[inline(always)]
    pub fn is_truthy(&self) -> bool {
        match self {
            Int(a) => *a != 0,
            Int64(a) => *a != 0,
            Unt(a) => *a != 0,
            Unt64(a) => *a != 0,
            Flt(a) => *a != 0.0,
            F64(a) => *a != 0.0,
            Im32(a) => *a != 0.0,
            Im64(a) => *a != 0.0,
            Str(s) => !s.is_empty(),
            Bol(b) => *b,
            _ => false,
        }
    }
}
use crate::core::scanner::Value::*;

impl std::fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Int(i) => write!(f, "{}", i.blue()),
            Int64(i) => write!(f, "{}", i.blue()),
            Unt(i) => write!(f, "{}", i.blue()),
            Unt64(i) => write!(f, "{}", i.blue()),
            Flt(i) => write!(f, "{}", i.blue()),
            F64(i) => write!(f, "{}", i.blue()),
            Bol(i) => write!(f, "{}", i.blue()),
            HeapRef(i) => write!(f, "{}", i.yellow()),
            Im32(i) => write!(f, "{}{}", i.blue(), "i".green()),
            Im64(i) => write!(f, "{}{}", i.blue(), "i".green()),
            Str(i) => write!(f, "'{}'", i.yellow()),
            Chr(i) => write!(f, "'{}'", i.yellow()),
            Nil => write!(f, "nil"),
            Function(_) => write!(f, "function"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub lexeme: &'a str,
    pub token_type: TokenType<'a>,
    pub value: Option<Value<'a>>,
    pub length: usize,
}

const BUFFER_SIZE: usize = 128;

static CHAR_LOOKUP: [u8; 256] = Scanner::build_char_lookup();

pub struct Scanner<'a> {
    input: &'a str,
    input_bytes: &'a [u8],
    tokens: SmallVec<[Token<'a>; BUFFER_SIZE]>,
    line: u32,
    pos: u32,
    start: usize,
    current: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            input_bytes: input.as_bytes(),
            tokens: SmallVec::with_capacity(BUFFER_SIZE),
            line: 1,
            pos: 1,
            start: 0,
            current: 0,
        }
    }

    pub fn start(&mut self) -> Vec<Token<'a>> {
        while !self.is_eof() {
            self.start = self.current;
            self.consume();
        }
        self.tokens.push(Token::eof());
        self.tokens.to_vec()
    }

    const fn build_char_lookup() -> [u8; 256] {
        let mut table = [0u8; 256];
        let mut i = 0;
        while i < 256 {
            table[i] = match i as u8 as char {
                '#' | '(' | ')' | '{' | '}' | '[' | ']' | ';' | ',' | '?' | '%' => 1,
                ':' | '&' | '|' | '<' | '>' | '!' | '\\' | '+' | '-' | '*' | '=' | '.' => 2,
                '/' => 3,
                '\'' => 4,
                '\r' => 5,
                '\t' => 6,
                ' ' => 7,
                '\n' => 8,
                '0'..='9' => 9,
                'a'..='z' | 'A'..='Z' | '_' => 10,
                _ => 0,
            };
            i += 1;
        }
        table
    }

    #[inline(always)]
    fn consume(&mut self) {
        let c = self.advance();
        let action = CHAR_LOOKUP[c as u8 as usize];

        match action {
            1 => self.push(SingleChar(c), None),
            2 => self.handle_operator(c),
            3 => self.handle_slash(),
            4 => self.handle_string_literal(),
            5 => {}
            6 => self.pos += 4,
            7 => self.pos += 1,
            8 => {
                self.pos = 1;
                self.line += 1;
            }
            9 => self.handle_number(),
            10 => self.handle_identifier_or_keyword(),
            _ => self.push(Identifier, None),
        }
    }

    #[inline(always)]
    fn handle_operator(&mut self, c: char) {
        if let Some(dbl_char) = self.match_double_char(c) {
            self.push(DblChar(dbl_char), None);
        } else {
            self.push(SingleChar(c), None);
        }
    }

    #[inline(always)]
    fn match_double_char(&mut self, c: char) -> Option<(char, char)> {
        let next = self.peek();
        match (c, next) {
            (':', ':')
            | ('&', '&')
            | ('|', '|')
            | ('<', '=')
            | ('>', '=')
            | ('!', '=')
            | ('+', '+')
            | ('+', '=')
            | ('-', '-')
            | ('-', '=')
            | ('*', '=')
            | ('=', '>')
            | ('=', '=')
            | ('.', '.') => {
                self.advance();
                Some((c, next))
            }
            _ => None,
        }
    }

    #[inline(always)]
    fn handle_slash(&mut self) {
        match self.peek() {
            '/' => {
                while self.peek() != '\n' && !self.is_eof() {
                    self.advance();
                }
            }
            '*' => self.handle_block_comment(),
            '=' => {
                self.advance();
                self.push(DblChar(('/', '=')), None);
            }
            _ => self.push(SingleChar('/'), None),
        }
    }

    #[inline(always)]
    fn handle_block_comment(&mut self) {
        while !self.is_eof() {
            if self.peek() == '*' && self.peek_next() == '/' {
                self.advance();
                self.advance();
                break;
            }
            if self.peek() == '\n' {
                self.line += 1;
                self.pos = 1;
            }
            self.advance();
        }
    }

    #[inline(always)]
    fn handle_string_literal(&mut self) {
        while self.peek() != '\'' && !self.is_eof() {
            if self.peek() == '\\' {
                self.advance();
            }
            self.advance();
        }
        self.advance();
        let lexeme = &self.input[self.start + 1..self.current - 1];
        if lexeme.len() == 1 {
            let val = lexeme.chars().next().unwrap_or('\0');
            self.push(Literal(Chr(val)), Some(Chr(val)));
        } else {
            let val = Str(lexeme.to_string());
            self.push(Literal(val.clone()), Some(val));
        }
    }

    #[inline(always)]
    fn handle_number(&mut self) {
        let mut is_float = false;
        while self.peek_byte().is_ascii_digit() {
            self.advance();
        }
        if self.peek() == '.' && self.peek_next_byte().is_ascii_digit() {
            is_float = true;
            self.advance();
            while self.peek_byte().is_ascii_digit() {
                self.advance();
            }
        }
        if self.peek() == 'i' {
            self.advance();
            let lexeme = &self.input[self.start..self.current - 1];
            let value = unsafe { lexeme.parse::<f32>().unwrap_unchecked() };
            self.push(Literal(Im32(value)), Some(Im32(value)));
            return;
        }

        let lexeme = &self.input[self.start..self.current];
        let value = if is_float {
            if let Ok(v) = lexeme.parse::<f32>() {
                Flt(v)
            } else {
                F64(unsafe { lexeme.parse::<f64>().unwrap_unchecked() })
            }
        } else {
            if let Ok(v) = lexeme.parse::<i32>() {
                Int(v)
            } else {
                Int64(unsafe { lexeme.parse::<i64>().unwrap_unchecked() })
            }
        };
        self.push(Literal(value.clone()), Some(value));
    }

    #[inline(always)]
    fn handle_identifier_or_keyword(&mut self) {
        while UnicodeXID::is_xid_continue(self.peek()) {
            self.advance();
        }
        let lexeme = &self.input[self.start..self.current];
        
        let token_type = if KEYWORDS.contains(&lexeme){
            Keyword(lexeme)
        } else {
            Identifier
        };
        self.push(token_type, None);
    }

    #[inline(always)]
    fn push(&mut self, token_type: TokenType<'a>, value: Option<Value<'a>>) {
        let lexeme = &self.input[self.start..self.current];
        let length = lexeme.len();
        self.tokens.push(Token {
            lexeme,
            token_type,
            value,
            length,
        });
        self.pos += length as u32;
    }

    #[inline(always)]
    fn advance(&mut self) -> char {
        let c = self.peek();
        self.current += c.len_utf8();
        c
    }

    #[inline(always)]
    fn peek(&self) -> char {
        self.input[self.current..].chars().next().unwrap_or('\0')
    }

    #[inline(always)]
    fn peek_byte(&self) -> u8 {
        *self.input_bytes.get(self.current).unwrap_or(&0)
    }

    #[inline(always)]
    fn peek_next_byte(&self) -> u8 {
        *self.input_bytes.get(self.current + 1).unwrap_or(&0)
    }

    #[inline(always)]
    fn peek_next(&self) -> char {
        self.input[self.current..].chars().nth(1).unwrap_or('\0')
    }

    #[inline(always)]
    fn is_eof(&self) -> bool {
        self.current >= self.input.len() || self.peek() == '\0'
    }
}

impl<'a> Token<'a> {
    fn eof() -> Self {
        Token {
            lexeme: "\0",
            token_type: EoF,
            value: None,
            length: 0,
        }
    }
}
