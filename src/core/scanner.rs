use coloredpp::Colorize;
use unicode_xid::UnicodeXID;
use TokenType::*;
use crate::consts::KEYWORDS;
use crate::core::memory::Function;


#[derive(Debug, Clone, PartialEq)]
pub enum TokenType<'a> {
    Keyword(&'a str),
    SingleChar(char),
    DblChar((char, char)),
    Identifier,
    Literal(Value<'a>),
    EoF,
}

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
    pub fn same_type(&self, v2: &Value) -> bool {
        match (self, v2) {
            (Int(_), Int(_)) => true,
            (Int64(_), Int64(_)) => true,
            (Unt(_), Unt(_)) => true,
            (Unt64(_), Unt64(_)) => true,
            (Flt(_), Flt(_)) => true,
            (F64(_), F64(_)) => true,
            (Im32(_), Im32(_)) => true,
            (Im64(_), Im64(_)) => true,
            (Str(_), Str(_)) => true,
            (Chr(_), Chr(_)) => true,
            (Bol(_), Bol(_)) => true,
            (HeapRef(_), HeapRef(_)) => true,
            (Function(_), Function(_)) => true,
            (Nil, Nil) => true,
            _ => false,
        }
    }

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
            Str(s) => s.len() > 0,
            Bol(b) => *b,
            _ => false
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
    pub line: usize,
    pub pos: usize,
    pub length: usize,
}

impl<'a> Token<'a> {
    fn eof(line: usize) -> Self {
        Token {
            lexeme: "\0",
            token_type: EoF,
            value: None,
            line,
            pos: 0,
            length: 0,
        }
    }
}

#[derive(Debug)]
pub struct Scanner<'a> {
    input: &'a str,
    tokens: Vec<Token<'a>>,
    line: usize,
    pos: usize,
    start: usize,
    current: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            tokens: Vec::with_capacity(128),
            line: 1,
            pos: 1,
            start: 0,
            current: 0,
        }
    }

    pub fn start(&mut self) -> &Vec<Token> {
        while !self.is_eof() {
            self.start = self.current;
            self.consume();
        }
        self.tokens.push(Token::eof(self.line));
        &self.tokens
    }

    fn consume(&mut self) {
        let c = self.advance();
        match c {
            '#' | '_' | '(' | ')' | '{' | '}' | '[' | ']' | ';' | ',' | '?' | '%' => {
                self.push(SingleChar(c), None)
            }
            ':' | '&' | '|' | '<' | '>' | '!' | '\\' | '+' | '-' | '*' | '=' | '.' => {
                self.handle_operator(c)
            }
            '/' => self.handle_slash(),
            '\'' => self.handle_string_literal(),
            '\r' => {}
            '\t' => self.pos += 4,
            ' ' => self.pos += 1,
            '\n' => {
                self.pos = 1;
                self.line += 1;
            }
            _ if c.is_ascii_digit() => self.handle_number(),
            _ if UnicodeXID::is_xid_start(c) || c == '_' => self.handle_identifier_or_keyword(),
            _ => self.push(Identifier, None),
        }
    }

    fn handle_operator(&mut self, c: char) {
        if let Some(dbl_char) = self.match_double_char(c) {
            self.push(DblChar(dbl_char), None);
        } else {
            self.push(SingleChar(c), None);
        }
    }

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

    fn handle_slash(&mut self) {
        match self.peek() {
            '/' => {
                while self.peek() != '\n' && !self.is_eof() {
                    self.advance();
                }
            }
            '*' => {
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
            '=' => {
                self.advance();
                self.push(DblChar(('/', '=')), None);
            }
            _ => self.push(SingleChar('/'), None),
        }
    }

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
            self.push(
                Literal(Value::Chr(val)),
                Some(Value::Chr(val)),
            );
        } else {
            let val = Value::Str(lexeme.to_string());
            self.push(Literal(val.clone()), Some(val));
        }
    }

    fn handle_number(&mut self) {
        let mut is_float = false;
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            is_float = true;
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        if self.peek() == 'i' {
            self.advance();
            let lexeme = &self.input[self.start..self.current - 1];
            let value = lexeme.parse::<f32>().unwrap();
            self.push(Literal(Value::Im32(value)), Some(Value::Im32(value)));
        } else {
            let lexeme = &self.input[self.start..self.current];
            let value = if is_float {
                match lexeme.parse::<f32>() {
                    Ok(lex) => Value::Flt(lex),
                    _ => {
                        let val = lexeme.parse::<f64>().unwrap();
                        F64(val)
                    }
                }
            } else {
                match lexeme.parse::<i32>() {
                    Ok(lex) => Value::Int(lex),
                    _ => {
                        let val = lexeme.parse::<i64>().unwrap();
                        Int64(val)
                    }
                }
            };
            self.push(Literal(value.clone()), Some(value));
        }
    }

    fn handle_identifier_or_keyword(&mut self) {
        while UnicodeXID::is_xid_continue(self.peek()) {
            self.advance();
        }
        let lexeme = &self.input[self.start..self.current];

        let token_type = if KEYWORDS.contains(&lexeme) {
            Keyword(lexeme)
        } else {
            Identifier
        };
        self.push(token_type, None);
    }
    fn push(&mut self, token_type: TokenType<'a>, value: Option<Value<'a>>) {
        // Avoid slicing the string multiple times
        let lexeme = &self.input[self.start..self.current];
        let length = lexeme.len();
        self.tokens.push(Token {
            lexeme,
            token_type,
            value,
            line: self.line,
            pos: self.pos,
            length,
        });
        self.pos += length;
    }

    fn advance(&mut self) -> char {
        let c = self.peek();
        self.current += c.len_utf8();
        c
    }

    fn peek(&self) -> char {
        self.input[self.current..].chars().next().unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        self.input[self.current..].chars().nth(1).unwrap_or('\0')
    }

    fn is_eof(&self) -> bool {
        self.current >= self.input.len() || self.peek() == '\0'
    }
}
