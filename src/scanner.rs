use unicode_xid::UnicodeXID;

#[derive(Debug)]
pub enum TokenType {
    Keyword(String),
    SingleChar(char),
    DblChar((char, char)),
    Identifier,
    Literal,
    EoF,
}

use TokenType::*;

#[derive(Debug)]
pub enum LitVal {
    Int(i16),
    Int32(i32),
    Unt(u16),
    Unt32(u32),
    Flt(f32),
    F64(f64),
    Imaginary32(f32),
    Imaginary64(f64),
    Str(String),
    Chr(char),
}

#[derive(Debug)]
pub struct Token<'a> {
    lexeme: &'a str,
    token_type: TokenType,
    value: Option<LitVal>,
    line: usize,
    pos: usize,
    length: usize,
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
            '#' | '_' | '(' | ')' | '{' | '}' | '[' | ']' | ';' | ',' | '?' => {
                self.push(SingleChar(c), None)
            }
            ':' | '&' | '|' | '<' | '>' | '!' | '\\' | '+' | '-' | '*' | '=' | '.' => {
                self.handle_operator(c)
            }
            '/' => self.handle_slash(),
            '\'' => self.handle_char_literal(),
            '"' => self.handle_string_literal(),
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
            | ('-', '-')
            | ('*', '=')
            | ('=', '>')
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

    fn handle_char_literal(&mut self) {
        let c = self.advance();
        if self.peek() == '\'' {
            self.advance();
            self.push(Literal, Some(LitVal::Chr(c)));
        } else {
            while self.peek() != '\'' && !self.is_eof() {
                self.advance();
            }
            self.advance();
            let lexeme = &self.input[self.start + 1..self.current - 1];
            self.push(Literal, Some(LitVal::Str(lexeme.to_string())));
        }
    }

    fn handle_string_literal(&mut self) {
        while self.peek() != '"' && !self.is_eof() {
            if self.peek() == '\\' {
                self.advance();
            }
            self.advance();
        }
        self.advance();
        let lexeme = &self.input[self.start + 1..self.current - 1];
        self.push(Literal, Some(LitVal::Str(lexeme.to_string())));
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
            if is_float {
                let lexeme = &self.input[self.start..self.current - 1];
                let value = lexeme.parse::<f32>().unwrap();
                self.push(Literal, Some(LitVal::Imaginary32(value)));
            }
        } else {
            let lexeme = &self.input[self.start..self.current];
            let value = if is_float {
                let val = lexeme.parse::<f32>().unwrap();
                LitVal::Flt(val)
            } else {
                let val = lexeme.parse::<i32>().unwrap();
                LitVal::Int32(val)
            };
            self.push(Literal, Some(value));
        }
    }

    fn handle_identifier_or_keyword(&mut self) {
        while UnicodeXID::is_xid_continue(self.peek()) {
            self.advance();
        }
        let lexeme = &self.input[self.start..self.current];
        let token_type = match lexeme {
            "if" | "else" | "for" | "while" | "return" | "int" | "str" => {
                Keyword(lexeme.to_string())
            }
            _ => Identifier,
        };
        self.push(token_type, None);
    }

    fn push(&mut self, token_type: TokenType, value: Option<LitVal>) {
        let lexeme = &self.input[self.start..self.current];
        let length = lexeme.chars().count();
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
        self.current >= self.input.len()
    }
}