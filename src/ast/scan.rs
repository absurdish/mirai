// scanner, aka tokenizer: collects tokens from the input source

use std::fmt::{self, Display, Formatter};
use unicode_xid::UnicodeXID;

use super::{
    Ast, AstError, FltSize, IntSize, LitValue, Token,
    TokenType::{self, *},
    KEYWORDS,
};

impl Display for AstError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::InvalidChar(c) => write!(f, "unknown character '{}'", c),
            Self::MissingToken(t) => write!(f, "expected token '{}'", t),
            Self::UnexpectedToken(t) => write!(f, "unexpected token '{}'", t),
            Self::UnreachableToken(t) => write!(f, "failed to reach the token '{}'", t),
        }
    }
}

/// static character lookup table
static CHAR_LOOKUP: [u8; 256] = Ast::build_char_lookup();

/// implement source tokenization
impl Ast {
    /// start the tokenization process
    pub fn tokenize(&mut self) -> Result<(), AstError> {
        // collect every token until you reach the end of the file
        while !self.is_eof() {
            self.start = self.current_token;
            self.consume()?;
        }
        // push the EoF token after reaching the end of the file
        self.tokens.push(Token {
            lexeme: "\0",
            token: TokenType::EoF,
            lit: None,
        });
        Ok(())
    }

    /// consumes tokens from the look up table
    #[inline]
    fn consume(&mut self) -> Result<(), AstError> {
        let char = self.advance();
        let id = CHAR_LOOKUP[char as u8 as usize];

        match id {
            // single character length sized tokens
            1 => self.push(Char(char), None),
            // operator keywords
            2 => {
                if let Some(chars) = self.match_chars(char) {
                    self.push(Chars(chars), None);
                } else {
                    self.push(Char(char), None);
                }
            }
            // slash and comments
            3 => {
                match self.peek() {
                    // line comment
                    '/' => {
                        while self.peek() != '\n' && !self.is_eof() {
                            self.advance();
                        }
                    }
                    // block comment
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
                    // assignement '/='
                    '=' => {
                        self.advance();
                        self.push(Chars(('/', '=')), None);
                    }
                    // normal `/` slash token
                    _ => self.push(Char('/'), None),
                }
            }
            // strings
            4 => {
                while self.peek() != '\'' && !self.is_eof() {
                    if self.peek() == '\\' {
                        self.advance();
                    }
                    self.advance();
                }
                self.advance();
                let lexeme = &self.input[self.start + 1..self.current_token - 1];
                self.push(
                    Literal(LitValue::Str(lexeme.to_string())),
                    Some(LitValue::Str(lexeme.to_string())),
                );
            }
            // ignored tokens, like '\r'
            5 => {}
            6 => self.pos += 4,
            7 => self.pos += 1,
            8 => {
                self.pos = 1;
                self.line += 1;
            }
            // numbers
            9 => {
                let mut is_float = false;
                while self.peek_byte(0).is_ascii_digit() {
                    self.advance();
                }
                if self.peek() == '.' && self.peek_byte(1).is_ascii_digit() {
                    is_float = true;
                    self.advance();
                    while self.peek_byte(0).is_ascii_digit() {
                        self.advance();
                    }
                }

                let lexeme = &self.input[self.start..self.current_token];

                // TODO: implement auto-resizing
                let value = if is_float {
                    LitValue::Flt(
                        lexeme.parse::<f32>().expect("failed to unwrap a number"),
                    )
                } else {
                    LitValue::Int(
                        lexeme.parse::<i32>().expect("failed to unwrap a number"),
                    )
                };
                self.push(Literal(value.clone()), Some(value));
            }
            // identifiers and keywords
            10 => {
                while UnicodeXID::is_xid_continue(self.peek()) {
                    self.advance();
                }
                let lexeme = &self.input[self.start..self.current_token];

                let token = if KEYWORDS.contains(&lexeme) {
                    Keyword(lexeme)
                } else {
                    Identifier
                };
                self.push(token, None);
            }
            _ => return Err(AstError::InvalidChar(char)),
        }

        Ok(())
    }

    // matches double character length sizes tokens
    #[inline(always)]
    fn match_chars(&mut self, char: char) -> Option<(char, char)> {
        let next = self.peek();
        match (char, next) {
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
                Some((char, next))
            }
            _ => None,
        }
    }

    /*
        helper functions
    */

    /// builds a table for looking up characters
    #[inline(always)]
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

    /// pushes token into the token list
    #[inline(always)]
    fn push(&mut self, token: TokenType, lit: Option<LitValue>) {
        let lexeme = &self.input[self.start..self.current_token];
        self.tokens.push(Token { lexeme, token, lit });
        self.pos += lexeme.len() as u32;
    }

    /// peek trough the next character
    #[inline(always)]
    fn peek(&mut self) -> char {
        // match is used over unwrap_or for better performance
        match self.input[self.current_token..].chars().next() {
            Some(c) => c,
            None => '\0',
        }
    }

    /// peek trough the next byte character
    #[inline(always)]
    fn peek_byte(&mut self, by: usize) -> &u8 {
        // match is used over unwrap_or for better performance
        match self.as_bytes.get(self.current_token + by) {
            Some(c) => c,
            None => &0,
        }
    }

    /// peek trough the next 2 characters
    #[inline(always)]
    fn peek_next(&mut self) -> char {
        // match is used over unwrap_or for better performance
        match self.input[self.current_token..].chars().nth(1) {
            Some(c) => c,
            None => '\0',
        }
    }

    /// advance with one character
    #[inline(always)]
    fn advance(&mut self) -> char {
        let c = self.peek();
        self.current_token += c.len_utf8();
        c
    }

    /// checks if scanner is currently at the end of the file
    #[inline(always)]
    fn is_eof(&mut self) -> bool {
        self.current_token >= self.input.len() || self.peek() == '\0'
    }
}

#[test]
fn scanner_tests() {
    use super::*;

    let input = "";
    let mut ast = Ast::new(input);
    ast.tokenize().unwrap();
    assert_eq!(
        ast.tokens,
        SmallVec::from([Token {
            lexeme: "\0",
            token: TokenType::EoF,
            lit: None
        }])
    );
}
