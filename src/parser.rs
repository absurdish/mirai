use crate::scanner::{Token, TokenType};
use std::fmt;

#[derive(Debug, Clone)]
pub enum Expr {
    Null,
}

#[derive(Debug, Clone)]
pub enum UseKinds<'a> {
    Str(&'a str),
    List(Vec<&'a str>),
}

#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Nil,
    Var {
        id: Token<'a>,
        type_: Token<'a>,
        value: Expr,
        is_pub: bool,
        is_const: bool,
        rules: &'a [&'a str],
    },
    Fn {
        id: Token<'a>,
        type_: Token<'a>,
        body: Vec<Stmt<'a>>,
        params: Vec<(Token<'a>, Token<'a>)>,
        is_pub: bool,
        rules: &'a [&'a str],
    },
    Block {
        stmts: Vec<Stmt<'a>>,
        rules: &'a [&'a str],
    },
    Expr(Expr),
    If {
        pred: Expr,
        body: Box<Stmt<'a>>,
        else_b: Option<Box<Stmt<'a>>>,
    },
    Return(Expr),
    Use {
        src: UseKinds<'a>,
        uses: Vec<&'a str>,
        is_all: bool,
    },
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(String),
    MissingToken(String),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserError::UnexpectedToken(msg) => write!(f, "Unexpected token: {}", msg),
            ParserError::MissingToken(msg) => write!(f, "Missing token: {}", msg),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    pub tokens: &'a Vec<Token<'a>>,
    pub current: usize,
    pub id_counter: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token<'a>>) -> Self {
        Self {
            tokens,
            current: 0,
            id_counter: 0,
        }
    }

    pub fn start(&mut self) -> Result<Vec<Stmt<'a>>, ParserError> {
        let mut stmts = Vec::with_capacity(self.tokens.len() / 2);
        while self.current < self.tokens.len() {
            stmts.push(self.stmt()?);
        }
        stmts.shrink_to_fit();
        Ok(stmts)
    }

    pub fn stmt(&mut self) -> Result<Stmt<'a>, ParserError> {
        let token = self.peek();

        if let TokenType::Keyword(k) = &token.token_type {
            if self.is_type_kwd(k) {
                let type_ = self.consume(TokenType::Keyword(k.clone()))?;
                let name = self.consume(TokenType::Identifier)?;
                if self.peek().lexeme == "=" {
                    self.stmt_var(name, type_)
                } else {
                    Ok(Stmt::Fn {
                        id: name,
                        type_,
                        body: Vec::new(),
                        params: Vec::new(),
                        is_pub: false,
                        rules: &[],
                    })
                }
            } else {
                Ok(Stmt::Nil)
            }
        } else {
            Ok(self.exprs())
        }
    }

    fn stmt_var(&mut self, name: Token<'a>, type_: Token<'a>) -> Result<Stmt<'a>, ParserError> {
        self.consume(TokenType::SingleChar('='))?;
        let is_const = matches!(self.consume_if_matches(TokenType::SingleChar('=')), Some(_));
        let value = self.expr();

        Ok(Stmt::Var {
            id: name,
            type_,
            value,
            is_const,
            is_pub: false,
            rules: &[],
        })
    }

    fn consume_if_matches(&mut self, token_type: TokenType) -> Option<Token<'a>> {
        if self.check(&token_type) {
            Some(self.advance())
        } else {
            None
        }
    }

    fn is_type_kwd(&self, k: &'a str) -> bool {
        matches!(
            k,
            "int"
                | "i16"
                | "i32"
                | "unt"
                | "u16"
                | "u32"
                | "flt"
                | "f32"
                | "f64"
                | "bol"
                | "str"
                | "chr"
                | "nil"
        )
    }

    pub fn exprs(&mut self) -> Stmt<'a> {
        self.advance();
        Stmt::Expr(Expr::Null)
    }

    pub fn expr(&mut self) -> Expr {
        self.advance();
        Expr::Null
    }

    pub fn next_id(&mut self) -> usize {
        self.id_counter += 1;
        self.id_counter - 1
    }

    pub fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        let current_token = self.peek();
        match token_type {
            TokenType::SingleChar(c) => current_token.lexeme == c.to_string(),
            TokenType::DblChar((c1, c2)) => current_token.lexeme == format!("{}{}", c1, c2),
            TokenType::Keyword(kw) => current_token.lexeme == *kw,
            _ => current_token.token_type == *token_type,
        }
    }

    pub fn consume(&mut self, token_type: TokenType) -> Result<Token<'a>, ParserError> {
        if self.check(&token_type) {
            Ok(self.advance())
        } else {
            Err(ParserError::MissingToken("Token not found".to_string()))
        }
    }

    pub fn consume_one_of(
        &mut self,
        token_types: &[TokenType],
        error_msg: &str,
    ) -> Result<Token<'a>, ParserError> {
        for token_type in token_types {
            if self.check(token_type) {
                return Ok(self.advance());
            }
        }
        Err(ParserError::MissingToken(error_msg.to_string()))
    }

    pub fn advance(&mut self) -> Token<'a> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.prev(1)
    }

    pub fn prev(&self, steps: usize) -> Token<'a> {
        if self.current < steps {
            self.error_token()
        } else {
            self.tokens[self.current - steps].clone()
        }
    }

    pub fn peek(&self) -> Token<'a> {
        if self.is_at_end() {
            self.error_token()
        } else {
            self.tokens[self.current].clone()
        }
    }

    pub fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn error_token(&self) -> Token<'a> {
        Token {
            token_type: TokenType::EoF,
            lexeme: "\0",
            line: 0,
            value: None,
            pos: 0,
            length: 0,
        }
    }

    pub fn match_token(&mut self, token_type: TokenType) -> bool {
        if self.check(&token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn match_any(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.match_token(token_type.clone()) {
                return true;
            }
        }
        false
    }
}
