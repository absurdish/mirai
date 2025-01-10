use crate::core::scanner::{Value, Token, TokenType::{self, *}};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Assign {
        id: usize,
        name: Token<'a>,
        value: Box<Expr<'a>>,
    },
    Var { id: usize, name: Token<'a> },
    Call { id: usize, name: Box<Expr<'a>>, args: Vec<Expr<'a>> },
    Unary { id: usize, op: Token<'a>, rhs: Box<Expr<'a>> },
    Binary { id: usize, op: Token<'a>, lhs: Box<Expr<'a>>, rhs: Box<Expr<'a>> },
    Grouping { id: usize, expr: Box<Expr<'a>> },
    Value { id: usize, value: Value<'a> },
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UseKinds<'a> {
    Str(&'a str),
    List(Vec<&'a str>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'a> {
    Print(Expr<'a>),
    Var {
        id: Token<'a>,
        type_: Token<'a>,
        value: Expr<'a>,
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
    Expr(Expr<'a>),
    If {
        pred: Expr<'a>,
        body: Box<Stmt<'a>>,
        else_b: Option<Box<Stmt<'a>>>,
    },
    While {
        pred: Expr<'a>,
        body: Box<Stmt<'a>>,
    },
    Return(Expr<'a>),
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
    UnknownError(String),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserError::UnexpectedToken(msg) => write!(f, "Unexpected token: {}", msg),
            ParserError::MissingToken(msg) => write!(f, "Missing token: {}", msg),
            ParserError::UnknownError(msg) => write!(f, "Unknown error: {}", msg),
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
        let mut stmts = Vec::with_capacity((self.tokens.len() as f32 / 4.0).round() as usize + 1);
        while !self.is_at_end() && self.peek().token_type != EoF {
            stmts.push(self.stmt()?);
        }
        stmts.shrink_to_fit();
        Ok(stmts)
    }
    //
    // parse statements
    //
    pub fn stmt(&mut self) -> Result<Stmt<'a>, ParserError> {
        let token = self.peek();
        match &token.token_type {
            Keyword(k) => match k.as_str() {
                "if" => self.stmt_if(),
                "while" => self.stmt_while(),
                "print" => {
                    self.consume(Keyword("print".to_string()))?;
                    let expr = self.expr()?;
                    Ok(Stmt::Print(expr))
                }
                _ if self.is_type_kwd(k) => {
                    let type_ = self.consume(Keyword(k.clone()))?;
                    let name = self.consume(Identifier)?;
                    if self.check(&SingleChar('=')) {
                        self.stmt_var(name, type_)
                    } else {
                        self.stmt_fn(name, type_)
                    }
                }
                _ => Err(ParserError::UnexpectedToken(k.clone())),
            },
            SingleChar(c) if *c == '{' => Ok(Stmt::Block {
                stmts: self.stmt_block()?,
                rules: &[],
            }),
            _ => Ok(self.expr_stmt()?),
        }
    }

    fn stmt_var(&mut self, name: Token<'a>, type_: Token<'a>) -> Result<Stmt<'a>, ParserError> {
        self.consume(SingleChar('='))?;
        let is_const = self.match_token(SingleChar('='));
        let value = self.expr()?;

        Ok(Stmt::Var {
            id: name,
            type_,
            value,
            is_const,
            is_pub: false,
            rules: &[],
        })
    }

    fn stmt_fn(&mut self, name: Token<'a>, type_: Token<'a>) -> Result<Stmt<'a>, ParserError> {
        self.consume(SingleChar('('))?;
        let mut params = Vec::with_capacity(4);
        while !self.check(&SingleChar(')')) {
            if self.is_type_kwd(self.peek().lexeme) {
                let type_ = self.consume(Keyword(self.peek().lexeme.to_string()))?;
                let param_name = self.consume(Identifier)?;
                params.push((param_name, type_));
                if !self.match_token(SingleChar(',')) {
                    break;
                }
            } else {
                return Err(ParserError::UnexpectedToken(self.peek().lexeme.to_string()));
            }
        }
        self.consume(SingleChar(')'))?;
        let body = self.stmt_block()?;
        Ok(Stmt::Fn {
            id: name,
            type_,
            params,
            body,
            is_pub: false,
            rules: &[],
        })
    }

    fn stmt_if(&mut self) -> Result<Stmt<'a>, ParserError> {
        self.consume(Keyword("if".to_string()))?;
        let pred = self.expr()?;
        let body = self.stmt()?.into();
        let else_b = if self.match_token(Keyword("else".to_string())) {
            Some(self.stmt()?.into())
        } else {
            None
        };
        Ok(Stmt::If { pred, body, else_b })
    }

    fn stmt_while(&mut self) -> Result<Stmt<'a>, ParserError> {
        self.consume(Keyword("while".to_string()))?;
        let pred = self.expr()?;
        let body = self.stmt()?.into();
        Ok(Stmt::While { pred, body })
    }

    fn stmt_block(&mut self) -> Result<Vec<Stmt<'a>>, ParserError> {
        self.consume(SingleChar('{'))?;
        let mut stmts = Vec::with_capacity(8);
        while !self.check(&SingleChar('}')) && !self.is_at_end() {
            stmts.push(self.stmt()?);
        }
        self.consume(SingleChar('}'))?;
        Ok(stmts)
    }
    pub fn expr_stmt(&mut self) -> Result<Stmt<'a>, ParserError> {
        let expr = self.expr()?;
        Ok(Stmt::Expr(expr))
    }
    //
    // parse expressions
    //
    pub fn expr(&mut self) -> Result<Expr<'a>, ParserError> {
        self.binary()
    }

    pub fn binary(&mut self) -> Result<Expr<'a>, ParserError> {
        let mut expr = self.unary()?;
        while !self.is_at_end() && self.match_any(&[
            SingleChar('+'),
            SingleChar('-'),
            SingleChar('*'),
            SingleChar('/'),
        ])
        {
            let op = self.prev(1);
            let rhs = self.unary()?;
            expr = Expr::Binary {
                id: self.next_id(),
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
            };
        }
        Ok(expr)
    }

    pub fn unary(&mut self) -> Result<Expr<'a>, ParserError> {
        if self.match_any(&[
            DblChar(('+', '+')),
            DblChar(('-', '-')),
            SingleChar('-'),
            SingleChar('&')
        ]) {
            let op = self.prev(1);
            let rhs = self.unary()?;
            Ok(Expr::Unary {
                id: self.next_id(),
                op,
                rhs: Box::new(rhs),
            })
        } else {
            self.call()
        }
    }

    pub fn call(&mut self) -> Result<Expr<'a>, ParserError> {
        let mut expr = self.primary()?;
        if self.check(&SingleChar('(')) {
            self.advance();
            let mut args = Vec::with_capacity(2);
            while !self.check(&SingleChar(')')) {
                args.push(self.expr()?);
                if !self.match_token(SingleChar(',')) {
                    break;
                }
            }
            self.consume(SingleChar(')'))?;
            expr = Expr::Call {
                name: Box::new(expr),
                args,
                id: self.next_id(),
            }
        }
        Ok(expr)
    }

    pub fn primary(&mut self) -> Result<Expr<'a>, ParserError> {
        let token = self.peek();
        match token.token_type {
            Identifier => {
                self.advance();

                if self.match_token(SingleChar('=')) {
                    let value = self.expr()?;
                    return Ok(Expr::Assign {
                        id: self.next_id(),
                        name: token,
                        value: Box::new(value),
                    });
                }

                Ok(Expr::Var {
                    id: self.next_id(),
                    name: token,
                })
            }
            SingleChar('(') => {
                self.advance();
                let expr = self.expr()?;
                self.consume(SingleChar(')'))?;
                Ok(Expr::Grouping {
                    id: self.next_id(),
                    expr: Box::new(expr),
                })
            }
            Literal(_) => {
                self.advance();
                Ok(Expr::Value {
                    id: self.next_id(),
                    value: self.token_to_literal(self.prev(1)),
                })
            }
            _ => Err(ParserError::UnexpectedToken(self.peek().lexeme.to_string()))
        }
    }

    pub fn token_to_literal(&mut self, token: Token<'a>) -> Value<'a> {
        match token.token_type {
            Literal(c) => c,
            _ => Value::Nil,
        }
    }
    //
    // parser helpers
    //
    fn consume_if_matches(&mut self, token_type: TokenType) -> Option<Token<'a>> {
        if self.check(&token_type) {
            Some(self.advance())
        } else {
            None
        }
    }

    fn is_type_kwd(&self, k: &'a str) -> bool {
        // pull the custom types from the memory in the future
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
                | "im32"
                | "im64"
                | "im"
        )
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
            SingleChar(c) => current_token.lexeme == c.to_string(),
            DblChar((c1, c2)) => current_token.lexeme == format!("{}{}", c1, c2),
            Keyword(kw) => current_token.lexeme == *kw,
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
            token_type: EoF,
            lexeme: "\0",
            line: 0,
            value: None,
            pos: 0,
            length: 0,
        }
    }

    pub fn match_token(&mut self, token_type: TokenType<'a>) -> bool {
        if self.check(&token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn match_any(&mut self, token_types: &[TokenType<'a>]) -> bool {
        for token_type in token_types {
            if self.match_token(token_type.clone()) {
                return true;
            }
        }
        false
    }
    pub fn match_any_token(&mut self, token_types: &[TokenType<'a>]) -> Option<TokenType<'a>> {
        for token_type in token_types {
            if self.match_token(token_type.clone()) {
                return Some(token_type.clone());
            }
        }
        None
    }
}
