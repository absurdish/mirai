use crate::core::env::{ApplyKind, Method};
use crate::core::scanner::{
    Token,
    TokenType::{self, *},
    Value,
};
use std::fmt;
use std::process::exit;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Assign {
        id: usize,
        name: Token<'a>,
        value: Box<Expr<'a>>,
    },
    Var {
        id: usize,
        name: Token<'a>,
        method: Option<(&'a str, Vec<Expr<'a>>)>,
    },
    Call {
        id: usize,
        name: Box<Expr<'a>>,
        args: Vec<Expr<'a>>,
    },
    Unary {
        id: usize,
        op: Token<'a>,
        rhs: Box<Expr<'a>>,
    },
    Binary {
        id: usize,
        op: Token<'a>,
        lhs: Box<Expr<'a>>,
        rhs: Box<Expr<'a>>,
    },
    Grouping {
        id: usize,
        expr: Box<Expr<'a>>,
    },
    Value {
        id: usize,
        value: Value<'a>,
    },
    Null,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum UseKinds<'a> {
    Str(&'a str),
    List(Vec<&'a str>),
}

#[allow(dead_code)]
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
    Method {
        id: Token<'a>,
        type_: Token<'a>,
        params: Vec<(&'a str, &'a str)>,
        body: Vec<Stmt<'a>>,
    },
    Impl {
        name: Token<'a>,
        body: Vec<Method<'a>>,
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
    Break,
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
    tokens: &'a [Token<'a>],
    current: usize,
    id_counter: usize,
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(tokens: &'a Vec<Token<'a>>) -> Self {
        Self {
            tokens: tokens.as_slice(),
            current: 0,
            id_counter: 0,
        }
    }

    #[inline]
    pub fn start(&mut self) -> Result<Vec<Stmt<'a>>, ParserError> {
        let mut stmts = Vec::with_capacity((self.tokens.len() >> 2) + 1);
        while !self.is_at_end() && self.peek().token_type != EoF {
            stmts.push(self.stmt()?);
        }
        Ok(stmts)
    }

    #[inline]
    fn stmt(&mut self) -> Result<Stmt<'a>, ParserError> {
        match &self.peek().token_type {
            Keyword(k) => self.handle_keyword(k),
            SingleChar('{') => Ok(Stmt::Block {
                stmts: self.stmt_block()?,
                rules: &[],
            }),
            _ => Ok(Stmt::Expr(self.expr()?)),
        }
    }

    #[inline]
    fn handle_keyword(&mut self, k: &'a str) -> Result<Stmt<'a>, ParserError> {
        match k {
            "if" => self.stmt_if(),
            "while" => self.stmt_while(),
            "impl" => self.stmt_impl(),
            "break" => {
                self.advance();
                Ok(Stmt::Break)
            }
            "self" => {
                self.advance();
                Ok(Stmt::Expr(Expr::Var {
                    id: self.id_counter,
                    name: Token {
                        lexeme: "self",
                        token_type: Keyword("self"),
                        value: None,
                        length: 4,
                    },
                    method: None,
                }))
            }
            "print" => {
                self.advance();
                let expr = self.expr()?;
                Ok(Stmt::Print(expr))
            }
            "return" => {
                self.advance();
                let expr = self.expr()?;
                Ok(Stmt::Return(expr))
            }
            _ if self.is_type_kwd(k) => {
                let type_ = self.advance();
                let name = self.consume(Identifier)?;
                if self.check(&SingleChar('=')) {
                    self.stmt_var(name, type_)
                } else {
                    self.stmt_fn(name, type_)
                }
            }
            _ => Err(ParserError::UnexpectedToken(k.to_string())),
        }
    }
    #[inline]
    fn stmt_var(&mut self, name: Token<'a>, type_: Token<'a>) -> Result<Stmt<'a>, ParserError> {
        self.advance(); // consume '='
        let value = self.expr()?;
        Ok(Stmt::Var {
            id: name,
            type_,
            value,
            is_const: false,
            is_pub: false,
            rules: &[],
        })
    }

    #[inline]
    fn stmt_fn(&mut self, name: Token<'a>, type_: Token<'a>) -> Result<Stmt<'a>, ParserError> {
        self.consume(SingleChar('('))?;
        let mut params = Vec::with_capacity(4);

        if !self.check(&SingleChar(')')) {
            loop {
                if self.is_type_kwd(self.peek().lexeme) {
                    let type_ = self.advance();
                    let param_name = self.consume(Identifier)?;
                    params.push((param_name, type_));
                    if !self.match_token(SingleChar(',')) {
                        break;
                    }
                } else {
                    return Err(ParserError::UnexpectedToken(self.peek().lexeme.to_string()));
                }
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

    #[inline]
    fn stmt_method(&mut self) -> Option<Method<'a>> {
        if let Keyword(k) = self.peek().token_type {
            if self.is_type_kwd(k) {
                let type_ = self.advance();
                let name = self.consume(Identifier).ok()?;
                self.consume(SingleChar('(')).ok()?;
                let mut params = vec![("self", type_.lexeme)];
                while !self.check(&SingleChar(')')) {
                    if self.is_type_kwd(self.peek().lexeme) {
                        let type_ = self.advance();
                        let param_name = self.consume(Identifier).ok()?;
                        params.push((param_name.lexeme, type_.lexeme));
                        if !self.match_token(SingleChar(',')) {
                            break;
                        }
                    } else {
                        return None;
                    }
                }
                self.consume(SingleChar(')')).ok()?;
                let body = self
                    .stmt_block()
                    .map_err(|e| {
                        println!("{}", e);
                        exit(1)
                    })
                    .ok()?;
                return Some(Method {
                    name: name.lexeme,
                    applies: ApplyKind::Normal(type_.lexeme),
                    type_: type_.lexeme,
                    params,
                    body: Rc::new(body),
                });
            }
        }
        None
    }

    #[inline]
    fn stmt_if(&mut self) -> Result<Stmt<'a>, ParserError> {
        self.advance();
        let pred = self.expr()?;
        let body = Box::new(self.stmt()?);
        let else_b = if self.match_token(Keyword("else")) {
            Some(Box::new(self.stmt()?))
        } else {
            None
        };
        Ok(Stmt::If { pred, body, else_b })
    }

    #[inline]
    fn stmt_while(&mut self) -> Result<Stmt<'a>, ParserError> {
        self.advance();
        Ok(Stmt::While {
            pred: self.expr()?,
            body: Box::new(self.stmt()?),
        })
    }

    #[inline]
    fn stmt_impl(&mut self) -> Result<Stmt<'a>, ParserError> {
        self.advance();
        let name = if let Keyword(k) = self.peek().token_type {
            if self.is_type_kwd(k) {
                self.advance()
            } else {
                return Err(ParserError::UnexpectedToken(k.to_string()));
            }
        } else {
            return Err(ParserError::UnexpectedToken(self.peek().lexeme.to_string()));
        };

        self.consume(SingleChar('{'))?;
        let mut methods = Vec::with_capacity(4);
        while !self.check(&SingleChar('}')) && !self.is_at_end() {
            if let Some(method) = self.stmt_method() {
                methods.push(method);
            }
        }
        self.consume(SingleChar('}'))?;
        Ok(Stmt::Impl {
            name,
            body: methods,
        })
    }

    #[inline]
    fn stmt_block(&mut self) -> Result<Vec<Stmt<'a>>, ParserError> {
        self.consume(SingleChar('{'))?;
        let mut stmts = Vec::with_capacity(8);

        while !self.check(&SingleChar('}')) && !self.is_at_end() {
            stmts.push(self.stmt()?);
        }

        self.consume(SingleChar('}'))?;
        Ok(stmts)
    }

    #[inline]
    fn expr(&mut self) -> Result<Expr<'a>, ParserError> {
        self.binary()
    }

    #[inline]
    fn binary(&mut self) -> Result<Expr<'a>, ParserError> {
        let mut expr = self.unary()?;
        while !self.is_at_end() && self.match_binary_op() {
            expr = Expr::Binary {
                id: self.next_id(),
                lhs: Box::new(expr),
                op: self.prev(1),
                rhs: Box::new(self.unary()?),
            };
        }
        Ok(expr)
    }

    #[inline]
    fn unary(&mut self) -> Result<Expr<'a>, ParserError> {
        if self.match_unary_op() {
            Ok(Expr::Unary {
                id: self.next_id(),
                op: self.prev(1),
                rhs: Box::new(self.unary()?),
            })
        } else {
            self.call()
        }
    }

    #[inline]
    fn call(&mut self) -> Result<Expr<'a>, ParserError> {
        let mut expr = self.primary()?;
        if self.check(&SingleChar('(')) {
            self.advance();
            let mut args = Vec::with_capacity(4);
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
            };
        }
        Ok(expr)
    }

    #[inline]
    fn primary(&mut self) -> Result<Expr<'a>, ParserError> {
        match self.peek().token_type {
            Identifier => {
                let token = self.advance();
                if self.match_token(SingleChar('=')) {
                    return Ok(Expr::Assign {
                        id: self.next_id(),
                        name: token,
                        value: Box::new(self.expr()?),
                    });
                }

                let method = if self.match_token(SingleChar('.')) {
                    let name = self.consume(Identifier)?;
                    self.consume(SingleChar('('))?;
                    let mut args = Vec::with_capacity(4);
                    while !self.check(&SingleChar(')')) {
                        args.push(self.expr()?);
                        if !self.match_token(SingleChar(',')) {
                            break;
                        }
                    }
                    self.consume(SingleChar(')'))?;
                    Some((name.lexeme, args))
                } else {
                    None
                };

                Ok(Expr::Var {
                    id: self.next_id(),
                    name: token,
                    method,
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
                let token = self.advance();
                Ok(Expr::Value {
                    id: self.next_id(),
                    value: match token.token_type {
                        Literal(c) => c,
                        _ => Value::Nil,
                    },
                })
            }
            _ => Err(ParserError::UnexpectedToken(self.peek().lexeme.to_string())),
        }
    }

    #[inline]
    fn match_binary_op(&mut self) -> bool {
        self.match_any(&[
            SingleChar('+'),
            SingleChar('-'),
            SingleChar('*'),
            SingleChar('/'),
            SingleChar('>'),
            SingleChar('<'),
            SingleChar('%'),
            DblChar(('>', '=')),
            DblChar(('<', '=')),
            DblChar(('=', '=')),
            DblChar(('!', '=')),
        ])
    }

    #[inline]
    fn match_unary_op(&mut self) -> bool {
        self.match_any(&[
            DblChar(('+', '+')),
            DblChar(('-', '-')),
            SingleChar('-'),
            SingleChar('&'),
        ])
    }

    #[inline]
    fn is_type_kwd(&self, k: &str) -> bool {
        matches!(
            k,
            "int"
                | "i64"
                | "i32"
                | "unt"
                | "u32"
                | "u64"
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

    #[inline]
    fn next_id(&mut self) -> usize {
        let id = self.id_counter;
        self.id_counter += 1;
        id
    }

    #[inline]
    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        if let Some(current_token) = self.tokens.get(self.current) {
            match token_type {
                SingleChar(c) => current_token.lexeme.chars().next() == Some(*c),
                DblChar((c1, c2)) => current_token.lexeme == &format!("{}{}", c1, c2),
                Keyword(kw) => &current_token.lexeme == kw,
                _ => &current_token.token_type == token_type,
            }
        } else {
            false
        }
    }

    #[inline]
    fn consume(&mut self, token_type: TokenType) -> Result<Token<'a>, ParserError> {
        if self.check(&token_type) {
            Ok(self.advance())
        } else {
            Err(ParserError::MissingToken("Token not found".to_string()))
        }
    }

    #[inline]
    fn advance(&mut self) -> Token<'a> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.prev(1)
    }

    #[inline]
    fn prev(&self, steps: usize) -> Token<'a> {
        if self.current >= steps {
            self.tokens
                .get(self.current - steps)
                .cloned()
                .unwrap_or_else(|| self.error_token())
        } else {
            self.error_token()
        }
    }

    #[inline]
    fn error_token(&self) -> Token<'a> {
        Token {
            token_type: EoF,
            lexeme: "\0",
            value: None,
            length: 0,
        }
    }

    #[inline]
    fn peek(&self) -> Token<'a> {
        self.tokens
            .get(self.current)
            .cloned()
            .unwrap_or_else(|| self.error_token())
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    #[inline]
    fn match_token(&mut self, token_type: TokenType<'a>) -> bool {
        if self.check(&token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    #[inline]
    fn match_any(&mut self, token_types: &[TokenType<'a>]) -> bool {
        for token_type in token_types {
            if self.match_token(token_type.clone()) {
                return true;
            }
        }
        false
    }
}
