use super::{
    Ast, AstError, LitValue, Token,
    TokenType::{self, *},
};

/// literal expressions that than can be evaluated into the literal values
#[derive(Debug, PartialEq, Clone)]
pub enum LExpr {
    /// assign operator assigns literal values to the already defined variables
    Assign {
        id: usize,
        name: Token,
        lit: Box<LExpr>,
        kind: AssignKind,
    },
    /// variable holding expression
    Var {
        id: usize,
        name: Token,
    },
    /// function caller
    Call {
        id: usize,
        name: Box<LExpr>,
        args: Vec<LExpr>,
    },
    /// unary operation: {op}{lexpr}
    Unary {
        id: usize,
        op: Token,
        lhs: Box<LExpr>,
    },
    /// binary operation: {lexpr}{op}{lexpr}
    Binary {
        id: usize,
        op: Token,
        lhs: Box<LExpr>,
        rhs: Box<LExpr>,
    },
    /// grouping: ({lexpr})
    Grouping {
        id: usize,
        expr: Box<LExpr>,
    },
    /// literal value
    Value {
        id: usize,
        lit: LitValue,
    },
    Null,
}

/// possible kinds of assignements
#[derive(Debug, PartialEq, Clone)]
pub enum AssignKind {
    Eq,    // id = value
    Plus,  // id += value
    Minus, // id -= value
    Mult,  // id *= value
    Div,   // id /= value
    /// dynamic assignement declares a new variable if target wasn't found and dynamically assigns a type to it
    Dynamic, // id := value
}

impl Ast {
    /// entry point of the literal expression parser, parses literal binary operation expression
    #[inline]
    pub fn lexpr(&mut self) -> Result<LExpr, AstError> {
        let mut expr = self.unary()?;
        while !self.is_at_end() && self.match_binary_op() {
            expr = LExpr::Binary {
                id: self.next_id(),
                op: self.prev(1)?,
                lhs: Box::new(expr),
                rhs: Box::new(self.unary()?),
            }
        }

        Ok(expr)
    }

    #[inline]
    fn match_binary_op(&mut self) -> bool {
        self.match_any(&[
            Char('+'),
            Char('-'),
            Char('*'),
            Char('/'),
            Char('>'),
            Char('<'),
            Char('%'),
            Chars(('>', '=')),
            Chars(('<', '=')),
            Chars(('=', '=')),
            Chars(('!', '=')),
        ])
    }

    #[inline]
    fn unary(&mut self) -> Result<LExpr, AstError> {
        if self.match_unary_op() {
            return Ok(LExpr::Unary {
                id: self.next_id(),
                op: self.prev(1)?,
                lhs: Box::new(self.call()?),
            });
        }
        self.call()
    }

    #[inline]
    fn match_unary_op(&mut self) -> bool {
        self.match_any(&[Char('&'), Char('*'), Char('-'), Char('!')])
    }

    #[inline]
    fn call(&mut self) -> Result<LExpr, AstError> {
        let mut expr = self.primary()?;
        if self.check(TokenType::Char('(')) {
            self.advances()?;
            let mut args = Vec::with_capacity(2);
            while !self.check(TokenType::Char(')')) {
                args.push(self.lexpr()?);
                if !self.match_token(TokenType::Char(',')) {
                    break;
                }
            }
            self.consumes(TokenType::Char(')'))?;
            expr = LExpr::Call {
                id: self.next_id(),
                name: Box::new(expr),
                args,
            }
        }
        Ok(expr)
    }

    #[inline]
    fn primary(&mut self) -> Result<LExpr, AstError> {
        match self.peeks()?.token {
            TokenType::Identifier => {
                // variable token
                let name = self.advances()?;

                // handle assignements
                match self.peeks()?.token {
                    TokenType::Char(c) if c == '=' => {
                        self.advances()?;
                        return Ok(LExpr::Assign {
                            id: self.next_id(),
                            name,
                            lit: Box::new(self.lexpr()?),
                            kind: AssignKind::Eq,
                        });
                    }
                    TokenType::Chars((l, r)) if r == '=' => {
                        let mut kind = AssignKind::Eq;
                        match l {
                            '+' => kind = AssignKind::Plus,
                            '-' => kind = AssignKind::Minus,
                            '/' => kind = AssignKind::Div,
                            '*' => kind = AssignKind::Mult,
                            ':' => kind = AssignKind::Dynamic,
                            _ => {}
                        }
                        self.advances()?;
                        return Ok(LExpr::Assign {
                            id: self.next_id(),
                            name,
                            lit: Box::new(self.lexpr()?),
                            kind,
                        });
                    }
                    _ => {}
                }

                Ok(LExpr::Var {
                    id: self.next_id(),
                    name,
                })
            }
            TokenType::Char('(') => {
                self.advances()?;
                let expr = self.lexpr()?;
                self.consumes(TokenType::Char(')'))?;
                Ok(LExpr::Grouping {
                    id: self.next_id(),
                    expr: Box::new(expr),
                })
            }
            TokenType::Literal(_) => {
                let token = self.advances()?;
                Ok(LExpr::Value {
                    id: self.next_id(),
                    lit: if let TokenType::Literal(l) = token.token {
                        l
                    } else {
                        LitValue::Nil
                    },
                })
            }
            _ => Err(AstError::UnexpectedToken(self.peeks()?.lexeme)),
        }
    }
}
