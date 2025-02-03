use super::{
    Ast, AstError, LitValue, Token,
    TokenType::{self, *},
};

/// literal expressions, evaluated into the literal valuesI
#[derive(Debug, PartialEq, Clone)]
pub enum LExpr {
    /// assignement expression assigns values to the variables
    /// ```
    /// int x = 0
    /// x = 1
    /// x += 1
    /// x -= 1
    /// x *= 2
    /// x /= 1
    /// x := 3 // declares a new variable if variable can't be found
    /// ```
    Assign {
        id: usize,
        name: Token,
        lit: Box<LExpr>,
        kind: AssignKind,
    },
    /// identifier
    /// ```
    /// x
    /// ```
    Var { id: usize, name: Token },
    /// function call expression
    /// ```
    /// myfunc(x, 3)
    /// ```
    Call {
        id: usize,
        name: Box<LExpr>,
        args: Vec<LExpr>,
    },
    /// unary operations, with operator on the left hand side
    /// ```
    /// -4
    /// &x
    /// *x
    /// ```
    Unary {
        id: usize,
        op: Token,
        lhs: Box<LExpr>,
    },
    /// binary operation, with literal expressions on the both sides of the opreator
    /// ```
    /// 2 + 2
    /// 5 / 2
    /// 7 * 7
    /// ```
    Binary {
        id: usize,
        op: Token,
        lhs: Box<LExpr>,
        rhs: Box<LExpr>,
    },
    /// mathematical grouping
    /// ```
    /// 2 * ( 2 + 2)
    /// (2 + 2)*(4 + 4)
    /// ```
    Grouping { id: usize, expr: Box<LExpr> },
    /// a vector type
    ///
    /// [1, 2, 3, 4]
    Vector { id: usize, elems: Vec<LExpr> },
    /// expression for literal values
    Value { id: usize, lit: LitValue },
}

/// kinds of assignement
#[derive(Debug, PartialEq, Clone)]
pub enum AssignKind {
    /// `id = lit`
    Eq,
    /// `id += lit`
    Plus,
    /// `id -= lit`
    Minus,
    /// `id *= lit`
    Mult,
    /// `id /= lit`
    Div,
    /// `id := lit`
    Dynamic,
}

impl Ast {
    /// parse literal expressions
    pub fn lexpr(&mut self) -> Result<LExpr, AstError> {
        let mut expr = self.unary()?;
        while self.match_binary_op() {
            let op = self.prev(1)?;
            let rhs = self.unary()?;
            expr = LExpr::Binary {
                id: self.next_id(),
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            };
        }
        Ok(expr)
    }

    /// list of binary operations
    fn match_binary_op(&mut self) -> bool {
        self.match_any(&[
            Char('+'),
            Char('-'),
            Char('*'),
            Char('/'),
            Char('%'),
            Char('>'),
            Char('<'),
            Chars(('>', '=')),
            Chars(('<', '=')),
            Chars(('=', '=')),
            Chars(('!', '=')),
        ])
    }

    /// unary operation parser
    fn unary(&mut self) -> Result<LExpr, AstError> {
        if self.match_unary_op() {
            let op = self.prev(1)?;
            let lhs = self.call()?;
            return Ok(LExpr::Unary {
                id: self.next_id(),
                op,
                lhs: Box::new(lhs),
            });
        }
        self.call()
    }

    /// list of unary operations
    fn match_unary_op(&mut self) -> bool {
        self.match_any(&[Char('&'), Char('*'), Char('-'), Char('!')])
    }

    /// function caller parser
    fn call(&mut self) -> Result<LExpr, AstError> {
        let mut expr = self.primary()?;
        while self.match_token(Char('(')) {
            let mut args = Vec::new();
            while !self.check(Char(')')) {
                args.push(self.lexpr()?);
                if !self.match_token(Char(',')) {
                    break;
                }
            }
            self.consumes(Char(')'))?;
            expr = LExpr::Call {
                id: self.next_id(),
                name: Box::new(expr),
                args,
            };
        }
        Ok(expr)
    }

    /// primary (primitive) expressions
    fn primary(&mut self) -> Result<LExpr, AstError> {
        match self.peeks()?.token {
            Identifier => {
                let name = self.advances()?;
                let kind = match self.peeks()?.token {
                    Char('=') => {
                        self.advances()?;
                        AssignKind::Eq
                    }
                    Chars(('+', '=')) => {
                        self.advances()?;
                        AssignKind::Plus
                    }
                    Chars(('-', '=')) => {
                        self.advances()?;
                        AssignKind::Minus
                    }
                    Chars(('*', '=')) => {
                        self.advances()?;
                        AssignKind::Mult
                    }
                    Chars(('/', '=')) => {
                        self.advances()?;
                        AssignKind::Div
                    }
                    Chars((':', '=')) => {
                        self.advances()?;
                        AssignKind::Dynamic
                    }
                    _ => {
                        return Ok(LExpr::Var {
                            id: self.next_id(),
                            name,
                        });
                    }
                };
                let rhs = self.lexpr()?;
                Ok(LExpr::Assign {
                    id: self.next_id(),
                    name,
                    lit: Box::new(rhs),
                    kind,
                })
            }
            Char('(') => {
                self.advances()?;
                let expr = self.lexpr()?;
                self.consumes(Char(')'))?;
                Ok(LExpr::Grouping {
                    id: self.next_id(),
                    expr: Box::new(expr),
                })
            }
            Char('[') => {
                self.advances()?;
                let mut elems = Vec::with_capacity(4);
                if !self.check(Char(']')) {
                    loop {
                        let elem = self.lexpr()?;
                        elems.push(elem);
                        if !self.match_token(Char(',')) {
                            break;
                        }
                    }
                }
                self.consumes(Char(']'))?;
                Ok(LExpr::Vector {
                    id: self.next_id(),
                    elems,
                })
            }
            Literal(_) => {
                let token = self.advances()?;
                let TokenType::Literal(lit) = token.token else {
                    unreachable!("Encountered non-literal token after match");
                };
                Ok(LExpr::Value {
                    id: self.next_id(),
                    lit,
                })
            }
            _ => Err(AstError::UnexpectedToken(self.peeks()?.lexeme)),
        }
    }
}
