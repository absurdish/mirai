use super::{lexp::LExpr, texp::TExpr, Ast, AstError, Token, TokenType::*};
use smallvec::SmallVec;

#[derive(Debug)]
pub enum Stmt {
    Print(LExpr),
    Var {
        name: Token,
        texpr: TExpr,
        lexpr: LExpr,
        is_const: bool,
    },
    Fn {
        name: Token,
        texpr: TExpr,
        body: Vec<Stmt>,
        params: SmallVec<[(Token, TExpr); 4]>,
    },
    If {
        pred: LExpr,
        body: Box<Stmt>,
        else_body: Option<Box<Stmt>>,
    },
    While {
        pred: LExpr,
        body: Box<Stmt>,
    },
    Return(LExpr),
    Break,
    Block(Vec<Stmt>),
    Expr(LExpr),
}

impl Ast {
    pub fn parse(&mut self) -> Result<Vec<Stmt>, AstError> {
        let mut stmts = Vec::with_capacity((self.tokens.len() >> 2) + 1);
        while !self.is_at_end() && self.peeks()?.token != EoF {
            stmts.push(self.stmt()?);
        }
        Ok(stmts)
    }

    pub fn stmt(&mut self) -> Result<Stmt, AstError> {
        match &self.peeks()?.token {
            Keyword(kwd) => self.stmts(kwd),
            Char('{') => Ok(Stmt::Block(self.stmt_block()?)),
            _ => Ok(Stmt::Expr(self.lexpr()?)),
        }
    }

    #[inline]
    fn stmts(&mut self, kwd: &'static str) -> Result<Stmt, AstError> {
        match kwd {
            "if" => {
                self.advances()?;
                let pred = self.lexpr()?;
                let body = Box::new(self.stmt()?);
                let else_body = if self.match_token(Keyword("else")) {
                    Some(Box::new(self.stmt()?))
                } else {
                    None
                };
                Ok(Stmt::If {
                    pred,
                    body,
                    else_body,
                })
            }
            "while" => {
                self.advances()?;
                Ok(Stmt::While {
                    pred: self.lexpr()?,
                    body: Box::new(self.stmt()?),
                })
            }
            "return" => {
                self.advances()?;
                Ok(Stmt::Return(self.lexpr()?))
            }
            "print" => {
                self.advances()?;
                Ok(Stmt::Print(self.lexpr()?))
            }
            "break" => {
                self.advances()?;
                Ok(Stmt::Break)
            }
            _ if self.is_type_kwd(kwd) => {
                let texpr = self.texpr()?;
                let name = self.consumes(Identifier)?;
                if self.check(Char('=')) || self.check(Chars(('=', '='))) {
                    let is_const = self.check(Chars(('=', '=')));
                    self.advances()?;
                    Ok(Stmt::Var {
                        name,
                        texpr,
                        lexpr: self.lexpr()?,
                        is_const,
                    })
                } else {
                    self.consumes(Char('('))?;
                    let mut params: SmallVec<[(Token, TExpr); 4]> = SmallVec::with_capacity(4);
                    if !self.check(Char(')')) {
                        loop {
                            if self.is_type_kwd(self.peeks()?.lexeme) {
                                let texpr = self.texpr()?;
                                let name = self.consumes(Identifier)?;
                                params.push((name, texpr));
                                if !self.match_token(Char(',')) {
                                    break;
                                }
                            } else {
                                return Err(AstError::UnexpectedToken(self.peeks()?.lexeme));
                            }
                        }
                    }
                    self.consumes(Char(')'))?;
                    Ok(Stmt::Fn {
                        name,
                        texpr,
                        body: self.stmt_block()?,
                        params,
                    })
                }
            }
            _ => Err(AstError::UnexpectedToken(kwd)),
        }
    }

    #[inline]
    fn stmt_block(&mut self) -> Result<Vec<Stmt>, AstError> {
        self.consumes(Char('{'))?;
        let mut stmts = Vec::with_capacity(8);
        while !self.check(Char('}')) && !self.is_at_end() {
            stmts.push(self.stmt()?);
        }
        self.consumes(Char('}'))?;
        Ok(stmts)
    }
}
