use super::{Ast, AstError, TokenType::*};

#[derive(Debug, PartialEq, Clone)]
pub enum TExpr {
    /// int, unt, str, ...
    Literal(&'static str),
    /// type[], int[], unt[], ...
    Vec(Box<TExpr>),
    /// type*, int*, int[]*, ...
    Deref(Box<TExpr>),
}

impl Ast {
    #[inline]
    pub fn texpr(&mut self) -> Result<TExpr, AstError> {
        match self.peeks()?.token {
            Keyword(kwd) if self.is_type_kwd(kwd) => {
                let name = self.advances()?;
                let peek = self.peeks()?.lexeme;

                if peek == "*" {
                    return Ok(TExpr::Deref(Box::new(self.texpr()?)));
                } else if peek == "[" {
                    self.advances()?;
                    self.consumes(Char(']'))?;
                  
                    return Ok(TExpr::Vec(Box::new(TExpr::Literal(name.lexeme))));
                }
                Ok(TExpr::Literal(name.lexeme))
            }
            _ => Err(AstError::UnexpectedToken(self.peeks()?.lexeme)),
        }
    }

    #[inline]
    pub fn is_type_kwd(&self, kwd: &str) -> bool {
        matches!(kwd, "int" | "unt" | "flt" | "bool" | "str" | "nil" | "void")
    }
}
