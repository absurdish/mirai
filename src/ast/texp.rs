use super::{Ast, AstError, TokenType};

#[derive(Debug, PartialEq, Clone)]
pub enum TExpr {
    Literal(&'static str),
    Deref(Box<TExpr>),
}

impl Ast {
    #[inline]
    pub fn texpr(&mut self) -> Result<TExpr, AstError> {
        match self.peeks()?.token {
            TokenType::Keyword(kwd) if self.is_type_kwd(kwd) => {
                let name = self.advances()?;

                if self.peeks()?.lexeme == "*" {
                    return Ok(TExpr::Deref(Box::new(self.texpr()?)));
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
