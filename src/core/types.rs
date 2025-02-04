use super::interpreter::RunTimeError;
use crate::ast::{
    texp::TExpr,
    LitValue::{self, *},
};

impl LitValue {
    pub fn type_check(&self, texpr: TExpr) -> Result<LitValue, RunTimeError> {
        let mut nonlit = false;
        if let TExpr::Literal(name) = texpr {
            let checks = match self {
                Int { .. } => name == "int",
                Unt { .. } => name == "unt",
                Flt { .. } => name == "flt",
                Bool { .. } => name == "bool",
                Str { .. } => name == "str",
                ImInt { .. } => name == "imi",
                ImUnt { .. } => name == "imu",
                ImFlt { .. } => name == "imf",
                HeapRef(_) | Fun(_) | Map(_) => true,
                Void => name == "void",
                Nil => name == "nil",
                Vector { .. } => {
                    nonlit = true;
                    false
                }
            };

            if !nonlit {
                return if !checks {
                    Err(RunTimeError::TypeError(self.clone(), texpr))
                } else {
                    Ok(self.clone())
                };
            }
        } else if let TExpr::Vec(inner_type) = texpr.clone() {
            return match self {
                Vector { kind, .. } => kind[0].type_check(*inner_type),
                _ => Err(RunTimeError::TypeError(self.clone(), texpr)),
            };
        }
        Ok(self.clone())
    }
}
