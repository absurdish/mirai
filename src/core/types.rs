use crate::ast::{
    texp::TExpr,
    LitValue::{self, *},
};

use super::interpreter::RunTimeError;

impl LitValue {
    pub fn type_check(&self, texpr: TExpr) -> Result<LitValue, RunTimeError> {
        if let TExpr::Literal(name) = texpr {
            match self {
                Int(_) => name == "int",
                Unt(_) => name == "unt",
                Flt(_) => name == "flt",
                Bool(_) => name == "bool",
                Str(_) => name == "str",
                HeapRef(_) | Fun(_) => true,
                Void => name == "void",
                Nil => name == "nil",
            };
        }
        unimplemented!()
    }
}
