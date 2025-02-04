use std::collections::HashMap;

use super::interpreter::RunTimeError;
use crate::ast::LitValue::{self, *};

pub fn map(args: Vec<LitValue>) -> HashMap<&'static str, Result<LitValue, RunTimeError>> {
    HashMap::from([
        ("unt!", unt_(args[0].clone())),
        ("int!", int_(args[0].clone())),
    ])
}

/// casting function
///
/// unt!: int | unt | flt -> unt
fn unt_(a: LitValue) -> Result<LitValue, RunTimeError> {
    match a {
        Int { kind, owner } => Ok(Unt {
            kind: kind as u64,
            owner,
        }),
        Flt { kind, owner } => Ok(Unt {
            kind: kind as u64,
            owner,
        }),
        Unt { kind, owner } => Ok(Unt { kind, owner }),
        _ => Err(RunTimeError::OperationNotAllowed(a, "unt! cast")),
    }
}

/// casting function
///
/// int!: int | unt | flt -> int
fn int_(a: LitValue) -> Result<LitValue, RunTimeError> {
    match a {
        Int { kind, owner } => Ok(Int { kind, owner }),
        Flt { kind, owner } => Ok(Int {
            kind: kind as i64,
            owner,
        }),
        Unt { kind, owner } => Ok(Int {
            kind: kind as i64,
            owner,
        }),
        _ => Err(RunTimeError::OperationNotAllowed(a, "int! cast")),
    }
}
