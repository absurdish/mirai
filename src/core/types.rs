use std::process::exit;
use crate::core::scanner::{TokenType, Value};
pub fn type_check(type_: TokenType, value: &Value) -> bool {
    match type_ {
        TokenType::Keyword(ref keyword) => match *keyword {
            "int" | "i32" => matches!(value, Value::Int(_)),
            "i64" => matches!(value, Value::Int64(_)) | matches!(value, Value::Int(_)),
            "unt" | "u32" => matches!(value, Value::Unt(_)),
            "u64" => matches!(value, Value::Unt64(_)) | matches!(value, Value::Unt(_)),
            "flt" | "f32" => matches!(value, Value::Flt(_)),
            "f64" => matches!(value, Value::F64(_)) | matches!(value, Value::Flt(_)),
            "im" | "im32" => matches!(value, Value::Im32(_)),
            "im64" => matches!(value, Value::Im64(_)),
            "str" => matches!(value, Value::Str(_)),
            "chr" => matches!(value, Value::Chr(_)),
            "bol" => matches!(value, Value::Bol(_)),
            "nil" => matches!(value, Value::Nil),
            _ => {
                eprintln!("invalid type token");
                false
            }
        },
        _ => {
            eprintln!("invalid type token");
            false
        }
    }
        .then(|| true)
        .unwrap_or_else(|| {
            eprintln!("type mismatch: for {:?}", value);
            exit(0);
        })
}
