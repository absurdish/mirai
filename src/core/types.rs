use std::process::exit;
use crate::core::scanner::{TokenType, Value};

pub fn type_check(type_: TokenType, value: &Value) -> bool {
    let mut lex = String::new();
    let status = if let TokenType::Keyword(keyword) = type_ {
        lex = keyword.clone();
        match keyword.as_str() {
            "int" | "i16" => matches!(value, Value::Int(_)),
            "i32" => matches!(value, Value::Int32(_)),
            "unt" | "u16" => matches!(value, Value::Unt(_)),
            "u32" => matches!(value, Value::Unt32(_)),
            "flt" | "f32" => matches!(value, Value::Flt(_)),
            "f64" => matches!(value, Value::F64(_)),
            "im" | "im32" => matches!(value, Value::Im32(_)),
            "im64" => matches!(value, Value::Im64(_)),
            "str" => matches!(value, Value::Str(_)),
            "chr" => matches!(value, Value::Chr(_)),
            "bol" => matches!(value, Value::Bol(_)),
            "nil" => matches!(value, Value::Nil),
            _ => false,
        }
    } else {
        eprintln!("invalid type token");
        false
    };

    if !status {
        eprintln!("type mismatch: expected '{}', got {:?}", lex, value);
        exit(0);
    }

    status
}