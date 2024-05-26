/*
use crate::ast::standard::{BlockStatement, Identifier, Statement};

use super::environment::Environment;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Error(String),
    Function(Function),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => i.to_string(),
            Object::Boolean(b) => b.to_string(),
            Object::Null => "null".to_string(),
            Object::Return(o) => o.inspect(),
            Object::Error(s) => "ERROR: ".to_string() + s,
            Object::Function(f) => {
                let mut params = Vec::new();

                for param in &f.parameters {
                    params.push(param.to_string());
                }

                format!("fn({}) {{\n{}\n}}", params.join(", "), f.body)
            }
        }
    }

    pub fn type_name(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::Null => "NULL".to_string(),
            Object::Return(o) => o.type_name(),
            Object::Error(_) => "ERROR".to_string(),
            Object::Function(_) => "FUNCTION".to_string(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}
*/
