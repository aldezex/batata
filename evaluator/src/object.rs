#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => i.to_string(),
            Object::Boolean(b) => b.to_string(),
            Object::Null => "null".to_string(),
        }
    }

    pub fn type_name(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::Null => "NULL".to_string(),
        }
    }
}
