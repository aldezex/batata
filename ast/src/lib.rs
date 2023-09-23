use lexer::token::Token;

pub struct Program {
    pub statements: Vec<Statement>,
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut program = String::new();

        for statement in &self.statements {
            program.push_str(&statement.to_string());
        }

        write!(f, "{}", program)
    }
}

pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Statement::LetStatement(statement) => write!(f, "{}", statement),
            Statement::ReturnStatement(statement) => write!(f, "{}", statement),
        }
    }
}

pub struct LetStatement {
    pub identifier: String,
    // pub expression: Expression,
}

impl std::fmt::Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "let {} ", self.identifier)
    }
}

pub struct ReturnStatement {
    pub token: Token,
}

impl std::fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{};", self.token)
    }
}

#[derive(PartialEq)]
pub struct Identifier {
    pub token: String,
    pub value: String,
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::LetStatement(LetStatement {
                identifier: String::from("ident"),
            })],
        };

        assert_eq!(program.to_string(), "let ident");
    }
}
