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

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(Expression),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Statement::LetStatement(statement) => write!(f, "{}", statement),
            Statement::ReturnStatement(statement) => write!(f, "{}", statement),
            Statement::ExpressionStatement(statement) => write!(f, "{}", statement),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub identifier: String,
    pub expression: Option<Expression>,
}

impl std::fmt::Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "let {} ", self.identifier)
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub expression: Expression,
}

impl std::fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{};", self.expression)
    }
}

#[derive(PartialEq, Debug)]
pub struct Identifier {
    pub token: String,
    pub value: String,
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    StringLiteral(String),
    IntegerLiteral(i64),
    Prefix(Prefix),
    Infix(Infix),
    Empty,
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Identifier(expression) => write!(f, "{}", expression),
            Expression::IntegerLiteral(expression) => write!(f, "{}", expression),
            Expression::StringLiteral(expression) => write!(f, "{}", expression),
            Expression::Prefix(expression) => write!(f, "{}", expression),
            Expression::Infix(expression) => write!(f, "{}", expression),
            Expression::Empty => write!(f, ""),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Prefix {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl std::fmt::Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, PartialEq)]
pub struct Infix {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl std::fmt::Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}
