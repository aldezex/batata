use lexer::token::Token;

pub struct Program {
    pub statements: Vec<Statement>,
}

pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
}

pub enum Expression {
    Identifier(Identifier),
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Expression>,
}

pub struct ReturnStatement {
    pub token: Token,
}

#[derive(PartialEq)]
pub struct Identifier {
    pub token: String,
    pub value: String,
}
