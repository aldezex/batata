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
    BlockStatement(BlockStatement),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Statement::LetStatement(statement) => write!(f, "{}", statement),
            Statement::ReturnStatement(statement) => write!(f, "{}", statement),
            Statement::ExpressionStatement(statement) => write!(f, "{}", statement),
            Statement::BlockStatement(statement) => write!(f, "{}", statement),
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

#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl std::fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut block_statement = String::new();

        for statement in &self.statements {
            block_statement.push_str(&statement.to_string());
        }

        write!(f, "{}", block_statement)
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
    Boolean(bool),
    IfExpression(IfExpression),
}

#[derive(Debug, PartialEq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
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
            Expression::Boolean(expression) => write!(f, "{}", expression),
            Expression::IfExpression(expression) => write!(f, "{}", expression),
        }
    }
}

impl std::fmt::Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut if_expression = String::new();

        if_expression.push_str("if");
        if_expression.push_str(&self.condition.to_string());
        if_expression.push_str(" ");
        if_expression.push_str(&self.consequence.to_string());

        if let Some(alternative) = &self.alternative {
            if_expression.push_str("else ");
            if_expression.push_str(&alternative.to_string());
        }

        write!(f, "{}", if_expression)
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
