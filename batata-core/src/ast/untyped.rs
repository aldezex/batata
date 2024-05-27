pub struct Parsed {
    pub module: Module,
}

pub struct Module {
    pub statements: Vec<Statement>,
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut program = String::new();

        for statement in &self.statements {
            program.push_str(&statement.to_string());
        }

        write!(f, "{}", program)
    }
}

pub enum Statement {
    Expression(Expression),
    Definition(Definition),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Statement::Expression(expression) => write!(f, "{}", expression),
            Statement::Definition(definition) => write!(f, "{}", definition),
        }
    }
}

pub struct Definition {
    pub name: String,
    pub value: Expression,
}

impl std::fmt::Display for Definition {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "let {} = {}", self.name, self.value)
    }
}

pub struct Expression {
    pub kind: ExpressionKind,
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

pub enum ExpressionKind {
    Integer(String),
    Infix(Infix),
}

impl std::fmt::Display for ExpressionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ExpressionKind::Integer(value) => write!(f, "{}", value),
            ExpressionKind::Infix(infix) => {
                write!(f, "({} {} {})", infix.left, infix.operator, infix.right)
            }
        }
    }
}

pub struct Infix {
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}
