pub struct Parsed {
    pub module: Module,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Definition(Definition),
    Function(Function),
    Block(Block),
    If(If),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Statement::Expression(expression) => write!(f, "{}", expression),
            Statement::Definition(definition) => write!(f, "{}", definition),
            Statement::Function(function) => write!(f, "{}", function),
            Statement::Block(block) => write!(f, "{}", block),
            Statement::If(if_statement) => write!(f, "{}", if_statement),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub condition: Expression,
    pub consequence: Block,
    pub alternative: Option<Block>,
}

impl std::fmt::Display for If {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut if_statement = String::new();

        if_statement.push_str(&format!("if {} {}", self.condition, self.consequence));

        if let Some(alternative) = &self.alternative {
            if_statement.push_str(&format!(" else {}", alternative));
        }

        write!(f, "{}", if_statement)
    }
}

#[derive(Debug, PartialEq)]
pub struct Definition {
    pub name: String,
    pub value: Expression,
}

impl std::fmt::Display for Definition {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, PartialEq)]
pub enum ExpressionKind {
    Integer(String),
    Infix(Infix),
    Identifier(String),
    DiscardIdentifier(String),
}

impl std::fmt::Display for ExpressionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ExpressionKind::Integer(value) => write!(f, "{}", value),
            ExpressionKind::Infix(infix) => {
                write!(f, "({} {} {})", infix.left, infix.operator, infix.right)
            }
            ExpressionKind::Identifier(name) => write!(f, "{}", name),
            ExpressionKind::DiscardIdentifier(name) => write!(f, "_{}", name),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Infix {
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut parameters = String::new();

        for (i, parameter) in self.parameters.iter().enumerate() {
            parameters.push_str(&parameter.to_string());

            if i < self.parameters.len() - 1 {
                parameters.push_str(", ");
            }
        }

        write!(f, "fn {}({}) {}", self.name, parameters, self.body)
    }
}

impl std::fmt::Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut block = String::new();

        for statement in &self.statements {
            block.push_str(&statement.to_string());
        }

        write!(f, "{{{}}}", block)
    }
}
