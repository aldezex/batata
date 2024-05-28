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
    Function(Function),
    Block(Block),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Statement::Expression(expression) => write!(f, "{}", expression),
            Statement::Definition(definition) => write!(f, "{}", definition),
            Statement::Function(function) => write!(f, "{}", function),
            Statement::Block(block) => write!(f, "{}", block),
        }
    }
}

pub struct Definition {
    pub name: String,
    pub value: Expression,
}

impl std::fmt::Display for Definition {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
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

pub struct Infix {
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

pub struct Function {
    pub name: String,
    pub parameters: Vec<Definition>,
    pub body: Block,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut parameters = String::new();

        for parameter in &self.parameters {
            parameters.push_str(&parameter.to_string());
        }

        write!(f, "fn {}({}) {}", self.name, parameters, self.body)
    }
}

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
