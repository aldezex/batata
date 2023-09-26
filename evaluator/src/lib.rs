use ast::Node;
use object::Object;

pub mod object;

fn eval(node: Node) -> object::Object {
    match node {
        Node::Program(program) => evalStatements(program.statements),
        Node::Statement(statement) => match statement {
            ast::Statement::LetStatement(_) => todo!(),
            ast::Statement::ReturnStatement(_) => todo!(),
            ast::Statement::ExpressionStatement(expression) => {
                eval(ast::Node::Expression(expression))
            }
            ast::Statement::BlockStatement(_) => todo!(),
        },
        Node::Expression(expression) => match expression {
            ast::Expression::Identifier(_) => todo!(),
            ast::Expression::StringLiteral(_) => todo!(),
            ast::Expression::IntegerLiteral(i) => Object::Integer(i),
            ast::Expression::Prefix(_) => todo!(),
            ast::Expression::Infix(_) => todo!(),
            ast::Expression::Empty => todo!(),
            ast::Expression::Boolean(boolean) => Object::Boolean(boolean),
            ast::Expression::IfExpression(_) => todo!(),
            ast::Expression::FunctionLiteral(_) => todo!(),
            ast::Expression::CallExpression(_) => todo!(),
        },
    }
}

fn evalStatements(statements: Vec<ast::Statement>) -> object::Object {
    let mut result = object::Object::Null;

    for statement in statements {
        result = eval(ast::Node::Statement(statement));
    }

    result
}

#[cfg(test)]
mod tests {
    use anyhow::Result;

    use super::*;

    #[test]
    fn test_integer_expression() -> Result<()> {
        let tests = vec![("5", 5), ("10", 10)];

        for (input, expected) in tests {
            let lexer = lexer::Lexer::new(input.into());
            let mut parser = parser::Parser::new(lexer)?;
            let program = parser.parse_program()?;

            let evaluation = eval(ast::Node::Program(program));

            assert_eq!(evaluation, object::Object::Integer(expected));
        }

        Ok(())
    }

    #[test]
    fn test_boolean_literals() -> Result<()> {
        let tests = vec![("true", true), ("false", false)];

        for (input, expected) in tests {
            let lexer = lexer::Lexer::new(input.into());
            let mut parser = parser::Parser::new(lexer)?;
            let program = parser.parse_program()?;

            let evaluation = eval(ast::Node::Program(program));

            assert_eq!(evaluation, object::Object::Boolean(expected));
        }

        Ok(())
    }
}
