use ast::Node;
use object::Object;

pub mod object;

fn eval(node: Node) -> Object {
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
            ast::Expression::Prefix(prefix) => {
                let right = eval(ast::Node::Expression(*prefix.right));

                match prefix.operator.as_str() {
                    "!" => evalBangOperatorExpression(right),
                    "-" => evalMinusPrefixOperatorExpression(right),
                    _ => Object::Null,
                }
            }
            ast::Expression::Infix(infix) => {
                let left = eval(ast::Node::Expression(*infix.left));
                let right = eval(ast::Node::Expression(*infix.right));

                evalInfixExpression(infix.operator, left, right)
            }
            ast::Expression::Empty => todo!(),
            ast::Expression::Boolean(boolean) => Object::Boolean(boolean),
            ast::Expression::IfExpression(_) => todo!(),
            ast::Expression::FunctionLiteral(_) => todo!(),
            ast::Expression::CallExpression(_) => todo!(),
        },
    }
}

fn evalStatements(statements: Vec<ast::Statement>) -> Object {
    let mut result = Object::Null;

    for statement in statements {
        result = eval(ast::Node::Statement(statement));
    }

    result
}

fn evalBangOperatorExpression(right: Object) -> Object {
    match right {
        Object::Boolean(true) => Object::Boolean(false),
        Object::Boolean(false) => Object::Boolean(true),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn evalMinusPrefixOperatorExpression(right: Object) -> Object {
    match right {
        Object::Integer(i) => Object::Integer(-i),
        _ => Object::Null,
    }
}

fn evalInfixExpression(operator: String, left: Object, right: Object) -> Object {
    match left.type_name() == "INTEGER" && right.type_name() == "INTEGER" {
        true => evalIntegerInfixExpression(operator, left, right),
        _ => {
            if operator == "==" {
                Object::Boolean(left == right)
            } else if operator == "!=" {
                Object::Boolean(left != right)
            } else {
                Object::Null
            }
        }
    }
}

fn evalIntegerInfixExpression(operator: String, left: Object, right: Object) -> Object {
    let leftVal = match left {
        Object::Integer(i) => i,
        _ => 0,
    };

    let rightVal = match right {
        Object::Integer(i) => i,
        _ => 0,
    };

    match operator.as_str() {
        "+" => Object::Integer(leftVal + rightVal),
        "-" => Object::Integer(leftVal - rightVal),
        "*" => Object::Integer(leftVal * rightVal),
        "/" => Object::Integer(leftVal / rightVal),
        "<" => Object::Boolean(leftVal < rightVal),
        ">" => Object::Boolean(leftVal > rightVal),
        "==" => Object::Boolean(leftVal == rightVal),
        "!=" => Object::Boolean(leftVal != rightVal),
        _ => Object::Null,
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Result;

    use super::*;

    #[test]
    fn test_integer_expression() -> Result<()> {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            let lexer = lexer::Lexer::new(input.into());
            let mut parser = parser::Parser::new(lexer)?;
            let program = parser.parse_program()?;

            let evaluation = eval(ast::Node::Program(program));

            assert_eq!(evaluation, Object::Integer(expected));
        }

        Ok(())
    }

    #[test]
    fn test_boolean_literals() -> Result<()> {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests {
            let lexer = lexer::Lexer::new(input.into());
            let mut parser = parser::Parser::new(lexer)?;
            let program = parser.parse_program()?;

            let evaluation = eval(ast::Node::Program(program));

            assert_eq!(evaluation, Object::Boolean(expected));
        }

        Ok(())
    }

    #[test]
    fn test_bang_prefix() -> Result<()> {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let lexer = lexer::Lexer::new(input.into());
            let mut parser = parser::Parser::new(lexer)?;
            let program = parser.parse_program()?;

            let evaluation = eval(ast::Node::Program(program));

            assert_eq!(evaluation, Object::Boolean(expected));
        }

        Ok(())
    }
}
