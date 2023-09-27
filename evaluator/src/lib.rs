use ast::{BlockStatement, IfExpression, Node, Program};
use object::Object;

pub mod object;

fn eval(node: Node) -> Object {
    match node {
        Node::Program(program) => eval_program(program),
        Node::Statement(statement) => match statement {
            ast::Statement::LetStatement(_) => todo!(),
            ast::Statement::ReturnStatement(ret) => {
                Object::Return(Box::new(eval(ast::Node::Expression(ret.expression))))
            }
            ast::Statement::ExpressionStatement(expression) => {
                eval(ast::Node::Expression(expression))
            }
            ast::Statement::BlockStatement(block) => eval_block_statement(block),
        },
        Node::Expression(expression) => match expression {
            ast::Expression::Identifier(_) => todo!(),
            ast::Expression::StringLiteral(_) => todo!(),
            ast::Expression::IntegerLiteral(i) => Object::Integer(i),
            ast::Expression::Prefix(prefix) => {
                let right = eval(ast::Node::Expression(*prefix.right));

                match prefix.operator.as_str() {
                    "!" => eval_bang_operator_expression(right),
                    "-" => eval_minus_prefix_operator_expression(right),
                    _ => Object::Null,
                }
            }
            ast::Expression::Infix(infix) => {
                let left = eval(ast::Node::Expression(*infix.left));
                let right = eval(ast::Node::Expression(*infix.right));

                eval_infix_expression(infix.operator, left, right)
            }
            ast::Expression::Empty => todo!(),
            ast::Expression::Boolean(boolean) => Object::Boolean(boolean),
            ast::Expression::IfExpression(ifexp) => eval_if_expression(ifexp),
            ast::Expression::FunctionLiteral(_) => todo!(),
            ast::Expression::CallExpression(_) => todo!(),
        },
    }
}

fn eval_statements(statements: Vec<ast::Statement>) -> Object {
    let mut result = Object::Null;

    for statement in statements {
        result = eval(ast::Node::Statement(statement));

        match result {
            Object::Return(value) => return *value,
            _ => continue,
        }
    }

    result
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(true) => Object::Boolean(false),
        Object::Boolean(false) => Object::Boolean(true),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(i) => Object::Integer(-i),
        _ => Object::Null,
    }
}

fn eval_infix_expression(operator: String, left: Object, right: Object) -> Object {
    match left.type_name() == "INTEGER" && right.type_name() == "INTEGER" {
        true => eval_integer_infix_expression(operator, left, right),
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

fn eval_integer_infix_expression(operator: String, left: Object, right: Object) -> Object {
    let left_val = match left {
        Object::Integer(i) => i,
        _ => 0,
    };

    let right_val = match right {
        Object::Integer(i) => i,
        _ => 0,
    };

    match operator.as_str() {
        "+" => Object::Integer(left_val + right_val),
        "-" => Object::Integer(left_val - right_val),
        "*" => Object::Integer(left_val * right_val),
        "/" => Object::Integer(left_val / right_val),
        "<" => Object::Boolean(left_val < right_val),
        ">" => Object::Boolean(left_val > right_val),
        "==" => Object::Boolean(left_val == right_val),
        "!=" => Object::Boolean(left_val != right_val),
        _ => Object::Null,
    }
}

fn eval_if_expression(exp: IfExpression) -> Object {
    let condition = eval(ast::Node::Expression(*exp.condition));

    if is_truthy(condition) {
        eval(ast::Node::Statement(ast::Statement::BlockStatement(
            exp.consequence,
        )))
    } else if exp.alternative.is_some() {
        return eval(ast::Node::Statement(ast::Statement::BlockStatement(
            exp.alternative.unwrap(),
        )));
    } else {
        return Object::Null;
    }
}

fn eval_program(program: Program) -> Object {
    let mut result = Object::Null;

    for statement in program.statements {
        result = eval(ast::Node::Statement(statement));

        match result {
            Object::Return(value) => return *value,
            _ => continue,
        }
    }

    return result;
}

fn eval_block_statement(block: BlockStatement) -> Object {
    let mut result = Object::Null;

    for statement in block.statements {
        result = eval(ast::Node::Statement(statement));

        if let Object::Return(_) = result {
            return result;
        }
    }

    result
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(b) => b,
        _ => true,
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

    #[test]
    fn test_conditions() -> Result<()> {
        let input = "if (true) { 
            if (true) { 
                return 40; 
            } else { 
                return 30; 
            }
         } else { return 20; }";

        let lexer = lexer::Lexer::new(input.into());
        let mut parser = parser::Parser::new(lexer)?;
        let program = parser.parse_program()?;

        let evaluation = eval(ast::Node::Program(program));

        assert_eq!(evaluation, Object::Integer(40));

        Ok(())
    }
}
