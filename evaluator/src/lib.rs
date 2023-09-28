use ast::{BlockStatement, IfExpression, Node, Program};
use environment::Environment;
use object::Object;

pub mod environment;
pub mod object;

fn eval(node: Node, env: &mut Environment) -> Object {
    match node {
        Node::Program(program) => eval_program(program, env),
        Node::Statement(statement) => match statement {
            ast::Statement::LetStatement(statement) => {
                if statement.expression.is_none() {
                    return Object::Error("expected expression".into());
                }

                let value = eval(ast::Node::Expression(statement.expression.unwrap()), env);

                if value.type_name() == "ERROR" {
                    return value;
                }

                env.set(&statement.identifier, value)
            }
            ast::Statement::ReturnStatement(ret) => {
                Object::Return(Box::new(eval(ast::Node::Expression(ret.expression), env)))
            }
            ast::Statement::ExpressionStatement(expression) => {
                eval(ast::Node::Expression(expression), env)
            }
            ast::Statement::BlockStatement(block) => eval_block_statement(block, env),
        },
        Node::Expression(expression) => match expression {
            ast::Expression::Identifier(identifier) => eval_identifier(identifier, env),
            ast::Expression::StringLiteral(_) => todo!(),
            ast::Expression::IntegerLiteral(i) => Object::Integer(i),
            ast::Expression::Prefix(prefix) => {
                let right = eval(ast::Node::Expression(*prefix.right), env);

                match prefix.operator.as_str() {
                    "!" => eval_bang_operator_expression(right),
                    "-" => eval_minus_prefix_operator_expression(right),
                    _ => Object::Error(format!(
                        "unknown operator: {}{}",
                        prefix.operator,
                        right.type_name()
                    )),
                }
            }
            ast::Expression::Infix(infix) => {
                let left = eval(ast::Node::Expression(*infix.left), env);
                let right = eval(ast::Node::Expression(*infix.right), env);

                if left.type_name() != right.type_name() {
                    return Object::Error(format!(
                        "type mismatch: {} {} {}",
                        left.type_name(),
                        infix.operator,
                        right.type_name()
                    ));
                }

                eval_infix_expression(infix.operator, left, right)
            }
            ast::Expression::Empty => todo!(),
            ast::Expression::Boolean(boolean) => Object::Boolean(boolean),
            ast::Expression::IfExpression(ifexp) => eval_if_expression(ifexp, env),
            ast::Expression::FunctionLiteral(function) => Object::Function(object::Function {
                parameters: function.parameters,
                body: function.body,
                env: env.clone(),
            }),
            ast::Expression::CallExpression(function) => {
                let fnn = eval(ast::Node::Expression(*function.function), env);

                if fnn.type_name() == "ERROR" {
                    return fnn;
                }

                let arguments = eval_expressions(function.arguments, env);

                if arguments.len() == 1 && arguments[0].type_name() == "ERROR" {
                    return arguments[0].clone();
                }

                apply_function(fnn, arguments)
            }
        },
    }
}

fn eval_statements(statements: Vec<ast::Statement>, env: &mut Environment) -> Object {
    let mut result = Object::Null;

    for statement in statements {
        result = eval(ast::Node::Statement(statement), env);

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
        _ => Object::Error(format!("unknown operator: -{}", right.type_name())),
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
                Object::Error(format!(
                    "unknown operator: {} {} {}",
                    left.type_name(),
                    operator,
                    right.type_name()
                ))
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
        _ => Object::Error(format!(
            "unknown operator: {} {} {}",
            left.type_name(),
            operator,
            right.type_name()
        )),
    }
}

fn eval_if_expression(exp: IfExpression, env: &mut Environment) -> Object {
    let condition = eval(ast::Node::Expression(*exp.condition), env);

    if is_truthy(condition) {
        eval(
            ast::Node::Statement(ast::Statement::BlockStatement(exp.consequence)),
            env,
        )
    } else if exp.alternative.is_some() {
        return eval(
            ast::Node::Statement(ast::Statement::BlockStatement(exp.alternative.unwrap())),
            env,
        );
    } else {
        return Object::Null;
    }
}

fn eval_program(program: Program, env: &mut Environment) -> Object {
    let mut result = Object::Null;

    for statement in program.statements {
        result = eval(ast::Node::Statement(statement), env);

        match result {
            Object::Return(value) => return *value,
            Object::Error(_) => return result,
            _ => continue,
        }
    }

    return result;
}

fn eval_block_statement(block: BlockStatement, env: &mut Environment) -> Object {
    let mut result = Object::Null;

    for statement in block.statements {
        result = eval(ast::Node::Statement(statement), env);

        match result {
            Object::Return(_) | Object::Error(_) => return result,
            _ => continue,
        }
    }

    result
}

fn eval_identifier(identifier: ast::Identifier, env: &mut Environment) -> Object {
    let (value, ok) = env.get(&identifier.value);

    if ok {
        return value.unwrap().clone();
    }

    Object::Error(format!("identifier not found: {}", identifier.value))
}

fn eval_expressions(expressions: Vec<ast::Expression>, env: &mut Environment) -> Vec<Object> {
    let mut result = Vec::new();

    for expression in expressions {
        let evaluated = eval(ast::Node::Expression(expression), env);

        if evaluated.type_name() == "ERROR" {
            return vec![evaluated];
        }

        result.push(evaluated);
    }

    result
}

fn apply_function(function: Object, args: Vec<Object>) -> Object {
    match function {
        Object::Function(f) => {
            let mut extended_env = extended_function_env(f.clone(), args.clone()).unwrap();

            for (param, arg) in f.parameters.iter().zip(args) {
                extended_env.set(&param.value, arg);
            }

            let evaluated = eval(
                ast::Node::Statement(ast::Statement::BlockStatement(f.body)),
                &mut extended_env,
            );

            unwrap_return_value(evaluated)
        }
        _ => Object::Error(format!("not a function: {}", function.type_name())),
    }
}

fn extended_function_env(
    function: object::Function,
    args: Vec<Object>,
) -> Result<Environment, String> {
    let mut env = function.env.new_enclosed_environment(function.env.clone());

    for (param, arg) in function.parameters.iter().zip(args) {
        env.set(&param.value, arg);
    }

    Ok(env)
}

fn unwrap_return_value(obj: Object) -> Object {
    match obj {
        Object::Return(value) => *value,
        _ => obj,
    }
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
    use anyhow::{Ok, Result};

    use super::*;

    #[test]
    fn test_integer_expression() -> Result<()> {
        let mut environment = Environment::new();

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

            let evaluation = eval(ast::Node::Program(program), &mut environment);

            assert_eq!(evaluation, Object::Integer(expected));
        }

        Ok(())
    }

    #[test]
    fn test_boolean_literals() -> Result<()> {
        let mut environment = Environment::new();

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

            let evaluation = eval(ast::Node::Program(program), &mut environment);

            assert_eq!(evaluation, Object::Boolean(expected));
        }

        Ok(())
    }

    #[test]
    fn test_bang_prefix() -> Result<()> {
        let mut environment = Environment::new();

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

            let evaluation = eval(ast::Node::Program(program), &mut environment);

            assert_eq!(evaluation, Object::Boolean(expected));
        }

        Ok(())
    }

    #[test]
    fn test_conditions() -> Result<()> {
        let mut environment = Environment::new();

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

        let evaluation = eval(ast::Node::Program(program), &mut environment);

        assert_eq!(evaluation, Object::Integer(40));

        Ok(())
    }

    #[test]
    fn test_errors() -> Result<()> {
        let mut environment = Environment::new();

        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) { 
                    if (10 > 1) { 
                        return true + false; 
                    } 
                    return 1; 
                }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
        ];

        for (input, expected) in tests {
            let lexer = lexer::Lexer::new(input.into());
            let mut parser = parser::Parser::new(lexer)?;
            let program = parser.parse_program()?;

            let evaluation = eval(ast::Node::Program(program), &mut environment);

            assert_eq!(evaluation, Object::Error(expected.into()));
        }

        Ok(())
    }

    #[test]
    fn test_environment() -> Result<()> {
        let mut environment = Environment::new();
        let tests = [
            ("let a =5; return a;", 5),
            ("let a = 5 * 5; return a;", 25),
            ("let a = 5; let b = a; return b;", 5),
            ("let a = 5; let b = a; return a + b;", 10),
            ("let a = 5; let b = a; let c = a + b + 5; return c;", 15),
        ];

        for (input, expected) in tests {
            let lexer = lexer::Lexer::new(input.into());
            let mut parser = parser::Parser::new(lexer)?;
            let program = parser.parse_program()?;

            let evaluation = eval(ast::Node::Program(program), &mut environment);

            assert_eq!(evaluation, Object::Integer(expected));
        }

        Ok(())
    }

    #[test]
    fn test_function_object() -> Result<()> {
        let mut environment = Environment::new();

        let input = "function(x) { x + 2; };";

        let lexer = lexer::Lexer::new(input.into());
        let mut parser = parser::Parser::new(lexer)?;
        let program = parser.parse_program()?;

        let evaluation = eval(ast::Node::Program(program), &mut environment);

        match evaluation {
            Object::Function(f) => {
                assert_eq!(f.parameters.len(), 1);
                assert_eq!(f.parameters[0].value, "x");
                assert_eq!(f.body.statements.len(), 1);
            }
            _ => panic!("object is not a function"),
        }

        Ok(())
    }

    #[test]
    fn test_function_apply() -> Result<()> {
        let mut environment = Environment::new();

        let tests = [
            ("let identity = function(x) { return x; }; identity(5);", 5),
            ("let identity = function(x) { return x; }; identity(5);", 5),
            ("let double = function(x) { return x * 2; }; double(5);", 10),
            ("let add = function(x, y) { return x + y; }; add(5, 5);", 10),
            (
                "let add = function(x, y) { return x + y; }; add(5 + 5, add(5, 5));",
                20,
            ),
            ("function(x) { return x; }(5)", 5),
        ];

        for (input, expected) in tests {
            let lexer = lexer::Lexer::new(input.into());
            let mut parser = parser::Parser::new(lexer)?;
            let program = parser.parse_program()?;

            let evaluation = eval(ast::Node::Program(program), &mut environment);

            assert_eq!(evaluation, Object::Integer(expected));
        }

        Ok(())
    }
}
