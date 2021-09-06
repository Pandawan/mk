use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{BlockExpression, Expression, IdentifierLiteral, Program, Statement},
    environment::Environment,
    object::{Function, Object, RuntimeError},
    token::Token,
};

pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self::new_with_env(Rc::new(RefCell::new(Environment::new())))
    }

    pub fn new_with_env(env: Rc<RefCell<Environment>>) -> Self {
        Evaluator { env }
    }

    pub fn eval(&mut self, prog: Program) -> Rc<Object> {
        let mut result = Rc::new(Object::Nil);

        for stmt in &prog.statements {
            let val = self.eval_statement(stmt);

            match val.as_ref() {
                // If a return value is found, immediately return and stop evaluating statements
                // Unwrap the return value into a final value so the program can use it
                Object::ReturnValue(inner_value) => return Rc::clone(inner_value),
                Object::Error(_) => return val,
                _ => result = val,
            }
        }

        result
    }

    // Similar to eval (for programs) but doesn't unwrap return values
    fn eval_block_expression(&mut self, block: &BlockExpression) -> Rc<Object> {
        let mut result = Rc::new(Object::Nil);

        for stmt in &block.statements {
            let val = self.eval_statement(stmt);

            match val.as_ref() {
                // If a return value is found, immediately return and stop evaluating statements
                // Don't unwrap the return value, we might be in a nested block which also needs to return
                Object::ReturnValue(_) => return val,
                Object::Error(_) => return val,
                _ => result = val,
            }
        }

        result
    }

    fn eval_statement(&mut self, stmt: &Statement) -> Rc<Object> {
        match stmt {
            Statement::Expression { expression } => self.eval_expression(expression),
            Statement::Return { value } => {
                let obj = self.eval_expression(value);

                // No need to encapsulate an Error with a ReturnValue since they both bubble up the same way
                if obj.is_error() {
                    return obj;
                }

                Rc::new(Object::ReturnValue(obj))
            }
            Statement::Let { name, value } => {
                let obj = self.eval_expression(value);
                // Early return the first error received
                if obj.is_error() {
                    return obj;
                }

                // TODO: Make sure that variables stay within their scopes
                // e.g. `{ let hello = 5; { let hello = 10; } hello }`
                // This would require changing the BlockExpression to create a new environment

                // Add the variable to the surrounding environment
                self.env.borrow_mut().set(name.to_owned(), obj);

                Rc::new(Object::Nil)
            }
        }
    }

    fn eval_expression(&mut self, expr: &Expression) -> Rc<Object> {
        match expr {
            Expression::Integer(value) => Rc::new(Object::Integer(*value)),
            Expression::Float(value) => Rc::new(Object::Float(*value)),
            Expression::Boolean(value) => Rc::new(Object::Boolean(*value)),
            // TODO: Should I really be cloning the String each time? (I also do this from lexer -> parser)
            Expression::String(value) => Rc::new(Object::String(value.clone())),
            Expression::Nil => Rc::new(Object::Nil),
            Expression::Identifier(identifier) => {
                self.eval_identifier_expression(identifier.clone())
            }

            Expression::Prefix(prefix) => {
                let right = self.eval_expression(&prefix.right);
                // Early return the first error received
                if right.is_error() {
                    return right;
                }
                self.eval_prefix_expression(&prefix.operator, right)
            }
            Expression::Infix(infix) => {
                let left = self.eval_expression(&infix.left);
                // Early return the first error received
                if left.is_error() {
                    return left;
                }
                let right = self.eval_expression(&infix.right);
                // Early return the first error received
                if right.is_error() {
                    return right;
                }
                self.eval_infix_expression(&infix.operator, left, right)
            }

            // TODO: Perhaps Block expressions should have their own scope? What about if/for/while/etc.
            Expression::Block(block) => self.eval_block_expression(block),

            Expression::If(if_expr) => self.eval_if_expression(
                &if_expr.condition,
                &if_expr.consequence,
                &if_expr.alternative,
            ),

            Expression::Function(func) => Rc::new(Object::Function(Function {
                parameters: func.parameters.clone(),
                body: Rc::clone(&func.body),
                env: Rc::clone(&self.env),
            })),
            Expression::Call(call) => {
                let func = self.eval_expression(&call.function);
                // Early return the first error received
                if func.is_error() {
                    return func;
                }
                let args = self.eval_expressions(&call.arguments);
                if args.len() == 1 && args.first().unwrap().is_error() {
                    return Rc::clone(args.first().unwrap());
                }

                self.apply_function(func, args)
            }

            _ => todo!("Implement other expressions {}", expr),
        }
    }

    fn eval_expressions(&mut self, exprs: &Vec<Expression>) -> Vec<Rc<Object>> {
        let mut result = Vec::new();
        for expr in exprs {
            let evaluated = self.eval_expression(expr);
            if evaluated.is_error() {
                return vec![evaluated];
            }
            result.push(evaluated);
        }
        result
    }

    fn eval_identifier_expression(&self, identifier: IdentifierLiteral) -> Rc<Object> {
        let result = self.env.borrow().get(&identifier.name);

        match result {
            Some(obj) => obj,
            None => Rc::new(Object::Error(RuntimeError::IdentifierNotFound(
                identifier.name,
            ))),
        }
    }

    fn eval_prefix_expression(&self, operator: &Token, right: Rc<Object>) -> Rc<Object> {
        match operator {
            Token::Bang => self.eval_bang_operator_expression(right),
            Token::Minus => self.eval_minus_prefix_operator_expression(right),
            // NOTE: Evaluator incorrectly asked to evaluate given operator as a prefix
            _ => panic!("unknown prefix operator {}{:?}", operator, right),
        }
    }

    fn eval_bang_operator_expression(&self, right: Rc<Object>) -> Rc<Object> {
        match *right {
            Object::Boolean(true) => Rc::new(Object::Boolean(false)),
            Object::Boolean(false) => Rc::new(Object::Boolean(true)),
            _ => Rc::new(Object::Error(RuntimeError::InvalidPrefixOperandType(
                Token::Bang,
                right,
            ))),
        }
    }

    fn eval_minus_prefix_operator_expression(&self, right: Rc<Object>) -> Rc<Object> {
        match *right {
            Object::Integer(value) => Rc::new(Object::Integer(-value)),
            Object::Float(value) => Rc::new(Object::Float(-value)),
            _ => Rc::new(Object::Error(RuntimeError::InvalidPrefixOperandType(
                Token::Minus,
                right,
            ))),
        }
    }

    fn eval_infix_expression(
        &self,
        operator: &Token,
        left: Rc<Object>,
        right: Rc<Object>,
    ) -> Rc<Object> {
        match (left.as_ref(), right.as_ref()) {
            (Object::Integer(left_value), Object::Integer(right_value)) => {
                self.eval_integer_infix_expression(operator, *left_value, *right_value)
            }

            (Object::Float(left_value), Object::Float(right_value)) => {
                self.eval_float_infix_expression(operator, *left_value, *right_value)
            }
            (Object::Float(left_value), Object::Integer(right_value)) => {
                self.eval_float_infix_expression(operator, *left_value, *right_value as f64)
            }
            (Object::Integer(left_value), Object::Float(right_value)) => {
                self.eval_float_infix_expression(operator, *left_value as f64, *right_value)
            }

            (Object::Boolean(left_value), Object::Boolean(right_value)) => {
                self.eval_boolean_infix_expression(operator, *left_value, *right_value)
            }

            (Object::String(left_value), Object::String(right_value)) => {
                self.eval_string_infix_expression(operator, left_value, right_value)
            }

            (_, _) => Rc::new(Object::Error(RuntimeError::InvalidInfixOperandType(
                operator.clone(),
                left,
                right,
            ))),
        }
    }

    fn eval_integer_infix_expression(
        &self,
        operator: &Token,
        left_value: i64,
        right_value: i64,
    ) -> Rc<Object> {
        match operator {
            Token::Plus => Rc::new(Object::Integer(left_value + right_value)),
            Token::Minus => Rc::new(Object::Integer(left_value - right_value)),
            Token::Star => Rc::new(Object::Integer(left_value * right_value)),
            Token::Slash => Rc::new(Object::Integer(left_value / right_value)),

            Token::LessThan => Rc::new(Object::Boolean(left_value < right_value)),
            Token::LessEqual => Rc::new(Object::Boolean(left_value <= right_value)),
            Token::GreaterThan => Rc::new(Object::Boolean(left_value > right_value)),
            Token::GreaterEqual => Rc::new(Object::Boolean(left_value >= right_value)),
            Token::EqualEqual => Rc::new(Object::Boolean(left_value == right_value)),
            Token::BangEqual => Rc::new(Object::Boolean(left_value != right_value)),

            operator => Rc::new(Object::Error(RuntimeError::InvalidInfixOperandType(
                operator.clone(),
                // TODO: Find a way to keep using the previous Object::Integer rather than creating a new one
                Rc::new(Object::Integer(left_value)),
                Rc::new(Object::Integer(right_value)),
            ))),
        }
    }

    fn eval_float_infix_expression(
        &self,
        operator: &Token,
        left_value: f64,
        right_value: f64,
    ) -> Rc<Object> {
        match operator {
            Token::Plus => Rc::new(Object::Float(left_value + right_value)),
            Token::Minus => Rc::new(Object::Float(left_value - right_value)),
            Token::Star => Rc::new(Object::Float(left_value * right_value)),
            Token::Slash => Rc::new(Object::Float(left_value / right_value)),

            Token::LessThan => Rc::new(Object::Boolean(left_value < right_value)),
            Token::LessEqual => Rc::new(Object::Boolean(left_value <= right_value)),
            Token::GreaterThan => Rc::new(Object::Boolean(left_value > right_value)),
            Token::GreaterEqual => Rc::new(Object::Boolean(left_value >= right_value)),
            Token::EqualEqual => Rc::new(Object::Boolean(left_value == right_value)),
            Token::BangEqual => Rc::new(Object::Boolean(left_value != right_value)),

            operator => Rc::new(Object::Error(RuntimeError::InvalidInfixOperandType(
                operator.clone(),
                // TODO: Find a way to keep using the previous Object::Integer rather than creating a new one
                Rc::new(Object::Float(left_value)),
                Rc::new(Object::Float(right_value)),
            ))),
        }
    }

    fn eval_boolean_infix_expression(
        &self,
        operator: &Token,
        left_value: bool,
        right_value: bool,
    ) -> Rc<Object> {
        match operator {
            // NOTE: No truthy/implicit conversion
            Token::EqualEqual => Rc::new(Object::Boolean(left_value == right_value)),
            Token::BangEqual => Rc::new(Object::Boolean(left_value != right_value)),

            operator => Rc::new(Object::Error(RuntimeError::InvalidInfixOperandType(
                operator.clone(),
                // TODO: Find a way to keep using the previous Object::Boolean rather than creating a new one
                Rc::new(Object::Boolean(left_value)),
                Rc::new(Object::Boolean(right_value)),
            ))),
        }
    }

    fn eval_string_infix_expression(
        &self,
        operator: &Token,
        left_value: &String,
        right_value: &String,
    ) -> Rc<Object> {
        match operator {
            Token::Plus => Rc::new(Object::String(left_value.to_owned() + right_value)),
            Token::EqualEqual => Rc::new(Object::Boolean(left_value == right_value)),
            Token::BangEqual => Rc::new(Object::Boolean(left_value != right_value)),

            operator => Rc::new(Object::Error(RuntimeError::InvalidInfixOperandType(
                operator.clone(),
                // TODO: Find a way to keep using the previous Object::String rather than creating a new one
                Rc::new(Object::String(left_value.clone())),
                Rc::new(Object::String(right_value.clone())),
            ))),
        }
    }

    fn eval_if_expression(
        &mut self,
        condition: &Expression,
        consequence: &BlockExpression,
        alternative: &Option<Expression>,
    ) -> Rc<Object> {
        let evaluated_condition = self.eval_expression(condition);
        // Early return the first error received
        if evaluated_condition.is_error() {
            return evaluated_condition;
        }

        match *evaluated_condition {
            Object::Boolean(value) => {
                if value {
                    self.eval_block_expression(consequence)
                } else if let Some(alternative) = alternative {
                    self.eval_expression(alternative)
                } else {
                    Rc::new(Object::Nil)
                }
            }
            _ => Rc::new(Object::Error(RuntimeError::ExpectedBooleanCondition(
                evaluated_condition,
            ))),
        }
    }

    fn apply_function(&mut self, func: Rc<Object>, args: Vec<Rc<Object>>) -> Rc<Object> {
        match func.as_ref() {
            Object::Function(func) => {
                // TODO: Check that number of args & params matches

                if args.len() != func.parameters.len() {
                    return Rc::new(Object::Error(RuntimeError::BadArity {
                        expected: func.parameters.len(),
                        got: args.len(),
                    }));
                }

                let current_env = Rc::clone(&self.env);
                let mut scoped_env = Environment::new_enclosed(Rc::clone(&func.env));

                for (ident, obj) in func.parameters.iter().zip(args.iter()) {
                    scoped_env.set(ident.name.clone(), Rc::clone(obj));
                }

                self.env = Rc::new(RefCell::new(scoped_env));

                let result = self.eval_block_expression(&func.body);

                self.env = current_env;

                result
            }
            _ => Rc::new(Object::Error(RuntimeError::NotAFunction(func))),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{
        evaluator::Evaluator,
        lexer::Lexer,
        object::{Object, RuntimeError},
        parser::Parser,
        token::Token,
    };

    #[test]
    fn eval_integer_expression() {
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

        for (input, expected_value) in tests {
            let evaluated = evaluate(input);
            test_integer_object(evaluated, expected_value);
        }
    }

    #[test]
    fn eval_float_expression() {
        let tests = vec![
            ("5.5", 5.5),
            ("10.5", 10.5),
            ("-5.5", -5.5),
            ("-10.5", -10.5),
            ("5.5 + 5.5 + 5.5 + 5.5 - 10.5", 11.5),
            ("2.5 * 2.5 * 2.5 * 2.5 * 2.5", 97.65625),
            ("-50.25 + 100.5 + -50.25", 0.0),
            ("5.5 * 2.5 + 10.5", 24.25),
            ("5.5 + 2.5 * 10.5", 31.75),
            ("20.5 + 2.5 * -10.5", -5.75),
            ("50.5 / 2.5 * 2.5 + 10.5", 61.0),
            ("2.5 * (5.5 + 10.5)", 40.0),
            ("3.5 * 3.5 * 3.5 + 10.5", 53.375),
            ("3.5 * (3.5 * 3.5) + 10.5", 53.375),
            (
                "(5.5 + 10.5 * 2.5 + 15.5 / 3.5) * 2.5 + -10.5",
                (5.5 + 10.5 * 2.5 + 15.5 / 3.5) * 2.5 + -10.5,
            ),
            ("0.1 + 0.2", 0.1 + 0.2),
        ];

        for (input, expected_value) in tests {
            let evaluated = evaluate(input);
            test_float_object(evaluated, expected_value);
        }
    }

    #[test]
    fn eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("1.5 < 2.5", true),
            ("1.5 > 2.5", false),
            ("1.5 < 1.5", false),
            ("1.5 > 1.5", false),
            ("1.5 == 2.5", false),
            ("1.5 != 2.5", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("\"hello\" == \'hello\'", true),
            ("\"hello\" == \'world\'", false),
            ("\"hello\" != \"world\"", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
            ("(1.5 < 2.5) == true", true),
            ("(1.5 < 2.5) == false", false),
            ("(1.5 > 2.5) == true", false),
            ("(1.5 > 2.5) == false", true),
            ("0.1 + 0.2 == 0.3", false),
        ];

        for (input, expected_value) in tests {
            let evaluated = evaluate(input);
            test_boolean_object(evaluated, expected_value);
        }
    }

    #[test]
    fn eval_string_expression() {
        let tests = vec![
            ("\"hello world\"", "hello world"),
            ("\"hello\" + \" \" + \"world\"", "hello world"),
        ];

        for (input, expected_value) in tests {
            let evaluated = evaluate(input);
            test_string_object(evaluated, expected_value);
        }
    }

    #[test]
    fn eval_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!!true", true),
            ("!!false", false),
        ];

        for (input, expected_value) in tests {
            let evaluated = evaluate(input);
            test_boolean_object(evaluated, expected_value);
        }
    }

    #[test]
    fn eval_if_else_expression() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Nil),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Nil),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        for (input, expected_value) in tests {
            let evaluated = evaluate(input);

            match expected_value {
                Object::Integer(i) => test_integer_object(evaluated, i),
                _ => test_null_object(evaluated),
            }
        }
    }

    #[test]
    fn eval_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "
                if (10 > 1) {
                  if (10 > 1) {
                    return 10;
                  }
                
                  return 1;
                }
                ",
                10,
            ),
        ];

        for (input, expected_value) in tests {
            let evaluated = evaluate(input);
            test_integer_object(evaluated, expected_value)
        }
    }

    #[test]
    fn eval_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected_value) in tests {
            let evaluated = evaluate(input);
            test_integer_object(evaluated, expected_value)
        }
    }

    #[test]
    fn eval_function_expression() {
        let input = "fn (x) { x + 2; }";
        let evaluated = evaluate(input);

        match evaluated.as_ref() {
            Object::Function(func) => {
                if func.parameters.len() != 1 {
                    panic!(
                        "expected function object with 1 parameter but got {} ({})",
                        func.parameters.len(),
                        (&func.parameters)
                            .into_iter()
                            .map(|p| p.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                }

                let param = func.parameters.first().unwrap();
                if param.name != "x" {
                    panic!("expected function parameter to be x but got {}", param.name)
                }

                if func.body.to_string() != "(x + 2)" {
                    panic!(
                        "expected function body to be (x + 2) but got {}",
                        func.body.to_string()
                    )
                }
            }
            obj => panic!("expected function object but got {}", obj),
        }
    }

    #[test]
    fn eval_call_expression() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
            (
                "
                let adder = fn(x) { fn(y) { x + y } }; 
                let fiveAdder = adder(5);
                fiveAdder(3);
                ",
                8,
            ),
        ];

        for (input, expected_value) in tests {
            let evaluated = evaluate(input);
            test_integer_object(evaluated, expected_value);
        }
    }

    #[test]
    fn error_handling() {
        let tests = vec![
            (
                "5 + true;",
                RuntimeError::InvalidInfixOperandType(
                    Token::Plus,
                    Rc::new(Object::Integer(5)),
                    Rc::new(Object::Boolean(true)),
                ),
            ),
            (
                "5 + true; 5;",
                RuntimeError::InvalidInfixOperandType(
                    Token::Plus,
                    Rc::new(Object::Integer(5)),
                    Rc::new(Object::Boolean(true)),
                ),
            ),
            (
                "-true",
                RuntimeError::InvalidPrefixOperandType(
                    Token::Minus,
                    Rc::new(Object::Boolean(true)),
                ),
            ),
            (
                "true + false;",
                RuntimeError::InvalidInfixOperandType(
                    Token::Plus,
                    Rc::new(Object::Boolean(true)),
                    Rc::new(Object::Boolean(false)),
                ),
            ),
            (
                "\"hello\" - \"world\";",
                RuntimeError::InvalidInfixOperandType(
                    Token::Minus,
                    Rc::new(Object::String("hello".to_string())),
                    Rc::new(Object::String("world".to_string())),
                ),
            ),
            (
                "5; true + false; 5",
                RuntimeError::InvalidInfixOperandType(
                    Token::Plus,
                    Rc::new(Object::Boolean(true)),
                    Rc::new(Object::Boolean(false)),
                ),
            ),
            (
                "if 10 > 1 { true + false; }",
                RuntimeError::InvalidInfixOperandType(
                    Token::Plus,
                    Rc::new(Object::Boolean(true)),
                    Rc::new(Object::Boolean(false)),
                ),
            ),
            (
                "if 1 { true + false; }",
                RuntimeError::ExpectedBooleanCondition(Rc::new(Object::Integer(1))),
            ),
            (
                "
                if (10 > 1) {
                  if (10 > 1) {
                    return true + false;
                  }
                  return 1;
                }
                ",
                RuntimeError::InvalidInfixOperandType(
                    Token::Plus,
                    Rc::new(Object::Boolean(true)),
                    Rc::new(Object::Boolean(false)),
                ),
            ),
            ("foobar", RuntimeError::IdentifierNotFound("foobar".into())),
        ];

        for (input, expected_error) in tests {
            let evaluated = evaluate(input);
            test_error_object(evaluated, expected_error)
        }
    }

    fn evaluate(input: &str) -> Rc<Object> {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let prog = p.parse_program();

        match prog {
            Ok(prog) => Evaluator::new().eval(prog),
            Err(errors) => {
                println!("parser had {} errors", errors.len());
                for error in errors {
                    println!("parser error: {}", error);
                }
                panic!("parser errors")
            }
        }
    }

    fn test_integer_object(obj: Rc<Object>, expected_value: i64) {
        match *obj {
            Object::Integer(value) => {
                if value != expected_value {
                    panic!(
                        "expected integer object with value {} but got {:?}",
                        expected_value, obj
                    )
                }
            }
            _ => panic!("expected integer object but got {:?}", obj),
        }
    }

    fn test_float_object(obj: Rc<Object>, expected_value: f64) {
        match *obj {
            Object::Float(value) => {
                if value != expected_value {
                    panic!(
                        "expected float object with value {} but got {:?}",
                        expected_value, obj
                    )
                }
            }
            _ => panic!("expected float object but got {:?}", obj),
        }
    }

    fn test_boolean_object(obj: Rc<Object>, expected_value: bool) {
        match *obj {
            Object::Boolean(value) => {
                if value != expected_value {
                    panic!(
                        "expected boolean object with value {} but got {:?}",
                        expected_value, obj
                    )
                }
            }
            _ => panic!("expected boolean object but got {:?}", obj),
        }
    }

    fn test_string_object(obj: Rc<Object>, expected_value: &str) {
        match obj.as_ref() {
            Object::String(value) => {
                if value != expected_value {
                    panic!(
                        "expected string object with value {} but got {:?}",
                        expected_value, obj
                    )
                }
            }
            _ => panic!("expected string object but got {:?}", obj),
        }
    }

    fn test_null_object(obj: Rc<Object>) {
        match *obj {
            Object::Nil => {}
            _ => panic!("expected null object but got {:?}", obj),
        }
    }

    fn test_error_object(obj: Rc<Object>, expected_error: RuntimeError) {
        match obj.as_ref() {
            Object::Error(err) => {
                if *err != expected_error {
                    panic!(
                        "expected error to be \"{:?}\" but got \"{:?}\"",
                        expected_error, err
                    )
                }
            }
            _ => panic!("expected error object but got {:?}", obj),
        }
    }
}
