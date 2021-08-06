use crate::{
    ast::{BlockExpression, Expression, Program, Statement},
    object::Object,
    token::Token,
};

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {}
    }

    pub fn eval(&self, prog: Program) -> Option<Object> {
        let mut result = None;

        for stmt in prog.statements {
            match self.eval_statement(stmt) {
                // If a return value is found, immediately return and stop evaluating statements
                // Unwrap the return value into a final value so the program can use it
                Object::ReturnValue(obj) => return Some(*obj),
                Object::Error(message) => return Some(Object::Error(message)),
                obj => result = Some(obj),
            }
        }

        return result;
    }

    // Similar to eval (for programs) but doesn't unwrap return values
    fn eval_block_expression(&self, block: BlockExpression) -> Option<Object> {
        let mut result = None;

        for stmt in block.statements {
            match self.eval_statement(stmt) {
                // If a return value is found, immediately return and stop evaluating statements
                // Don't unwrap the return value, we might be in a nested block which also needs to return
                Object::ReturnValue(obj) => return Some(Object::ReturnValue(obj)),
                Object::Error(message) => return Some(Object::Error(message)),
                obj => result = Some(obj),
            }
        }

        return result;
    }

    fn eval_statement(&self, stmt: Statement) -> Object {
        match stmt {
            Statement::Expression { expression } => self.eval_expression(expression),
            Statement::Return { value } => {
                let obj = self.eval_expression(value);

                // No need to encapsulate an Error with a ReturnValue since they both bubble up the same way
                if obj.is_error() {
                    return obj;
                }

                return Object::ReturnValue(Box::new(obj));
            }
            _ => todo!("Implement other statements"),
        }
    }

    fn eval_expression(&self, expr: Expression) -> Object {
        match expr {
            Expression::Integer(value) => Object::Integer(value),
            Expression::Float(value) => Object::Float(value),
            Expression::Boolean(value) => Object::Boolean(value),
            Expression::Nil => Object::Nil,
            Expression::Prefix(prefix) => {
                let right = self.eval_expression(prefix.right);
                // Early return the first error received
                if right.is_error() {
                    return right;
                }
                self.eval_prefix_expression(prefix.operator, right)
            }
            Expression::Infix(infix) => {
                let left = self.eval_expression(infix.left);
                // Early return the first error received
                if left.is_error() {
                    return left;
                }
                let right = self.eval_expression(infix.right);
                // Early return the first error received
                if right.is_error() {
                    return right;
                }
                self.eval_infix_expression(infix.operator, left, right)
            }

            Expression::Block(block) => self
                .eval_block_expression(*block)
                // Blocks return nil unless a value is given
                .unwrap_or(Object::Nil),

            Expression::If(if_expr) => {
                self.eval_if_expression(if_expr.condition, if_expr.consequence, if_expr.alternative)
            }

            _ => todo!("Implement other expressions {}", expr),
        }
    }

    fn eval_prefix_expression(&self, operator: Token, right: Object) -> Object {
        match operator {
            Token::Bang => self.eval_bang_operator_expression(right),
            Token::Minus => self.eval_minus_prefix_operator_expression(right),
            _ => Object::Error(format!("unknown prefix operator {}{:?}", operator, right)),
        }
    }

    fn eval_bang_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Boolean(true) => Object::Boolean(false),
            Object::Boolean(false) => Object::Boolean(true),
            _ => Object::Error(format!(
                "invalid operand type `{}` for ! operator",
                right.typename()
            )),
        }
    }

    fn eval_minus_prefix_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Integer(value) => Object::Integer(-value),
            Object::Float(value) => Object::Float(-value),
            _ => Object::Error(format!(
                "invalid operand type `{}` for - operator",
                right.typename()
            )),
        }
    }

    fn eval_infix_expression(&self, operator: Token, left: Object, right: Object) -> Object {
        match (left, right) {
            (Object::Integer(left_value), Object::Integer(right_value)) => {
                self.eval_integer_infix_expression(operator, left_value, right_value)
            }

            (Object::Float(left_value), Object::Float(right_value)) => {
                self.eval_float_infix_expression(operator, left_value, right_value)
            }
            (Object::Float(left_value), Object::Integer(right_value)) => {
                self.eval_float_infix_expression(operator, left_value, right_value as f64)
            }
            (Object::Integer(left_value), Object::Float(right_value)) => {
                self.eval_float_infix_expression(operator, left_value as f64, right_value)
            }

            (Object::Boolean(left_value), Object::Boolean(right_value)) => {
                self.eval_boolean_infix_expression(operator, left_value, right_value)
            }

            (left, right) => Object::Error(format!(
                "type mismatch for {}: {:?} and {:?}",
                operator, left, right
            )),
        }
    }

    fn eval_integer_infix_expression(
        &self,
        operator: Token,
        left_value: i64,
        right_value: i64,
    ) -> Object {
        match operator {
            Token::Plus => Object::Integer(left_value + right_value),
            Token::Minus => Object::Integer(left_value - right_value),
            Token::Star => Object::Integer(left_value * right_value),
            Token::Slash => Object::Integer(left_value / right_value),

            Token::LessThan => Object::Boolean(left_value < right_value),
            Token::LessEqual => Object::Boolean(left_value <= right_value),
            Token::GreaterThan => Object::Boolean(left_value > right_value),
            Token::GreaterEqual => Object::Boolean(left_value >= right_value),
            Token::EqualEqual => Object::Boolean(left_value == right_value),
            Token::BangEqual => Object::Boolean(left_value != right_value),

            operator => Object::Error(format!(
                "unknown infix operator: {:?} {} {:?}",
                Object::Integer(left_value),
                operator,
                Object::Integer(right_value)
            )),
        }
    }

    fn eval_float_infix_expression(
        &self,
        operator: Token,
        left_value: f64,
        right_value: f64,
    ) -> Object {
        match operator {
            Token::Plus => Object::Float(left_value + right_value),
            Token::Minus => Object::Float(left_value - right_value),
            Token::Star => Object::Float(left_value * right_value),
            Token::Slash => Object::Float(left_value / right_value),

            Token::LessThan => Object::Boolean(left_value < right_value),
            Token::LessEqual => Object::Boolean(left_value <= right_value),
            Token::GreaterThan => Object::Boolean(left_value > right_value),
            Token::GreaterEqual => Object::Boolean(left_value >= right_value),
            Token::EqualEqual => Object::Boolean(left_value == right_value),
            Token::BangEqual => Object::Boolean(left_value != right_value),

            // TODO: Better error messages like rust "cannot add `bool` and `bool`"
            operator => Object::Error(format!(
                "unknown infix operator: {:?} {} {:?}",
                Object::Float(left_value),
                operator,
                Object::Float(right_value)
            )),
        }
    }

    fn eval_boolean_infix_expression(
        &self,
        operator: Token,
        left_value: bool,
        right_value: bool,
    ) -> Object {
        match operator {
            // NOTE: No truthy/implicit conversion
            Token::EqualEqual => Object::Boolean(left_value == right_value),
            Token::BangEqual => Object::Boolean(left_value != right_value),

            operator => Object::Error(format!(
                "unknown infix operator: {:?} {} {:?}",
                Object::Boolean(left_value),
                operator,
                Object::Boolean(right_value)
            )),
        }
    }

    fn eval_if_expression(
        &self,
        condition: Expression,
        consequence: BlockExpression,
        alternative: Option<Expression>,
    ) -> Object {
        let evaluated_condition = self.eval_expression(condition);
        // Early return the first error received
        if evaluated_condition.is_error() {
            return evaluated_condition;
        }

        match evaluated_condition {
            Object::Boolean(value) => {
                if value {
                    // TODO: I don't like repeating this part in both eval_expression and eval_if_expression, maybe consolidate it
                    self.eval_block_expression(consequence)
                        // Blocks return nil unless a value is given
                        .unwrap_or(Object::Nil)
                } else if let Some(alternative) = alternative {
                    self.eval_expression(alternative)
                } else {
                    Object::Nil
                }
            }
            obj => Object::Error(format!("expected `boolean` condition but got {:?}", obj)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{evaluator::Evaluator, lexer::Lexer, object::Object, parser::Parser};

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
    fn error_handling() {
        let tests = vec![
            (
                "5 + true;",
                "type mismatch for +: Integer(5) and Boolean(true)",
            ),
            (
                "5 + true; 5;",
                "type mismatch for +: Integer(5) and Boolean(true)",
            ),
            ("-true", "invalid operand type `boolean` for - operator"),
            (
                "true + false;",
                "unknown infix operator: Boolean(true) + Boolean(false)",
            ),
            (
                "5; true + false; 5",
                "unknown infix operator: Boolean(true) + Boolean(false)",
            ),
            (
                "if 10 > 1 { true + false; }",
                "unknown infix operator: Boolean(true) + Boolean(false)",
            ),
            (
                "if 1 { true + false; }",
                "expected `boolean` condition but got Integer(1)",
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
                "unknown infix operator: Boolean(true) + Boolean(false)",
            ),
        ];

        for (input, expected_error) in tests {
            let evaluated = evaluate(input);
            test_error_object(evaluated, expected_error)
        }
    }

    fn evaluate(input: &str) -> Object {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let prog = p.parse_program();

        match prog {
            Ok(prog) => match Evaluator::new().eval(prog) {
                Some(obj) => obj,
                None => panic!("Expected object result but got None"),
            },
            Err(errors) => {
                println!("parser had {} errors", errors.len());
                for error in errors {
                    println!("parser error: {}", error);
                }
                panic!("parser errors")
            }
        }
    }

    fn test_integer_object(obj: Object, expected_value: i64) {
        match obj {
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

    fn test_float_object(obj: Object, expected_value: f64) {
        match obj {
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
    fn test_boolean_object(obj: Object, expected_value: bool) {
        match obj {
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

    fn test_null_object(obj: Object) {
        if obj != Object::Nil {
            panic!("expected null object but got {:?}", obj)
        }
    }

    fn test_error_object(obj: Object, expected_message: &str) {
        match obj {
            Object::Error(message) => {
                if message != expected_message {
                    panic!(
                        "expected error message to be \"{}\" but got \"{}\"",
                        expected_message, message
                    )
                }
            }
            _ => panic!("expected error object but got {:?}", obj),
        }
    }
}
