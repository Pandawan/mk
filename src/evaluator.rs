use crate::{
    ast::{Expression, Program, Statement},
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
            result = Some(self.eval_statement(stmt));
        }

        return result;
    }

    fn eval_statement(&self, stmt: Statement) -> Object {
        match stmt {
            Statement::Expression { expression } => self.eval_expression(expression),
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
                self.eval_prefix_expression(prefix.operator, right)
            }
            Expression::Infix(infix) => {
                let left = self.eval_expression(infix.left);
                let right = self.eval_expression(infix.right);
                self.eval_infix_expression(infix.operator, left, right)
            }
            _ => todo!("Implement other expressions"),
        }
    }

    fn eval_prefix_expression(&self, operator: Token, right: Object) -> Object {
        match operator {
            Token::Bang => self.eval_bang_operator_expression(right),
            Token::Minus => self.eval_minus_prefix_operator_expression(right),
            _ => todo!("Implement other prefix operators"),
        }
    }

    fn eval_bang_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Boolean(true) => Object::Boolean(false),
            Object::Boolean(false) => Object::Boolean(true),
            _ => todo!("Error for non-bool/nil expression with ! prefix operator"),
        }
    }

    fn eval_minus_prefix_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Integer(value) => Object::Integer(-value),
            Object::Float(value) => Object::Float(-value),
            // TODO: When adding float, add it here too
            _ => todo!("Error for non-integer values with - prefix operator"),
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

            _ => todo!("Error for type mismatch"),
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

            _ => todo!("Error for operator doesn't exist for integers"),
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

            _ => todo!("Error for operator doesn't exist for float"),
        }
    }

    fn eval_boolean_infix_expression(
        &self,
        operator: Token,
        left_value: bool,
        right_value: bool,
    ) -> Object {
        match operator {
            Token::EqualEqual => Object::Boolean(left_value == right_value),
            Token::BangEqual => Object::Boolean(left_value != right_value),
            _ => todo!("Error for operator doesn't exist for boolean"),
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
}
