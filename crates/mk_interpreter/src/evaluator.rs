use std::{cell::RefCell, convert::TryInto, rc::Rc};

use crate::{
    builtin::Builtin,
    environment::Environment,
    error::RuntimeError,
    object::{Array, Function, Object},
};

use mk_parser::{
    ast::{BlockExpression, Expression, IdentifierLiteral, Program, Statement},
    span::WithSpan,
    token::Token,
};

type EvalResult<T> = Result<T, RuntimeError>;

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

    pub fn eval(&mut self, prog: Program) -> EvalResult<Rc<Object>> {
        let mut result = Rc::new(Object::Nil);

        for stmt in &prog.statements {
            let val = self.eval_statement(stmt)?;

            match val.as_ref() {
                // If a return value is found, immediately return and stop evaluating statements
                // Unwrap the return value into a final value so the program can use it
                Object::ReturnValue(inner_value) => return Ok(Rc::clone(inner_value)),
                _ => result = val,
            }
        }

        Ok(result)
    }

    // Similar to eval (for programs) but doesn't unwrap return values
    fn eval_block_expression(
        &mut self,
        block: &BlockExpression,
        with_premade_env: Option<Rc<RefCell<Environment>>>,
    ) -> EvalResult<Rc<Object>> {
        // Open new block scope
        let outer_env = Rc::clone(&self.env);
        let inner_env = match with_premade_env {
            Some(env) => env,
            None => Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(
                &self.env,
            )))),
        };
        self.env = inner_env;

        let mut result = Rc::new(Object::Nil);

        for stmt in &block.statements {
            let val = self.eval_statement(stmt)?;

            match val.as_ref() {
                // If a return value is found, immediately return and stop evaluating statements
                // Don't unwrap the return value, we might be in a nested block which also needs to return
                Object::ReturnValue(_) => return Ok(val),
                _ => result = val,
            }
        }

        // Exit block scope
        self.env = outer_env;

        Ok(result)
    }

    fn eval_statement(&mut self, stmt: &Statement) -> EvalResult<Rc<Object>> {
        match stmt {
            Statement::Expression { expression } => self.eval_expression(expression),
            Statement::Return { value } => {
                let obj = self.eval_expression(value)?;

                Ok(Rc::new(Object::ReturnValue(obj)))
            }
            Statement::Let { name, value } => {
                let obj = self.eval_expression(value)?;

                // Add the variable to the surrounding environment
                self.env.borrow_mut().define(name.to_owned(), obj);

                // TODO: Let returns nil, but it's a statement so maybe it shouldn't return anything? How should that work?
                Ok(Rc::new(Object::Nil))
            }
        }
    }

    fn eval_expression(&mut self, expr: &Expression) -> EvalResult<Rc<Object>> {
        match expr {
            Expression::Integer(value) => Ok(Rc::new(Object::Integer(*value))),
            Expression::Float(value) => Ok(Rc::new(Object::Float(*value))),
            Expression::Boolean(value) => Ok(Rc::new(Object::Boolean(*value))),
            // TODO: Should I really be cloning the String each time? (I also do this from lexer -> parser)
            Expression::String(value) => Ok(Rc::new(Object::String(value.clone()))),
            Expression::Nil => Ok(Rc::new(Object::Nil)),
            Expression::Identifier(identifier) => {
                self.eval_identifier_expression(identifier.clone())
            }

            Expression::Prefix(prefix) => {
                let right = self.eval_expression(&prefix.right)?;
                self.eval_prefix_expression(&prefix.operator, right)
            }
            Expression::Infix(infix) => {
                // TODO: I don't like this special-casing & hardcoding...
                // Short circuiting operators have a different handler
                if infix.operator.value == Token::AndAnd || infix.operator.value == Token::OrOr {
                    return self.eval_infix_expression_with_short_circuiting(
                        &infix.operator,
                        &infix.left,
                        &infix.right,
                    );
                }

                let left = self.eval_expression(&infix.left)?;
                let right = self.eval_expression(&infix.right)?;
                self.eval_infix_expression(&infix.operator, left, right)
            }

            Expression::Assignment(assignment) => {
                let obj = self.eval_expression(&assignment.value)?;

                // Add the variable to the surrounding environment
                let result = self
                    .env
                    .borrow_mut()
                    .assign(assignment.identifier.name.to_owned(), Rc::clone(&obj));

                match result {
                    // Assignment expression returns the value that was just assigned
                    Ok(_) => Ok(obj),
                    Err(err) => Err(err),
                }
            }

            Expression::Block(block) => self.eval_block_expression(block, None),

            Expression::If(if_expr) => self.eval_if_expression(
                &if_expr.condition,
                &if_expr.consequence,
                &if_expr.alternative,
            ),

            Expression::Array(arr) => {
                let elements = self.eval_expressions(&arr.elements)?;
                Ok(Rc::new(Object::Array(Array { elements })))
            }
            Expression::Index(expr) => {
                let left = self.eval_expression(&expr.left)?;
                let index = self.eval_expression(&expr.index)?;
                self.eval_index_expression(left, index)
            }

            Expression::Function(func) => Ok(Rc::new(Object::Function(Function {
                parameters: func.parameters.clone(),
                body: Rc::clone(&func.body),
                env: Rc::clone(&self.env),
            }))),
            Expression::Call(call) => {
                let func = self.eval_expression(&call.function)?;
                let args = self.eval_expressions(&call.arguments)?;
                self.apply_function(func, args)
            }
        }
    }

    fn eval_expressions(&mut self, exprs: &[Expression]) -> EvalResult<Vec<Rc<Object>>> {
        let mut result = Vec::new();
        for expr in exprs {
            let evaluated = self.eval_expression(expr)?;
            result.push(evaluated);
        }
        Ok(result)
    }

    fn eval_identifier_expression(&self, identifier: IdentifierLiteral) -> EvalResult<Rc<Object>> {
        let result = self.env.borrow().get(&identifier.name);

        match result {
            Some(obj) => Ok(obj),
            // If we don't find the identifier, look it up as a builtin
            // NOTE: This means that builtins are not "in environment/scope" like other variables
            None => match Builtin::lookup(&identifier.name) {
                // TODO: Maybe have pre-made Rc objects of builtins, so I don't have to create new ones each time
                Some(builtin) => Ok(Rc::new(Object::Builtin(builtin))),
                None => Err(RuntimeError::IdentifierNotFound(identifier.name)),
            },
        }
    }

    fn eval_prefix_expression(
        &self,
        operator: &WithSpan<Token>,
        right: Rc<Object>,
    ) -> EvalResult<Rc<Object>> {
        match operator.value {
            Token::Bang => self.eval_bang_operator_expression(operator, right),
            Token::Minus => self.eval_minus_prefix_operator_expression(operator, right),
            // NOTE: Evaluator incorrectly asked to evaluate given operator as a prefix
            _ => panic!(
                "unknown prefix operator {op}{r:?} {at}",
                op = operator.value,
                r = right,
                at = operator.span.at_str()
            ),
        }
    }

    fn eval_bang_operator_expression(
        &self,
        operator: &WithSpan<Token>,
        right: Rc<Object>,
    ) -> EvalResult<Rc<Object>> {
        let obj = match *right {
            Object::Boolean(true) => Object::Boolean(false),
            Object::Boolean(false) => Object::Boolean(true),
            _ => {
                return Err(RuntimeError::InvalidPrefixOperandType(
                    operator.clone(),
                    right,
                ))
            }
        };
        Ok(Rc::new(obj))
    }

    fn eval_minus_prefix_operator_expression(
        &self,
        operator: &WithSpan<Token>,
        right: Rc<Object>,
    ) -> EvalResult<Rc<Object>> {
        let obj = match *right {
            Object::Integer(value) => Object::Integer(-value),
            Object::Float(value) => Object::Float(-value),
            _ => {
                return Err(RuntimeError::InvalidPrefixOperandType(
                    operator.clone(),
                    right,
                ))
            }
        };
        Ok(Rc::new(obj))
    }

    fn eval_infix_expression_with_short_circuiting(
        &mut self,
        operator: &WithSpan<Token>,
        left_expr: &Expression,
        right_expr: &Expression,
    ) -> EvalResult<Rc<Object>> {
        if operator.value != Token::AndAnd && operator.value != Token::OrOr {
            panic!("Only && and || operators can short circuit!")
        };

        let left = self.eval_expression(left_expr)?;

        // At this point, know operator can short circuit
        match (left.as_ref(), &operator.value) {
            // Short-circuiting cases
            // false && <>
            (Object::Boolean(false), Token::AndAnd) => Ok(Rc::new(Object::Boolean(false))),
            // true || <>
            (Object::Boolean(true), Token::OrOr) => Ok(Rc::new(Object::Boolean(true))),

            // Non-short-circuiting case
            (Object::Boolean(left_value), Token::AndAnd | Token::OrOr) => {
                let right = self.eval_expression(right_expr)?;

                match right.as_ref() {
                    Object::Boolean(right_value) => {
                        self.eval_boolean_infix_expression(operator, *left_value, *right_value)
                    }
                    _ => Err(RuntimeError::InvalidLogicalInfixOperandType(
                        operator.clone(),
                        left,
                        Some(right),
                    )),
                }
            }

            // Invalid left operand type
            _ => Err(RuntimeError::InvalidLogicalInfixOperandType(
                operator.clone(),
                left,
                None,
            )),
        }
    }

    fn eval_infix_expression(
        &self,
        operator: &WithSpan<Token>,
        left: Rc<Object>,
        right: Rc<Object>,
    ) -> EvalResult<Rc<Object>> {
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

            // TODO: Have == and != result in true/false regardless of type (instead of invalid operand error)
            (_, _) => Err(RuntimeError::InvalidInfixOperandType(
                operator.clone(),
                left,
                right,
            )),
        }
    }

    fn eval_integer_infix_expression(
        &self,
        operator: &WithSpan<Token>,
        left_value: i64,
        right_value: i64,
    ) -> EvalResult<Rc<Object>> {
        let obj = match operator.value {
            // TODO: Replace all of these to have only one Ok(Rc::new())
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

            _ => {
                return Err(RuntimeError::InvalidInfixOperandType(
                    operator.clone(),
                    // TODO: Find a way to keep using the previous Object::Integer rather than creating a new one
                    Rc::new(Object::Integer(left_value)),
                    Rc::new(Object::Integer(right_value)),
                ));
            }
        };
        Ok(Rc::new(obj))
    }

    fn eval_float_infix_expression(
        &self,
        operator: &WithSpan<Token>,
        left_value: f64,
        right_value: f64,
    ) -> EvalResult<Rc<Object>> {
        let obj = match operator.value {
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

            _ => {
                return Err(RuntimeError::InvalidInfixOperandType(
                    operator.clone(),
                    // TODO: Find a way to keep using the previous Object::Integer rather than creating a new one
                    Rc::new(Object::Float(left_value)),
                    Rc::new(Object::Float(right_value)),
                ));
            }
        };
        Ok(Rc::new(obj))
    }

    fn eval_boolean_infix_expression(
        &self,
        operator: &WithSpan<Token>,
        left_value: bool,
        right_value: bool,
    ) -> EvalResult<Rc<Object>> {
        let obj = match operator.value {
            // NOTE: No truthy/implicit conversion
            Token::EqualEqual => Object::Boolean(left_value == right_value),
            Token::BangEqual => Object::Boolean(left_value != right_value),

            Token::AndAnd => Object::Boolean(left_value && right_value),
            Token::OrOr => Object::Boolean(left_value || right_value),

            _ => {
                return Err(RuntimeError::InvalidInfixOperandType(
                    operator.clone(),
                    // TODO: Find a way to keep using the previous Object::Boolean rather than creating a new one
                    Rc::new(Object::Boolean(left_value)),
                    Rc::new(Object::Boolean(right_value)),
                ));
            }
        };
        Ok(Rc::new(obj))
    }

    fn eval_string_infix_expression(
        &self,
        operator: &WithSpan<Token>,
        left_value: &str,
        right_value: &str,
    ) -> EvalResult<Rc<Object>> {
        let obj = match operator.value {
            Token::Plus => Object::String(left_value.to_owned() + right_value),
            Token::EqualEqual => Object::Boolean(left_value == right_value),
            Token::BangEqual => Object::Boolean(left_value != right_value),

            _ => {
                return Err(RuntimeError::InvalidInfixOperandType(
                    operator.clone(),
                    // TODO: Find a way to keep using the previous Object::String rather than creating a new one
                    Rc::new(Object::String(left_value.to_owned())),
                    Rc::new(Object::String(right_value.to_owned())),
                ));
            }
        };
        Ok(Rc::new(obj))
    }

    fn eval_index_expression(
        &mut self,
        left: Rc<Object>,
        index: Rc<Object>,
    ) -> EvalResult<Rc<Object>> {
        match left.as_ref() {
            Object::Array(_) => self.eval_array_index_expression(left, index),
            _ => Err(RuntimeError::IndexNotSupported(left)),
        }
    }

    fn eval_array_index_expression(
        &mut self,
        array: Rc<Object>,
        index: Rc<Object>,
    ) -> EvalResult<Rc<Object>> {
        // TODO: Avoid this?
        let arr = match array.as_ref() {
            Object::Array(arr) => arr,
            _ => unreachable!("Should always be an array!"),
        };

        match index.as_ref() {
            Object::Integer(i) => {
                let length: i64 = arr.elements.len().try_into().unwrap();

                // Out of bounds
                if *i <= -length || *i >= length {
                    return Err(RuntimeError::IndexOutOfBounds { array, index });
                }

                let i = if *i >= 0 {
                    *i as usize
                } else {
                    (length + *i) as usize
                };

                match arr.elements.get(i) {
                    Some(el) => Ok(Rc::clone(el)),
                    // TODO: Should this be an error?
                    None => Ok(Rc::new(Object::Nil)),
                }
            }
            _ => Err(RuntimeError::InvalidIndexOperandType(index)),
        }
    }

    fn eval_if_expression(
        &mut self,
        condition: &Expression,
        consequence: &BlockExpression,
        alternative: &Option<Expression>,
    ) -> EvalResult<Rc<Object>> {
        let evaluated_condition = self.eval_expression(condition)?;

        match *evaluated_condition {
            Object::Boolean(value) => {
                if value {
                    self.eval_block_expression(consequence, None)
                } else if let Some(alternative) = alternative {
                    self.eval_expression(alternative)
                } else {
                    Ok(Rc::new(Object::Nil))
                }
            }
            _ => Err(RuntimeError::ExpectedBooleanCondition(evaluated_condition)),
        }
    }

    fn apply_function(
        &mut self,
        func: Rc<Object>,
        args: Vec<Rc<Object>>,
    ) -> EvalResult<Rc<Object>> {
        match func.as_ref() {
            Object::Function(func) => {
                // Check that number of args & params matches
                if args.len() != func.parameters.len() {
                    return Err(RuntimeError::BadArity {
                        expected: func.parameters.len(),
                        got: args.len(),
                    });
                }

                // Create a new scoped environment for function
                let mut scoped_env = Environment::new_enclosed(Rc::clone(&func.env));

                // Add arguments as variables in function's environment
                for (ident, obj) in func.parameters.iter().zip(args.iter()) {
                    scoped_env.define(ident.name.clone(), Rc::clone(obj));
                }

                // Actually evaluate the function
                // NOTE: No need to set/unset environment, this is done by eval_block_expression using `with_premade_env`
                let result =
                    self.eval_block_expression(&func.body, Some(Rc::new(RefCell::new(scoped_env))));

                result
            }
            // Builtins handle themselves
            Object::Builtin(builtin) => match builtin.apply(args) {
                Ok(obj) => Ok(obj),
                Err(err) => Err(err),
            },
            _ => Err(RuntimeError::NotAFunction(func)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{
        builtin::Builtin,
        error::RuntimeError,
        evaluator::Evaluator,
        object::{Array, Object},
    };

    use mk_parser::{
        lexer::Lexer,
        parser::Parser,
        span::{BytePos, Span, WithSpan},
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
            let evaluated = evaluate(input).unwrap();
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
            let evaluated = evaluate(input).unwrap();
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
            let evaluated = evaluate(input).unwrap();
            test_boolean_object(evaluated, expected_value);
        }
    }

    #[test]
    fn eval_boolean_logical_expression() {
        let tests = vec![
            ("true && true", Object::Boolean(true)),
            ("true && false", Object::Boolean(false)),
            ("false && true", Object::Boolean(false)),
            ("false && false", Object::Boolean(false)),
            ("true || true", Object::Boolean(true)),
            ("true || false", Object::Boolean(true)),
            ("false || true", Object::Boolean(true)),
            ("false || false", Object::Boolean(false)),
            (
                "let calls = 0; false && { calls = 1; true }; calls",
                Object::Integer(0), // Short circuit
            ),
            (
                "let calls = 0; true && { calls = 1; true }; calls",
                Object::Integer(1), // Evaluate right (not short circuiting)
            ),
            (
                "let calls = 0; false || { calls = 1; true }; calls",
                Object::Integer(1), // Evaluate right (not short circuiting)
            ),
            (
                "let calls = 0; true || { calls = 1; true }; calls",
                Object::Integer(0), // Short circuit
            ),
        ];

        for (input, expected_obj) in tests {
            let evaluated = evaluate(input).unwrap();
            match expected_obj {
                Object::Boolean(expected_value) => {
                    test_boolean_object(evaluated, expected_value);
                }
                Object::Integer(expected_value) => test_integer_object(evaluated, expected_value),
                _ => panic!("expected boolean or integer but got {}", expected_obj),
            }
        }
    }

    #[test]
    fn eval_string_expression() {
        let tests = vec![
            ("\"hello world\"", "hello world"),
            ("\"hello\" + \" \" + \"world\"", "hello world"),
        ];

        for (input, expected_value) in tests {
            let evaluated = evaluate(input).unwrap();
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
            let evaluated = evaluate(input).unwrap();
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

        for (input, expected_obj) in tests {
            let evaluated = evaluate(input).unwrap();

            match expected_obj {
                Object::Integer(expected_value) => test_integer_object(evaluated, expected_value),
                Object::Nil => test_nil_object(evaluated),
                _ => panic!("expected integer or nil but got {}", expected_obj),
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
            let evaluated = evaluate(input).unwrap();
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
            let evaluated = evaluate(input).unwrap();
            test_integer_object(evaluated, expected_value)
        }
    }

    #[test]
    fn eval_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = evaluate(input).unwrap();

        match evaluated.as_ref() {
            Object::Array(arr) => {
                if arr.elements.len() != 3 {
                    panic!(
                        "expected array object with 3 elements but got {} ({})",
                        arr.elements.len(),
                        arr
                    )
                }

                test_integer_object(Rc::clone(&arr.elements[0]), 1);
                test_integer_object(Rc::clone(&arr.elements[1]), 4);
                test_integer_object(Rc::clone(&arr.elements[2]), 6);
            }
            obj => panic!("expected array object but got {}", obj),
        }
    }

    #[test]
    fn eval_array_index_expression() {
        let tests = vec![
            ("[1, 2, 3][0]", 1),
            ("[1, 2, 3][1]", 2),
            ("[1, 2, 3][2]", 3),
            ("let i = 0; [1][i];", 1),
            ("[1, 2, 3][1 + 1];", 3),
            ("let myArray = [1, 2, 3]; myArray[2];", 3),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                6,
            ),
            ("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", 2),
            ("[1, 2, 3][-1]", 3),
        ];

        for (input, expected_value) in tests {
            let evaluated = evaluate(input).unwrap();
            test_integer_object(evaluated, expected_value)
        }
    }

    #[test]
    fn eval_block_expression() {
        let tests = vec![
            // Return value
            ("{ }", Object::Nil),
            ("{ 6 }", Object::Integer(6)),
            ("{ 6; 7 }", Object::Integer(7)),
        ];

        for (input, expected_value) in tests {
            let evaluated = evaluate(input).unwrap();

            match expected_value {
                Object::Integer(expected_value) => test_integer_object(evaluated, expected_value),
                Object::Nil => test_nil_object(evaluated),
                _ => unimplemented!(),
            }
        }
    }

    #[test]
    fn eval_scoping() {
        let tests = vec![
            // If
            ("let x = 5; if true { x = 6; }; x", Object::Integer(6)),
            ("let x = 5; if true { let x = 6; }; x", Object::Integer(5)),
            // Block
            ("let x = 5; { x = 6; }; x", Object::Integer(6)),
            ("let x = 5; { let x = 6; }; x", Object::Integer(5)),
            // Function
            ("let x = 5; (fn() { x = 6; })(); x", Object::Integer(6)),
            ("let x = 5; (fn(){ let x = 6; })(); x", Object::Integer(5)),
            ("let x = 5; (fn(x){ x = 10; x })(x)", Object::Integer(10)),
            ("let x = 5; (fn(x){ x = 10; })(x); x", Object::Integer(5)),
        ];

        for (input, expected_value) in tests {
            let evaluated = evaluate(input).unwrap();

            match expected_value {
                Object::Integer(expected_value) => test_integer_object(evaluated, expected_value),
                Object::Nil => test_nil_object(evaluated),
                _ => unimplemented!(),
            }
        }
    }

    #[test]
    fn eval_function_expression() {
        let input = "fn (x) { x + 2; }";
        let evaluated = evaluate(input).unwrap();

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
            let evaluated = evaluate(input).unwrap();
            test_integer_object(evaluated, expected_value);
        }
    }

    #[test]
    fn eval_builtin_functions() {
        let tests = vec![
            ("len('')", Ok(Object::Integer(0))),
            ("len('four')", Ok(Object::Integer(4))),
            ("len('hello world')", Ok(Object::Integer(11))),
            (
                "len(1)",
                Err(RuntimeError::InvalidArgumentType(
                    Builtin::Len,
                    Rc::new(Object::Integer(1)),
                )),
            ),
            (
                "len('hello', 'world')",
                Err(RuntimeError::BadArity {
                    expected: 1,
                    got: 2,
                }),
            ),
            ("len([])", Ok(Object::Integer(0))),
            ("len([1])", Ok(Object::Integer(1))),
            ("len([1, 'hello world', []])", Ok(Object::Integer(3))),
            ("typeof(1)", Ok(Object::String("integer".to_owned()))),
            ("typeof('string')", Ok(Object::String("string".to_owned()))),
            ("typeof(typeof)", Ok(Object::String("builtin".to_owned()))),
        ];

        for (input, expected_value) in tests {
            let evaluated = evaluate(input);

            match evaluated {
                Ok(evaluated_obj) => match expected_value {
                    Ok(Object::Integer(expected_value)) => {
                        test_integer_object(evaluated_obj, expected_value)
                    }
                    Ok(Object::String(expected_value)) => {
                        test_string_object(evaluated_obj, &expected_value)
                    }
                    Ok(_) => unimplemented!(),
                    Err(_) => panic!("expected error object but got {:?}", evaluated_obj),
                },
                Err(evaluated_err) => match expected_value {
                    Ok(expected_obj) => panic!(
                        "expected {} object but got error {:?}",
                        expected_obj.typename(),
                        evaluated_err
                    ),
                    Err(expected_error) => {
                        assert_eq!(
                            expected_error, evaluated_err,
                            "expected error to be {:?} but got {:?}",
                            expected_error, evaluated_err
                        )
                    }
                },
            }
        }
    }

    #[test]
    fn error_handling() {
        let tests = vec![
            (
                "5 + true;",
                RuntimeError::InvalidInfixOperandType(
                    WithSpan::new(Token::Plus, Span::new(BytePos::new(2), BytePos::new(3))),
                    Rc::new(Object::Integer(5)),
                    Rc::new(Object::Boolean(true)),
                ),
            ),
            (
                "5 + true; 5;",
                RuntimeError::InvalidInfixOperandType(
                    WithSpan::new(Token::Plus, Span::new(BytePos::new(2), BytePos::new(3))),
                    Rc::new(Object::Integer(5)),
                    Rc::new(Object::Boolean(true)),
                ),
            ),
            (
                "-true",
                RuntimeError::InvalidPrefixOperandType(
                    WithSpan::new(Token::Minus, Span::new(BytePos::new(0), BytePos::new(1))),
                    Rc::new(Object::Boolean(true)),
                ),
            ),
            (
                "true + false;",
                RuntimeError::InvalidInfixOperandType(
                    WithSpan::new(Token::Plus, Span::new(BytePos::new(5), BytePos::new(6))),
                    Rc::new(Object::Boolean(true)),
                    Rc::new(Object::Boolean(false)),
                ),
            ),
            (
                "\"hello\" - \"world\";",
                RuntimeError::InvalidInfixOperandType(
                    WithSpan::new(Token::Minus, Span::new(BytePos::new(8), BytePos::new(9))),
                    Rc::new(Object::String("hello".to_string())),
                    Rc::new(Object::String("world".to_string())),
                ),
            ),
            (
                "5; true + false; 5",
                RuntimeError::InvalidInfixOperandType(
                    WithSpan::new(Token::Plus, Span::new(BytePos::new(8), BytePos::new(9))),
                    Rc::new(Object::Boolean(true)),
                    Rc::new(Object::Boolean(false)),
                ),
            ),
            (
                "if 10 > 1 { true + false; }",
                RuntimeError::InvalidInfixOperandType(
                    WithSpan::new(Token::Plus, Span::new(BytePos::new(17), BytePos::new(18))),
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
                    WithSpan::new(Token::Plus, Span::new(BytePos::new(95), BytePos::new(96))),
                    Rc::new(Object::Boolean(true)),
                    Rc::new(Object::Boolean(false)),
                ),
            ),
            ("foobar", RuntimeError::IdentifierNotFound("foobar".into())),
            (
                "foobar = 5",
                RuntimeError::IdentifierNotFound("foobar".into()),
            ),
            (
                "[0, 1, 2][3]",
                RuntimeError::IndexOutOfBounds {
                    array: Rc::new(Object::Array(Array {
                        elements: vec![
                            Rc::new(Object::Integer(0)),
                            Rc::new(Object::Integer(1)),
                            Rc::new(Object::Integer(2)),
                        ],
                    })),
                    index: Rc::new(Object::Integer(3)),
                },
            ),
        ];

        for (input, expected_error) in tests {
            let evaluated = evaluate(input);

            match evaluated {
                Ok(evaluated_obj) => panic!("expected error but got {:?}", evaluated_obj),
                Err(evaluated_err) => assert_eq!(
                    expected_error, evaluated_err,
                    "expected error to be {:?} but got {:?}",
                    expected_error, evaluated_err
                ),
            }
        }
    }

    fn evaluate(input: &str) -> Result<Rc<Object>, RuntimeError> {
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
                panic!("parser errors");
            }
        }
    }

    fn test_integer_object(obj: Rc<Object>, expected_value: i64) {
        match *obj {
            Object::Integer(value) => {
                assert_eq!(
                    expected_value, value,
                    "expected integer object with value {} but got {:?}",
                    expected_value, obj
                )
            }
            _ => panic!("expected integer object but got {:?}", obj),
        }
    }

    fn test_float_object(obj: Rc<Object>, expected_value: f64) {
        match *obj {
            Object::Float(value) => {
                assert_eq!(
                    expected_value, value,
                    "expected float object with value {} but got {:?}",
                    expected_value, obj
                )
            }
            _ => panic!("expected float object but got {:?}", obj),
        }
    }

    fn test_boolean_object(obj: Rc<Object>, expected_value: bool) {
        match *obj {
            Object::Boolean(value) => {
                assert_eq!(
                    expected_value, value,
                    "expected boolean object with value {} but got {:?}",
                    expected_value, obj
                )
            }
            _ => panic!("expected boolean object but got {:?}", obj),
        }
    }

    fn test_string_object(obj: Rc<Object>, expected_value: &str) {
        match obj.as_ref() {
            Object::String(value) => {
                assert_eq!(
                    expected_value, value,
                    "expected string object with value {} but got {:?}",
                    expected_value, obj
                )
            }
            _ => panic!("expected string object but got {:?}", obj),
        }
    }

    fn test_nil_object(obj: Rc<Object>) {
        match *obj {
            Object::Nil => {}
            _ => panic!("expected null object but got {:?}", obj),
        }
    }
}
