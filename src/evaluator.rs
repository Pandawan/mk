use crate::{
    ast::{Expression, Program, Statement},
    object::Object,
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
            _ => todo!("Implement other expressions"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{evaluator::Evaluator, lexer::Lexer, object::Object, parser::Parser};

    #[test]
    fn eval_integer_expression() {
        let tests = vec![("5", 5), ("10", 10)];

        for (input, expected_value) in tests {
            let evaluated = evaluate(input);
            test_integer_object(evaluated, expected_value);
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
}
