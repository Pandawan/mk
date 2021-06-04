use crate::ast::{Expression, Program, Statement};
use crate::{lexer::Lexer, token::Token};

// TODO: Change this to an actual error type
type ParserError = String;

pub struct Parser<'a> {
    lexer: Lexer<'a>,

    current_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Parser<'a> {
        let cur = lexer.next_token();
        let next = lexer.next_token();
        Parser {
            lexer,
            current_token: cur,
            peek_token: next,
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, Vec<ParserError>> {
        let mut program = Program::new();
        let mut errors: Vec<ParserError> = Vec::new();

        while self.current_token != Token::Eof {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(error) => errors.push(error),
            }
            self.next_token();
        }

        if errors.len() != 0 {
            return Err(errors);
        }

        return Ok(program);
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            _ => Err(format!("Token {} not recognized", self.current_token)),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let name = self.expect_identifier()?;

        self.expect_peek(Token::Equal)?;

        // TODO: Parse expression
        while !self.current_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Let {
            name,
            value: Expression::Nil,
        })
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn current_token_is(&self, token: Token) -> bool {
        match (&token, &self.current_token) {
            (Token::Identifier(_), Token::Identifier(_)) => true,
            (Token::Number(_), Token::Number(_)) => true,
            _ => token == self.current_token,
        }
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        match (&token, &self.peek_token) {
            (Token::Identifier(_), Token::Identifier(_)) => true,
            (Token::Number(_), Token::Number(_)) => true,
            _ => token == &self.peek_token,
        }
    }

    fn expect_peek(&mut self, token: Token) -> Result<(), ParserError> {
        if self.peek_token_is(&token) {
            self.next_token();
            Ok(())
        } else {
            Err(format!(
                "expected next token to be {} but got {} instead",
                token, self.peek_token
            ))
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParserError> {
        let name = match &self.peek_token {
            Token::Identifier(name) => name.to_owned(),
            _ => return Err(format!("invalid identifier {}", self.peek_token)),
        };

        self.next_token();
        Ok(name)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Program, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn let_statement() {
        let input = "\
        let x = 5;
        let y = 10;
        let foobar = 838383;";

        let prog = setup(input, 3);

        let tests = vec!["x", "y", "foobar"];

        let mut itr = prog.statements.iter();

        for t in tests {
            match itr.next().unwrap() {
                Statement::Let { name, value: _ } => {
                    assert_eq!(name, t);
                }
                stmt => panic!("Unknown statement {:?}", stmt),
            }
        }
    }

    fn setup(input: &str, stmt_count: usize) -> Program {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let prog = p.parse_program().unwrap();

        if stmt_count != 0 && prog.statements.len() != stmt_count {
            panic!(
                "expected {} statement(s) for '{}' but got {:?}",
                stmt_count, input, prog.statements
            )
        }

        prog
    }
}
