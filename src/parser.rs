use std::fmt::Display;

use crate::ast::{Expression, Program, Statement};
use crate::{lexer::Lexer, token::Token};

#[derive(Debug)]
pub enum ParseError {
    Unexpected(Token),
    // TODO: Might want to use TokenKind to allow for Expected(TokenKind, Token)
    Expected(String, Token),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Unexpected(token) => write!(f, "Unexpected token {}", token),
            ParseError::Expected(expected, got) => {
                write!(
                    f,
                    "Expected next token to be {}, but got {} instead",
                    expected, got
                )
            }
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

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

    pub fn parse_program(&mut self) -> Result<Program, Vec<ParseError>> {
        let mut program = Program::new();
        let mut errors: Vec<ParseError> = Vec::new();

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

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => Err(ParseError::Unexpected(self.current_token.clone())),
        }
    }

    fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        let name = self.expect_peek_identifier()?;

        self.expect_peek(Token::Equal)?;

        // TODO: Parse expression
        while !self.current_token_is(Token::Semicolon) && !self.current_token_is(Token::Eof) {
            self.next_token();
        }

        Ok(Statement::Let {
            name,
            value: Expression::Nil,
        })
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        // Consume the `return` token
        self.next_token();

        // TODO: Parse expression
        while !self.current_token_is(Token::Semicolon) && !self.current_token_is(Token::Eof) {
            self.next_token();
        }

        Ok(Statement::Return {
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

    fn expect_peek(&mut self, token: Token) -> ParseResult<()> {
        if self.peek_token_is(&token) {
            self.next_token();
            Ok(())
        } else {
            Err(ParseError::Expected(
                token.to_string(),
                self.peek_token.clone(),
            ))
        }
    }

    fn expect_peek_identifier(&mut self) -> ParseResult<String> {
        let name = match &self.peek_token {
            Token::Identifier(name) => name.to_owned(),
            _ => {
                return Err(ParseError::Expected(
                    String::from("identifier"),
                    self.peek_token.clone(),
                ))
            }
        };

        self.next_token();
        Ok(name)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Program, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_let_statement() {
        let input = "\
        let x = 5;
        let y = 10;
        let foobar = 838383;";

        let prog = setup(input, 3);

        let tests = vec!["x", "y", "foobar"];

        for (statement, test) in prog.statements.iter().zip(tests.iter()) {
            assert_eq!(
                *statement,
                Statement::Let {
                    name: test.to_string(),
                    // TODO: Check value
                    value: Expression::Nil
                }
            );
        }
    }

    #[test]
    fn test_return_statement() {
        let input = "\
        return 5;
        return y;";

        let prog = setup(input, 2);

        for statement in prog.statements {
            assert_eq!(
                statement,
                Statement::Return {
                    // TODO: Check value
                    value: Expression::Nil
                }
            );
        }
    }

    fn setup(input: &str, stmt_count: usize) -> Program {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let prog = p.parse_program();

        match prog {
            Ok(prog) => {
                if stmt_count != 0 && prog.statements.len() != stmt_count {
                    panic!(
                        "expected {} statement(s) for '{}' but got {:?}",
                        stmt_count, input, prog.statements
                    )
                }

                prog
            }
            Err(errors) => {
                println!("parser had {} errors", errors.len());
                for error in errors {
                    println!("parser error: {}", error);
                }
                panic!("parser errors")
            }
        }
    }
}
