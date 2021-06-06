use std::fmt::Display;

use crate::ast::{Expression, InfixExpression, PrefixExpression, Program, Statement};
use crate::{lexer::Lexer, token::Token};

#[derive(Debug)]
pub enum ParseError {
    Unexpected(Token),
    // TODO: Might want to use TokenKind to allow for Expected(TokenKind, Token)
    Expected(String, Token),

    InvalidPrefixFn(Token),
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
            ParseError::InvalidPrefixFn(token) => {
                write!(f, "No prefix parsing function found for token {}", token)
            }
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

type PrefixFn = fn(parser: &mut Parser<'_>) -> ParseResult<Expression>;
type InfixFn = fn(parser: &mut Parser<'_>, left: Expression) -> ParseResult<Expression>;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    fn token_precedence(tok: &Token) -> Precedence {
        match tok {
            Token::EqualEqual => Precedence::Equals,
            Token::BangEqual => Precedence::Equals,
            Token::LessThan => Precedence::LessGreater,
            Token::GreaterThan => Precedence::LessGreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Slash => Precedence::Product,
            Token::Star => Precedence::Product,
            // Token::LeftParen => Precedence::Call,
            // Token::LeftBrace => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

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
            _ => self.parse_expression_statement(),
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

    fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        // Consume the semicolon at the end of the statement
        // TODO: Optional semicolon? What about for let & return?
        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expression { expression: expr })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParseResult<Expression> {
        let mut left_expr;

        // Parse the current token (either as a prefix or as a literal)
        if let Some(prefix_fn) = self.get_prefix_fn(&self.current_token) {
            left_expr = prefix_fn(self)?;
        } else {
            return Err(ParseError::InvalidPrefixFn(self.current_token.clone()));
        }

        // At this point the last token of the left_expr is still current_token
        // (if not below, the token will be consumed by next_token() in parse_program)

        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_precedence() {
            // Check if the peek token is an operator (and has an infix function)
            if let Some(infix_fn) = self.get_infix_fn(&self.peek_token) {
                // Move the infix operator to be current_token
                self.next_token();
                // Parse the infix expression
                left_expr = infix_fn(self, left_expr)?;
            }
            // If no infix function is found for the peek token, then this is the end of the expression
        }

        Ok(left_expr)
    }

    fn get_prefix_fn(&self, token: &Token) -> Option<PrefixFn> {
        match token {
            Token::Identifier(_) => Some(Parser::parse_identifier_expression),
            Token::Number(_) => Some(Parser::parse_number_expression),

            Token::Bang | Token::Minus => Some(Parser::parse_prefix_expression),
            _ => None,
        }
    }

    fn get_infix_fn(&self, token: &Token) -> Option<InfixFn> {
        match token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Star
            | Token::EqualEqual
            | Token::BangEqual
            | Token::LessThan
            | Token::GreaterThan => Some(Parser::parse_infix_expression),
            _ => None,
        }
    }

    fn parse_identifier_expression(parser: &mut Parser<'_>) -> ParseResult<Expression> {
        if let Token::Identifier(ref name) = parser.current_token {
            Ok(Expression::Identifier(name.clone()))
        } else {
            Err(ParseError::Expected(
                "identifier".to_string(),
                parser.current_token.clone(),
            ))
        }
    }

    fn parse_number_expression(parser: &mut Parser<'_>) -> ParseResult<Expression> {
        if let Token::Number(value) = parser.current_token {
            Ok(Expression::Number(value))
        } else {
            Err(ParseError::Expected(
                "number".to_string(),
                parser.current_token.clone(),
            ))
        }
    }

    fn parse_prefix_expression(parser: &mut Parser<'_>) -> ParseResult<Expression> {
        let operator = parser.current_token.clone();
        // Consume the operator token
        parser.next_token();
        let right = parser.parse_expression(Precedence::Prefix)?;
        Ok(Expression::Prefix(Box::new(PrefixExpression {
            operator,
            right,
        })))
    }

    fn parse_infix_expression(
        parser: &mut Parser<'_>,
        left: Expression,
    ) -> ParseResult<Expression> {
        let operator = parser.current_token.clone();
        let precedence = parser.current_precedence();
        parser.next_token();

        let right = parser.parse_expression(precedence)?;

        Ok(Expression::Infix(Box::new(InfixExpression {
            left,
            operator,
            right,
        })))
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

    fn current_precedence(&self) -> Precedence {
        Precedence::token_precedence(&self.current_token)
    }

    fn peek_precedence(&self) -> Precedence {
        Precedence::token_precedence(&self.peek_token)
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
    use crate::token::Token;

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

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let prog = setup(input, 1);

        assert_eq!(
            prog.statements[0],
            Statement::Expression {
                expression: Expression::Identifier("foobar".to_string())
            }
        )
    }

    #[test]
    fn test_number_expression() {
        let input = "5;";

        let prog = setup(input, 1);

        assert_eq!(
            prog.statements[0],
            Statement::Expression {
                expression: Expression::Number(5)
            }
        )
    }

    #[test]
    fn test_prefix_expression() {
        let tests: Vec<(&str, Token, u64)> =
            vec![("!5;", Token::Bang, 5), ("-15", Token::Minus, 15)];

        for (input, op, right) in tests {
            let prog = setup(input, 1);

            let expr = unwrap_expression(&prog);

            match expr {
                Expression::Prefix(expr) => {
                    assert_eq!(
                        op, expr.operator,
                        "expected operator {} but got {}",
                        op, expr.operator,
                    );
                    test_number_literal(&expr.right, right);
                }
                expr => panic!("expected prefix expression but got {}", expr),
            }
        }
    }

    #[test]
    fn test_infix_expression() {
        let tests: Vec<(&str, u64, Token, u64)> = vec![
            ("5 + 5;", 5, Token::Plus, 5),
            ("5 - 5;", 5, Token::Minus, 5),
            ("5 * 5;", 5, Token::Star, 5),
            ("5 / 5;", 5, Token::Slash, 5),
            ("5 > 5;", 5, Token::GreaterThan, 5),
            ("5 < 5;", 5, Token::LessThan, 5),
            ("5 == 5;", 5, Token::EqualEqual, 5),
            ("5 != 5;", 5, Token::BangEqual, 5),
        ];

        for (input, left, op, right) in tests {
            let prog = setup(input, 1);

            let expr = unwrap_expression(&prog);

            match expr {
                Expression::Infix(expr) => {
                    test_number_literal(&expr.left, left);
                    assert_eq!(
                        op, expr.operator,
                        "expected operator {} but got {}",
                        op, expr.operator,
                    );
                    test_number_literal(&expr.right, right);
                }
                expr => panic!("expected prefix expression but got {}", expr),
            }
        }
    }

    #[test]
    fn test_operator_precedence() {
        // Tests: (input, expected)
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for (input, expected) in tests {
            let prog = setup(input, 0).to_string();

            assert_eq!(expected, prog, "expected '{}' but got '{}'", expected, prog)
        }
    }

    fn unwrap_expression(prog: &Program) -> &Expression {
        match prog.statements.first().unwrap() {
            Statement::Expression { expression } => &expression,
            stmt => panic!("{:?} isn't an expression statement", stmt),
        }
    }

    fn test_number_literal(expr: &Expression, value: u64) {
        match expr {
            Expression::Number(num) => {
                assert_eq!(value, *num, "expected {} but got {}", value, num)
            }
            _ => panic!("expected number literal {} but got {}", value, expr),
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
