use std::fmt::Display;
use std::rc::Rc;

use crate::ast::{
    BlockExpression, CallExpression, Expression, FunctionLiteral, IdentifierLiteral, IfExpression,
    InfixExpression, PrefixExpression, Program, Statement,
};
use crate::lexer::LexError;
use crate::{lexer::Lexer, token::Token};

#[derive(Debug)]
pub enum ParseError {
    Unexpected(Token),
    // TODO: Might want to use TokenKind to allow for Expected(TokenKind, Token)
    Expected(String, Token),

    InvalidPrefixFn(Token),

    // Wrapper for LexErrors to bubble up
    SyntaxError(LexError),
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
            ParseError::SyntaxError(err) => {
                write!(f, "Syntax Error: {}", err)
            }
        }
    }
}

impl LexError {
    fn to_parse_error(self) -> ParseError {
        ParseError::SyntaxError(self)
    }
}

type ParseResult<T> = Result<T, ParseError>;

type PrefixFn = fn(parser: &mut Parser<'_>) -> ParseResult<Expression>;
type InfixFn = fn(parser: &mut Parser<'_>, left: Expression) -> ParseResult<Expression>;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    // Exponent
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
            Token::LeftParen => Precedence::Call,
            // Token::StarStar => Precedence::Exponent
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
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        Parser {
            lexer,
            // TODO: Better lexing error handling when consuming in parser
            current_token: Token::Eof,
            peek_token: Token::Eof,
        }
    }

    /// Parse the entire input as a program.
    pub fn parse_program(&mut self) -> Result<Program, Vec<ParseError>> {
        let mut program = Program::new();
        let mut errors: Vec<ParseError> = Vec::new();

        // Prepare parser by fetching first two tokens
        match self.lexer.next_token() {
            Ok(tok) => self.current_token = tok,
            Err(err) => return Err(vec![ParseError::SyntaxError(err)]),
        };
        match self.lexer.next_token() {
            Ok(tok) => self.peek_token = tok,
            Err(err) => return Err(vec![ParseError::SyntaxError(err)]),
        };

        while self.current_token != Token::Eof {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                // Encountering a SyntaxError should exit out immediately
                Err(ParseError::SyntaxError(err)) => return Err(vec![err.to_parse_error()]),
                Err(error) => errors.push(error),
            }

            // Consume the next token
            self.next_token()
                // exit out if there was a LexError
                .map_err(|err| vec![err])?;
        }

        if errors.is_empty() == false {
            return Err(errors);
        }

        Ok(program)
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

        // Consume equal sign
        self.next_token()?;

        let value = self.parse_expression(Precedence::Lowest)?;

        // Consume the semicolon at the end of the statement
        // TODO: Optional semicolon?
        if self.peek_token_is(&Token::Semicolon) {
            self.next_token()?;
        }

        Ok(Statement::Let { name, value })
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        // Consume the `return` token
        self.next_token()?;

        let value = self.parse_expression(Precedence::Lowest)?;

        // Consume the semicolon at the end of the statement
        // TODO: Optional semicolon?
        if self.peek_token_is(&Token::Semicolon) {
            self.next_token()?;
        }

        Ok(Statement::Return { value })
    }

    fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        // Consume the semicolon at the end of the statement
        // TODO: Optional semicolon?
        if self.peek_token_is(&Token::Semicolon) {
            self.next_token()?;
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
                self.next_token()?;
                // Parse the infix expression
                left_expr = infix_fn(self, left_expr)?;
            }
            // If no infix function is found for the peek token, then this is the end of the expression
        }

        Ok(left_expr)
    }

    /// Get the matching prefix parsing function for the given token.
    fn get_prefix_fn(&self, token: &Token) -> Option<PrefixFn> {
        match token {
            Token::If => Some(Parser::parse_if_expression),
            Token::Fn => Some(Parser::parse_function_literal),

            Token::Identifier(_) => Some(Parser::parse_identifier_expression),
            Token::Integer(_) => Some(Parser::parse_integer_expression),
            Token::Float(_) => Some(Parser::parse_float_expression),
            Token::True | Token::False => Some(Parser::parse_boolean_expression),
            Token::Nil => Some(Parser::parse_nil_expression),

            Token::LeftParen => Some(Parser::parse_grouped_expression),
            Token::LeftBrace => Some(Parser::parse_block_expression),

            Token::Bang | Token::Minus => Some(Parser::parse_prefix_expression),
            _ => None,
        }
    }

    /// Get the matching infix parsing function for the given token.
    fn get_infix_fn(&self, token: &Token) -> Option<InfixFn> {
        match token {
            Token::LeftParen => Some(Parser::parse_call_expression),

            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Star
            | Token::EqualEqual
            | Token::BangEqual
            | Token::LessThan
            // TODO: Implement Exponent (Token::StarStar)
            | Token::GreaterThan => Some(Parser::parse_infix_expression),
            _ => None,
        }
    }

    fn parse_if_expression(parser: &mut Parser<'_>) -> ParseResult<Expression> {
        // Advance past the `if` token (was already current_token, consume it out)
        parser.next_token()?;

        // Parse the condition (note: parentheses are not required)
        let condition = parser.parse_expression(Precedence::Lowest)?;

        // Expect opening brace of consequence block
        parser.expect_peek(Token::LeftBrace)?;

        let consequence = parser.parse_block_expression_as_block()?;

        let alternative = if parser.peek_token_is(&Token::Else) {
            // Consume the `else` token (was peek_token, advance into current_token)
            parser.next_token()?;

            // Allow `else if` by branching separately based on Token::LeftBrace vs Token::If
            if parser.peek_token_is(&Token::If) {
                // Consume `if` token (was peek_token, advance into current_token)
                parser.next_token()?;
                let else_if_expr = Parser::parse_if_expression(parser)?;

                Some(else_if_expr)
            } else {
                // Expect opening brace of alternative block
                parser.expect_peek(Token::LeftBrace)?;

                let else_block = Parser::parse_block_expression(parser)?;
                Some(else_block)
            }
        } else {
            None
        };

        Ok(Expression::If(Box::new(IfExpression {
            condition,
            consequence,
            alternative,
        })))
    }

    /// Parse a block expresssion as a BlockExpression rather than a generic Expression
    fn parse_block_expression_as_block(&mut self) -> ParseResult<BlockExpression> {
        let mut statements = Vec::new();

        // Consume the left brace
        self.next_token()?;

        while !self.current_token_is(Token::RightBrace) && !self.current_token_is(Token::Eof) {
            // TODO: Is throwing on Err the right approach?
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token()?;
        }

        // Expect a closing brace for the block (if we got EOF, this should result in a parse error)
        if !self.current_token_is(Token::RightBrace) {
            return Err(ParseError::Expected(
                "closing brace".to_string(),
                self.current_token.clone(),
            ));
        }

        Ok(BlockExpression { statements })
    }

    /// Parse a block expression directly as a generic Expression
    fn parse_block_expression(parser: &mut Parser<'_>) -> ParseResult<Expression> {
        let block = parser.parse_block_expression_as_block()?;
        Ok(Expression::Block(Box::new(block)))
    }

    fn parse_function_literal(parser: &mut Parser<'_>) -> ParseResult<Expression> {
        parser.expect_peek(Token::LeftParen)?;

        let parameters = parser.parse_function_parameters()?;

        parser.expect_peek(Token::LeftBrace)?;

        let body = parser.parse_block_expression_as_block()?;

        Ok(Expression::Function(Box::new(FunctionLiteral {
            parameters,
            body: Rc::new(body),
        })))
    }

    fn parse_function_parameters(&mut self) -> ParseResult<Vec<IdentifierLiteral>> {
        let mut identifiers = Vec::new();

        // No parameters, parentheses close immediately
        if self.peek_token_is(&Token::RightParen) {
            // Consume the left parenthesis
            self.next_token()?;
            return Ok(identifiers);
        }

        // Consume the left parenthesis
        self.next_token()?;

        // Push the first parameter identifier
        identifiers.push(self.parse_identifier_as_literal()?);

        while self.peek_token_is(&Token::Comma) {
            // Consume previous identifier
            self.next_token()?;
            // Consume comma
            self.next_token()?;
            // Push the next parameter identifier
            identifiers.push(self.parse_identifier_as_literal()?);
        }

        self.expect_peek(Token::RightParen)?;

        Ok(identifiers)
    }

    fn parse_identifier_as_literal(&mut self) -> ParseResult<IdentifierLiteral> {
        if let Token::Identifier(ref name) = self.current_token {
            Ok(IdentifierLiteral::from(name.clone()))
        } else {
            Err(ParseError::Expected(
                "identifier".to_string(),
                self.current_token.clone(),
            ))
        }
    }

    fn parse_identifier_expression(parser: &mut Parser<'_>) -> ParseResult<Expression> {
        let identifier_literal = parser.parse_identifier_as_literal()?;
        Ok(Expression::Identifier(identifier_literal))
    }

    fn parse_integer_expression(parser: &mut Parser<'_>) -> ParseResult<Expression> {
        if let Token::Integer(value) = parser.current_token {
            Ok(Expression::Integer(value))
        } else {
            Err(ParseError::Expected(
                "integer".to_string(),
                parser.current_token.clone(),
            ))
        }
    }

    fn parse_float_expression(parser: &mut Parser<'_>) -> ParseResult<Expression> {
        if let Token::Float(value) = parser.current_token {
            Ok(Expression::Float(value))
        } else {
            Err(ParseError::Expected(
                "float".to_string(),
                parser.current_token.clone(),
            ))
        }
    }

    fn parse_boolean_expression(parser: &mut Parser<'_>) -> ParseResult<Expression> {
        match parser.current_token {
            Token::True => Ok(Expression::Boolean(true)),
            Token::False => Ok(Expression::Boolean(false)),
            _ => Err(ParseError::Expected(
                "boolean".to_string(),
                parser.current_token.clone(),
            )),
        }
    }

    fn parse_nil_expression(parser: &mut Parser<'_>) -> ParseResult<Expression> {
        match parser.current_token {
            Token::Nil => Ok(Expression::Nil),
            _ => Err(ParseError::Expected(
                "nil".to_string(),
                parser.current_token.clone(),
            )),
        }
    }

    fn parse_grouped_expression(parser: &mut Parser<'_>) -> ParseResult<Expression> {
        // Consume left parenthesis
        parser.next_token()?;

        // Parse the inside expression
        let exp = parser.parse_expression(Precedence::Lowest);

        // Expect a right (closing) parenthesis
        parser.expect_peek(Token::RightParen)?;

        exp
    }

    fn parse_prefix_expression(parser: &mut Parser<'_>) -> ParseResult<Expression> {
        let operator = parser.current_token.clone();
        // Consume the operator token
        parser.next_token()?;
        let right = parser.parse_expression(Precedence::Prefix)?;
        Ok(Expression::Prefix(Box::new(PrefixExpression {
            operator,
            right,
        })))
    }

    fn parse_call_expression(parser: &mut Parser<'_>, left: Expression) -> ParseResult<Expression> {
        let arguments = parser.parse_call_arguments()?;
        Ok(Expression::Call(Box::new(CallExpression {
            function: left,
            arguments,
        })))
    }

    fn parse_call_arguments(&mut self) -> ParseResult<Vec<Expression>> {
        let mut arguments = Vec::new();

        // No parameters, parentheses close immediately
        if self.peek_token_is(&Token::RightParen) {
            // Consume the left parenthesis
            self.next_token()?;
            return Ok(arguments);
        }

        // Consume the left parenthesis
        self.next_token()?;
        // Push the first parameter identifier
        arguments.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(&Token::Comma) {
            // Consume previous identifier
            self.next_token()?;
            // Consume comma
            self.next_token()?;
            // Push the next parameter identifier
            arguments.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(Token::RightParen)?;

        Ok(arguments)
    }

    fn parse_infix_expression(
        parser: &mut Parser<'_>,
        left: Expression,
    ) -> ParseResult<Expression> {
        let operator = parser.current_token.clone();
        let precedence = parser.current_precedence();
        parser.next_token()?;

        let right = parser.parse_expression(precedence)?;

        Ok(Expression::Infix(Box::new(InfixExpression {
            left,
            operator,
            right,
        })))
    }

    fn next_token(&mut self) -> Result<(), ParseError> {
        self.current_token = self.peek_token.clone();
        // TODO: Better lexing error handling when consuming in parser (ParserError::LexerError(err))
        match self.lexer.next_token() {
            Ok(tok) => {
                self.peek_token = tok;
                Ok(())
            }
            Err(err) => Err(err.to_parse_error()),
        }
    }

    fn current_token_is(&self, token: Token) -> bool {
        match (&token, &self.current_token) {
            (Token::Identifier(_), Token::Identifier(_)) => true,
            (Token::Integer(_), Token::Integer(_)) => true,
            _ => token == self.current_token,
        }
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        match (&token, &self.peek_token) {
            (Token::Identifier(_), Token::Identifier(_)) => true,
            (Token::Integer(_), Token::Integer(_)) => true,
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
            self.next_token()?;
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

        self.next_token()?;
        Ok(name)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, IdentifierLiteral, Program, Statement};
    use crate::lexer::Lexer;
    use crate::parser::{ParseError, Parser};
    use crate::token::Token;

    #[test]
    fn let_statement() {
        let tests = vec![
            ("let x = 5;", "x", Expression::Integer(5)),
            ("let y = true;", "y", Expression::Boolean(true)),
            (
                "let foobar = y;",
                "foobar",
                Expression::Identifier(IdentifierLiteral::from("y")),
            ),
        ];

        for (input, expected_ident, expected_value) in tests {
            let prog = setup(input, 1);

            match &prog.statements[0] {
                Statement::Let { name, value } => {
                    assert_eq!(
                        expected_ident, name,
                        "expected identifier {} but got {}",
                        expected_ident, name
                    );

                    assert_eq!(
                        expected_value, *value,
                        "expected value {} but got {}",
                        expected_value, value
                    );
                }
                stmt => panic!("expected let statement but got {}", stmt),
            }
        }
    }

    #[test]
    fn return_statement() {
        let tests = vec![
            ("return 5;", Expression::Integer(5)),
            ("return true;", Expression::Boolean(true)),
            (
                "return y;",
                Expression::Identifier(IdentifierLiteral::from("y")),
            ),
        ];

        for (input, expected_value) in tests {
            let prog = setup(input, 1);

            match &prog.statements[0] {
                Statement::Return { value } => {
                    assert_eq!(
                        expected_value, *value,
                        "expected value {} but got {}",
                        expected_value, value
                    );
                }
                stmt => panic!("expected let statement but got {}", stmt),
            }
        }
    }

    #[test]
    fn identifier_expression() {
        let input = "foobar;";

        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog);

        test_identifier(expr, "foobar");
    }

    #[test]
    fn integer_expression() {
        let input = "5;";

        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog);

        test_integer_literal(expr, 5);
    }

    #[test]
    fn float_expression() {
        let input = "5.5;";

        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog);

        test_float_literal(expr, 5.5);
    }

    #[test]
    fn boolean_expression() {
        // Tests: (input, value)
        let tests = vec![("true;", true), ("false;", false)];

        for (input, value) in tests {
            let prog = setup(input, 1);
            let expr = unwrap_expression(&prog);

            test_boolean_literal(expr, value);
        }
    }

    #[test]
    fn nil_expression() {
        let input = "nil;";

        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog);

        assert_eq!(
            Expression::Nil,
            *expr,
            "expected nil value but got {}",
            expr
        );
    }

    #[test]
    fn prefix_integer_expression() {
        // Tests: (input, operator, value)
        let tests: Vec<(&str, Token, i64)> =
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
                    test_integer_literal(&expr.right, right);
                }
                expr => panic!("expected prefix expression but got {}", expr),
            }
        }
    }

    #[test]
    fn prefix_float_expression() {
        // Tests: (input, operator, value)
        let tests: Vec<(&str, Token, f64)> =
            vec![("!5.5;", Token::Bang, 5.5), ("-15.5", Token::Minus, 15.5)];

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
                    test_float_literal(&expr.right, right);
                }
                expr => panic!("expected prefix expression but got {}", expr),
            }
        }
    }

    #[test]
    fn prefix_boolean_expression() {
        // Tests: (input, operator, value)
        let tests: Vec<(&str, Token, bool)> = vec![
            ("!true;", Token::Bang, true),
            ("!false", Token::Bang, false),
        ];

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
                    test_boolean_literal(&expr.right, right);
                }
                expr => panic!("expected prefix expression but got {}", expr),
            }
        }
    }

    #[test]
    fn infix_integer_expression() {
        // Tests: (input, left_value, operator, right_value)
        let tests: Vec<(&str, i64, Token, i64)> = vec![
            ("2 + 5;", 2, Token::Plus, 5),
            ("2 - 5;", 2, Token::Minus, 5),
            ("2 * 5;", 2, Token::Star, 5),
            ("2 / 5;", 2, Token::Slash, 5),
            // ("2 ** 5;", 2, Token::StarStar, 5),
            ("2 > 5;", 2, Token::GreaterThan, 5),
            ("2 < 5;", 2, Token::LessThan, 5),
            ("2 == 5;", 2, Token::EqualEqual, 5),
            ("2 != 5;", 2, Token::BangEqual, 5),
        ];

        for (input, left, op, right) in tests {
            let prog = setup(input, 1);

            let expr = unwrap_expression(&prog);

            match expr {
                Expression::Infix(expr) => {
                    test_integer_literal(&expr.left, left);
                    assert_eq!(
                        op, expr.operator,
                        "expected operator {} but got {}",
                        op, expr.operator,
                    );
                    test_integer_literal(&expr.right, right);
                }
                expr => panic!("expected prefix expression but got {}", expr),
            }
        }
    }

    #[test]
    fn infix_float_expression() {
        // Tests: (input, left_value, operator, right_value)
        let tests: Vec<(&str, f64, Token, f64)> = vec![
            ("2.5 + 5.5;", 2.5, Token::Plus, 5.5),
            ("2.5 - 5.5;", 2.5, Token::Minus, 5.5),
            ("2.5 * 5.5;", 2.5, Token::Star, 5.5),
            ("2.5 / 5.5;", 2.5, Token::Slash, 5.5),
            // ("2.5 ** 5.5;", 2.5, Token::StarStar, 5.5),
            ("2.5 > 5.5;", 2.5, Token::GreaterThan, 5.5),
            ("2.5 < 5.5;", 2.5, Token::LessThan, 5.5),
            ("2.5 == 5.5;", 2.5, Token::EqualEqual, 5.5),
            ("2.5 != 5.5;", 2.5, Token::BangEqual, 5.5),
        ];

        for (input, left, op, right) in tests {
            let prog = setup(input, 1);

            let expr = unwrap_expression(&prog);

            match expr {
                Expression::Infix(expr) => {
                    test_float_literal(&expr.left, left);
                    assert_eq!(
                        op, expr.operator,
                        "expected operator {} but got {}",
                        op, expr.operator,
                    );
                    test_float_literal(&expr.right, right);
                }
                expr => panic!("expected prefix expression but got {}", expr),
            }
        }
    }

    #[test]
    fn infix_boolean_expression() {
        // Tests: (input, left_value, operator, right_value)
        let tests: Vec<(&str, bool, Token, bool)> = vec![
            ("true == true", true, Token::EqualEqual, true),
            ("true != false", true, Token::BangEqual, false),
            ("false == false", false, Token::EqualEqual, false),
        ];

        for (input, left, op, right) in tests {
            let prog = setup(input, 1);

            let expr = unwrap_expression(&prog);

            match expr {
                Expression::Infix(expr) => {
                    test_boolean_literal(&expr.left, left);
                    assert_eq!(
                        op, expr.operator,
                        "expected operator {} but got {}",
                        op, expr.operator,
                    );
                    test_boolean_literal(&expr.right, right);
                }
                expr => panic!("expected prefix expression but got {}", expr),
            }
        }
    }

    #[test]
    fn operator_precedence() {
        // Tests: (input, expected)
        #[rustfmt::skip]
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            // ("a + b / c ** d", "(a + (b / (c ** d)))"),
            // ("-b ** c", "((-b) ** c)"),
            // ("a ** b ** c", "(a ** (b ** c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4); ((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
            ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"),
            /*
            ("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)"),
            ("add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"),
            */
        ];

        for (input, expected) in tests {
            let prog = setup(input, 0).to_string();
            assert_eq!(expected, prog, "expected '{}' but got '{}'", expected, prog)
        }
    }

    #[test]
    fn block_expression() {
        let input = "{ 5 }";
        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog);

        match expr {
            Expression::Block(block) => {
                assert_eq!(block.statements.len(), 1);
                match block.statements.first().unwrap() {
                    Statement::Expression { expression } => test_integer_literal(expression, 5),
                    stmt => panic!("expected expression statement but got {:?}", stmt),
                }
            }
            expr => panic!("expected block expression but got {:?}", expr),
        }
    }

    #[test]
    fn block_eof_handling() {
        let input = "{ 5 ";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let prog = p.parse_program();

        match prog {
            Err(errs) => {
                assert_eq!(errs.len(), 1);
                match errs.first().unwrap() {
                    ParseError::Expected(expected, got) => {
                        assert_eq!(expected, "closing brace");
                        assert_eq!(*got, Token::Eof);
                    }
                    err => panic!(
                        "expected \"expected closing brace\" parser error but got {:?}",
                        err
                    ),
                }
            }
            Ok(prog) => panic!("expected invalid block to error but got {}", prog),
        }
    }

    #[test]
    fn if_expression() {
        let input = "if x < y { x }";

        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog);

        match expr {
            Expression::If(if_expr) => {
                test_if_condition(&if_expr.condition, "x", Token::LessThan, "y");

                assert_eq!(if_expr.consequence.statements.len(), 1);

                match &if_expr.consequence.statements.first().unwrap() {
                    Statement::Expression { expression } => test_identifier(expression, "x"),
                    stmt => panic!("expected expression statement but got {:?}", stmt),
                }

                assert_eq!(if_expr.alternative, None);
            }
            expr => panic!("expected if expression but got {}", expr),
        }
    }

    #[test]
    fn if_else_expression() {
        let input = "if x < y { x } else { y }";

        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog);

        match expr {
            Expression::If(if_expr) => {
                test_if_condition(&if_expr.condition, "x", Token::LessThan, "y");

                assert_eq!(if_expr.consequence.statements.len(), 1);

                match if_expr.consequence.statements.first().unwrap() {
                    Statement::Expression { expression } => test_identifier(expression, "x"),
                    stmt => panic!("expected expression statement (in if) but got {:?}", stmt),
                }

                if let Some(Expression::Block(alternative)) = &if_expr.alternative {
                    assert_eq!(alternative.statements.len(), 1);

                    match alternative.statements.first().unwrap() {
                        Statement::Expression { expression } => test_identifier(expression, "y"),
                        stmt => {
                            panic!("expected expression statement (in else) but got {:?}", stmt)
                        }
                    }
                } else {
                    panic!("expected alternative (else) block")
                }
            }
            expr => panic!("expected if expression but got {}", expr),
        }
    }

    #[test]
    fn if_elseif_else_expression() {
        let input = "if x < y { x } else if x > y { y } else { z }";

        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog);

        match expr {
            Expression::If(if_expr) => {
                test_if_condition(&if_expr.condition, "x", Token::LessThan, "y");

                assert_eq!(if_expr.consequence.statements.len(), 1);

                match if_expr.consequence.statements.first().unwrap() {
                    Statement::Expression { expression } => test_identifier(expression, "x"),
                    stmt => panic!("expected expression statement (in if) but got {:?}", stmt),
                }

                if let Some(Expression::If(elseif)) = &if_expr.alternative {
                    test_if_condition(&elseif.condition, "x", Token::GreaterThan, "y");

                    assert_eq!(elseif.consequence.statements.len(), 1);

                    match elseif.consequence.statements.first().unwrap() {
                        Statement::Expression { expression } => test_identifier(expression, "y"),
                        stmt => panic!(
                            "expected expression statement (in else if) but got {:?}",
                            stmt
                        ),
                    }

                    if let Some(Expression::Block(alternative)) = &elseif.alternative {
                        assert_eq!(alternative.statements.len(), 1);

                        match alternative.statements.first().unwrap() {
                            Statement::Expression { expression } => {
                                test_identifier(expression, "z")
                            }
                            stmt => {
                                panic!("expected expression statement (in else) but got {:?}", stmt)
                            }
                        }
                    } else {
                        panic!("expected alternative (else) block")
                    }
                } else {
                    panic!("expected alternative (else if) expression")
                }
            }
            expr => panic!("expected if expression but got {}", expr),
        }
    }

    #[test]
    fn function_literal() {
        let input = "fn(x, y) { x + y; }";

        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog);

        match expr {
            Expression::Function(func) => {
                assert_eq!(
                    func.parameters.len(),
                    2,
                    "expected 2 parameters but got {:?}",
                    func.parameters
                );
                test_identifier_literal(&func.parameters[0], "x");
                test_identifier_literal(&func.parameters[1], "y");
                assert_eq!(
                    func.body.statements.len(),
                    1,
                    "expected 1 body statement but got {:?}",
                    func.body
                );

                match func.body.statements.first().unwrap() {
                    Statement::Expression { expression } => match expression {
                        Expression::Infix(infix) => {
                            assert_eq!(
                                infix.operator,
                                Token::Plus,
                                "expected + but got {}",
                                infix.operator
                            );
                            test_identifier(&infix.left, "x");
                            test_identifier(&infix.right, "y");
                        }
                        stmt => panic!("expected infix expression but got {:?}", stmt),
                    },
                    stmt => panic!("expected expression statement but got {:?}", stmt),
                }
            }
            expr => panic!("expected function literal expression but got {}", expr),
        }
    }

    #[test]
    fn function_parameters() {
        // Tests: (input, expected parameters)
        #[rustfmt::skip]
        let tests = vec![
            ("fn() {}", vec![]),
            ("fn(x) {}", vec!["x"]),
            ("fn(x, y, z) {}", vec!["x", "y", "z"])
        ];

        for (input, expected) in tests {
            let prog = setup(input, 0);
            let expr = unwrap_expression(&prog);

            match expr {
                Expression::Function(func) => {
                    assert_eq!(
                        expected.len(),
                        func.parameters.len(),
                        "expected {} parameters but got {:?}",
                        expected.len(),
                        func.parameters
                    );

                    for (ident, &expected_value) in func.parameters.iter().zip(expected.iter()) {
                        test_identifier_literal(ident, expected_value);
                    }
                }
                expr => panic!("expected function literal expression but got {}", expr),
            }
        }
    }

    #[test]
    fn call_expression() {
        let input = "add(1, 2 * 3)";

        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog);

        match expr {
            Expression::Call(call) => {
                test_identifier(&call.function, "add");
                assert_eq!(
                    call.arguments.len(),
                    2,
                    "expected 2 call arguments but got {:?}",
                    call.arguments
                );

                test_integer_literal(&call.arguments[0], 1);

                match &call.arguments[1] {
                    Expression::Infix(expr) => {
                        test_integer_literal(&expr.left, 2);
                        assert_eq!(
                            Token::Star,
                            expr.operator,
                            "expected operator * but got {}",
                            expr.operator,
                        );
                        test_integer_literal(&expr.right, 3);
                    }
                    expr => panic!(
                        "expected prefix expression for second argument but got {}",
                        expr
                    ),
                }
            }
            expr => panic!("expected call expression but got {}", expr),
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

    fn unwrap_expression(prog: &Program) -> &Expression {
        match prog.statements.first().unwrap() {
            Statement::Expression { expression } => &expression,
            stmt => panic!("{:?} isn't an expression statement", stmt),
        }
    }

    fn test_identifier(expr: &Expression, expected_value: &str) {
        match expr {
            Expression::Identifier(ident) => {
                test_identifier_literal(ident, expected_value);
            }
            _ => panic!("expected identifier {} but got {}", expected_value, expr),
        }
    }

    fn test_identifier_literal(ident: &IdentifierLiteral, expected_value: &str) {
        assert_eq!(
            expected_value, ident.name,
            "expected identifier with name {} but got {}",
            expected_value, ident.name
        );
    }

    fn test_integer_literal(expr: &Expression, expected_value: i64) {
        match expr {
            Expression::Integer(num) => {
                assert_eq!(
                    expected_value, *num,
                    "expected {} but got {}",
                    expected_value, num
                )
            }
            _ => panic!(
                "expected integer literal {} but got {}",
                expected_value, expr
            ),
        }
    }

    fn test_float_literal(expr: &Expression, expected_value: f64) {
        match expr {
            Expression::Float(num) => {
                assert_eq!(
                    expected_value, *num,
                    "expected {} but got {}",
                    expected_value, num
                )
            }
            _ => panic!("expected float literal {} but got {}", expected_value, expr),
        }
    }

    fn test_boolean_literal(expr: &Expression, expected_value: bool) {
        match expr {
            Expression::Boolean(num) => {
                assert_eq!(
                    expected_value, *num,
                    "expected {} but got {}",
                    expected_value, num
                )
            }
            _ => panic!(
                "expected boolean literal {} but got {}",
                expected_value, expr
            ),
        }
    }

    fn test_if_condition(
        expr: &Expression,
        expected_left_ident: &str,
        expected_operator: Token,
        expected_right_ident: &str,
    ) {
        match expr {
            Expression::Infix(infix) => {
                test_identifier(&infix.left, expected_left_ident);

                if infix.operator != expected_operator {
                    panic!(
                        "expected {} operator but got {}",
                        expected_operator, infix.operator
                    );
                }

                test_identifier(&infix.right, expected_right_ident);
            }
            expr => panic!("expected infix expression (condition) but got {}", expr),
        }
    }
}
