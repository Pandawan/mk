use std::fmt::Display;

use crate::ast::{
    BlockStatement, CallExpression, Expression, FunctionLiteral, IdentifierLiteral, IfExpression,
    InfixExpression, PrefixExpression, Program, Statement,
};
use crate::position::{Span, WithSpan};
use crate::{lexer::Lexer, token::Token};

#[derive(Debug)]
pub enum ParseError {
    // TODO: Might want to use TokenKind to allow for Expected(TokenKind, Token)
    Expected(String, WithSpan<Token>),

    InvalidPrefixFn(WithSpan<Token>),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Expected(expected, got) => {
                write!(
                    f,
                    "Expected next token to be {}, but got {} instead",
                    expected, got.value
                )
            }
            ParseError::InvalidPrefixFn(token) => {
                write!(
                    f,
                    "No prefix parsing function found for token {}",
                    token.value
                )
            }
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

type PrefixFn = fn(parser: &mut Parser<'_>) -> ParseResult<WithSpan<Expression>>;
type InfixFn =
    fn(parser: &mut Parser<'_>, left: WithSpan<Expression>) -> ParseResult<WithSpan<Expression>>;

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
            Token::LeftParen => Precedence::Call,
            // Token::LeftBrace => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,

    current_token: WithSpan<Token>,
    peek_token: WithSpan<Token>,
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

        while self.current_token.value != Token::Eof {
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

    fn parse_statement(&mut self) -> ParseResult<WithSpan<Statement>> {
        match self.current_token.value {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> ParseResult<WithSpan<Statement>> {
        let left_span = self.current_token.span;
        let name = self.expect_peek_identifier()?;

        self.expect_peek(Token::Equal)?;

        // Consume equal sign
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        // Consume the semicolon at the end of the statement
        // TODO: Optional semicolon?
        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        let total_span = Span::union(left_span, self.current_token.span);

        Ok(WithSpan::new(Statement::Let { name, value }, total_span))
    }

    fn parse_return_statement(&mut self) -> ParseResult<WithSpan<Statement>> {
        let left_span = self.current_token.span;
        // Consume the `return` token
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        // Consume the semicolon at the end of the statement
        // TODO: Optional semicolon?
        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        let total_span = Span::union(left_span, self.current_token.span);

        Ok(WithSpan::new(Statement::Return { value }, total_span))
    }

    fn parse_expression_statement(&mut self) -> ParseResult<WithSpan<Statement>> {
        let left_span = self.current_token.span;
        let expr = self.parse_expression(Precedence::Lowest)?;

        // Consume the semicolon at the end of the statement
        // TODO: Optional semicolon?
        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        let total_span = Span::union(left_span, self.current_token.span);

        Ok(WithSpan::new(
            Statement::Expression { expression: expr },
            total_span,
        ))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParseResult<WithSpan<Expression>> {
        let mut left_expr;

        // Parse the current token (either as a prefix or as a literal)
        if let Some(prefix_fn) = self.get_prefix_fn(&self.current_token.value) {
            left_expr = prefix_fn(self)?;
        } else {
            return Err(ParseError::InvalidPrefixFn(self.current_token.clone()));
        }

        // At this point the last token of the left_expr is still current_token
        // (if not below, the token will be consumed by next_token() in parse_program)

        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_precedence() {
            // Check if the peek token is an operator (and has an infix function)
            if let Some(infix_fn) = self.get_infix_fn(&self.peek_token.value) {
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
            Token::If => Some(Parser::parse_if_expression),
            Token::Fn => Some(Parser::parse_function_literal),

            Token::Identifier(_) => Some(Parser::parse_identifier_expression),
            Token::Number(_) => Some(Parser::parse_number_expression),
            Token::True | Token::False => Some(Parser::parse_boolean_expression),
            Token::Nil => Some(Parser::parse_nil_expression),

            Token::LeftParen => Some(Parser::parse_grouped_expression),

            Token::Bang | Token::Minus => Some(Parser::parse_prefix_expression),
            _ => None,
        }
    }

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
            | Token::GreaterThan => Some(Parser::parse_infix_expression),
            _ => None,
        }
    }

    fn parse_if_expression(parser: &mut Parser<'_>) -> ParseResult<WithSpan<Expression>> {
        let left_span = parser.current_token.span;
        // Consume the `if` token
        parser.next_token();

        // Parse the condition (note: parentheses are not required)
        let condition = parser.parse_expression(Precedence::Lowest)?;

        // Expect opening brace of consequence block
        parser.expect_peek(Token::LeftBrace)?;

        let consequence = parser.parse_block_statement()?;

        let alternative = if parser.peek_token_is(&Token::Else) {
            // Consume the `else` token
            parser.next_token();

            // TODO: Allow `else if` (potentially by branching separately based on Token::LeftBrace vs Token::If)

            // Expect opening brace of alternative block
            parser.expect_peek(Token::LeftBrace)?;

            Some(parser.parse_block_statement()?)
        } else {
            None
        };

        let total_span = Span::union(left_span, parser.current_token.span);

        Ok(WithSpan::new(
            Expression::If(Box::new(IfExpression {
                condition,
                consequence,
                alternative,
            })),
            total_span,
        ))
    }

    fn parse_block_statement(&mut self) -> ParseResult<WithSpan<BlockStatement>> {
        let mut statements = Vec::new();

        // Start on left brace
        let left_span = self.current_token.span;

        // Consume the left brace
        self.next_token();

        while !self.current_token_is(Token::RightBrace) && !self.current_token_is(Token::Eof) {
            // TODO: Is throwing on Err the right approach?
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }

        // Span ends on current token (right brace OR eof)
        let total_span = Span::union(left_span, self.current_token.span);

        return Ok(WithSpan::new(BlockStatement { statements }, total_span));
    }

    fn parse_function_literal(parser: &mut Parser<'_>) -> ParseResult<WithSpan<Expression>> {
        let left_span = parser.current_token.span;
        parser.expect_peek(Token::LeftParen)?;

        let parameters = parser.parse_function_parameters()?;

        parser.expect_peek(Token::LeftBrace)?;

        let body = parser.parse_block_statement()?;

        let total_span = Span::union(left_span, parser.current_token.span);

        return Ok(WithSpan::new(
            Expression::Function(Box::new(FunctionLiteral { parameters, body })),
            total_span,
        ));
    }

    fn parse_function_parameters(&mut self) -> ParseResult<Vec<WithSpan<IdentifierLiteral>>> {
        let mut identifiers = Vec::new();

        // No parameters, parentheses close immediately
        if self.peek_token_is(&Token::RightParen) {
            // Consume the left parenthesis
            self.next_token();
            return Ok(identifiers);
        }

        // Consume the left parenthesis
        self.next_token();

        // Push the first parameter identifier
        identifiers.push(self.parse_identifier_as_literal()?);

        while self.peek_token_is(&Token::Comma) {
            // Consume previous identifier
            self.next_token();
            // Consume comma
            self.next_token();
            // Push the next parameter identifier
            identifiers.push(self.parse_identifier_as_literal()?);
        }

        self.expect_peek(Token::RightParen)?;

        return Ok(identifiers);
    }

    fn parse_identifier_as_literal(&mut self) -> ParseResult<WithSpan<IdentifierLiteral>> {
        if let Token::Identifier(ref name) = self.current_token.value {
            Ok(WithSpan::new(
                IdentifierLiteral::from(name.clone()),
                self.current_token.span,
            ))
        } else {
            Err(ParseError::Expected(
                "identifier".to_string(),
                self.current_token.clone(),
            ))
        }
    }

    fn parse_identifier_expression(parser: &mut Parser<'_>) -> ParseResult<WithSpan<Expression>> {
        let identifier_literal = parser.parse_identifier_as_literal()?;
        return Ok(WithSpan::new(
            Expression::Identifier(identifier_literal.value),
            identifier_literal.span,
        ));
    }

    fn parse_number_expression(parser: &mut Parser<'_>) -> ParseResult<WithSpan<Expression>> {
        if let Token::Number(value) = parser.current_token.value {
            Ok(WithSpan::new(
                Expression::Number(value),
                parser.current_token.span,
            ))
        } else {
            Err(ParseError::Expected(
                "number".to_string(),
                parser.current_token.clone(),
            ))
        }
    }

    fn parse_boolean_expression(parser: &mut Parser<'_>) -> ParseResult<WithSpan<Expression>> {
        match parser.current_token.value {
            Token::True => Ok(WithSpan::new(
                Expression::Boolean(true),
                parser.current_token.span,
            )),
            Token::False => Ok(WithSpan::new(
                Expression::Boolean(false),
                parser.current_token.span,
            )),
            _ => Err(ParseError::Expected(
                "boolean".to_string(),
                parser.current_token.clone(),
            )),
        }
    }

    fn parse_nil_expression(parser: &mut Parser<'_>) -> ParseResult<WithSpan<Expression>> {
        match parser.current_token.value {
            Token::Nil => Ok(WithSpan::new(Expression::Nil, parser.current_token.span)),
            _ => Err(ParseError::Expected(
                "nil".to_string(),
                parser.current_token.clone(),
            )),
        }
    }

    fn parse_grouped_expression(parser: &mut Parser<'_>) -> ParseResult<WithSpan<Expression>> {
        let left_span = parser.current_token.span;
        // Consume left parenthesis
        parser.next_token();

        // Parse the inside expression
        let exp = parser.parse_expression(Precedence::Lowest)?;

        // Expect a right (closing) parenthesis
        parser.expect_peek(Token::RightParen)?;

        let total_span = Span::union(left_span, parser.current_token.span);

        return Ok(WithSpan::new(exp.value, total_span));
    }

    fn parse_prefix_expression(parser: &mut Parser<'_>) -> ParseResult<WithSpan<Expression>> {
        let left_span = parser.current_token.span;
        let operator = parser.current_token.clone();
        // Consume the operator token
        parser.next_token();
        let right = parser.parse_expression(Precedence::Prefix)?;

        let total_span = Span::union(left_span, right.span);

        Ok(WithSpan::new(
            Expression::Prefix(Box::new(PrefixExpression { operator, right })),
            total_span,
        ))
    }

    fn parse_call_expression(
        parser: &mut Parser<'_>,
        left: WithSpan<Expression>,
    ) -> ParseResult<WithSpan<Expression>> {
        let arguments = parser.parse_call_arguments()?;
        let total_span = Span::union(left.span, parser.current_token.span);
        Ok(WithSpan::new(
            Expression::Call(Box::new(CallExpression {
                function: left,
                arguments,
            })),
            total_span,
        ))
    }

    fn parse_call_arguments(&mut self) -> ParseResult<Vec<WithSpan<Expression>>> {
        let mut arguments = Vec::new();

        // No parameters, parentheses close immediately
        if self.peek_token_is(&Token::RightParen) {
            // Consume the left parenthesis
            self.next_token();
            return Ok(arguments);
        }

        // Consume the left parenthesis
        self.next_token();
        // Push the first parameter identifier
        arguments.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(&Token::Comma) {
            // Consume previous identifier
            self.next_token();
            // Consume comma
            self.next_token();
            // Push the next parameter identifier
            arguments.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(Token::RightParen)?;

        Ok(arguments)
    }

    fn parse_infix_expression(
        parser: &mut Parser<'_>,
        left: WithSpan<Expression>,
    ) -> ParseResult<WithSpan<Expression>> {
        let operator = parser.current_token.clone();
        let precedence = parser.current_precedence();
        parser.next_token();

        let right = parser.parse_expression(precedence)?;

        let total_span = Span::union(left.span, parser.current_token.span);

        Ok(WithSpan::new(
            Expression::Infix(Box::new(InfixExpression {
                left,
                operator,
                right,
            })),
            total_span,
        ))
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn current_token_is(&self, token: Token) -> bool {
        match (&token, &self.current_token.value) {
            (Token::Identifier(_), Token::Identifier(_)) => true,
            (Token::Number(_), Token::Number(_)) => true,
            _ => token == self.current_token.value,
        }
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        match (&token, &self.peek_token.value) {
            (Token::Identifier(_), Token::Identifier(_)) => true,
            (Token::Number(_), Token::Number(_)) => true,
            _ => token == &self.peek_token.value,
        }
    }

    fn current_precedence(&self) -> Precedence {
        Precedence::token_precedence(&self.current_token.value)
    }

    fn peek_precedence(&self) -> Precedence {
        Precedence::token_precedence(&self.peek_token.value)
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

    fn expect_peek_identifier(&mut self) -> ParseResult<WithSpan<IdentifierLiteral>> {
        let name = match &self.peek_token.value {
            Token::Identifier(name) => name.to_owned(),
            _ => {
                return Err(ParseError::Expected(
                    String::from("identifier"),
                    self.peek_token.clone(),
                ))
            }
        };

        self.next_token();
        Ok(WithSpan::new(
            IdentifierLiteral::from(name),
            self.current_token.span,
        ))
    }
}

#[cfg(test)]
mod tests {
    // TODO: Cleanup tests? Don't check for span everywhere, only those specific to the current test
    use crate::ast::{Expression, IdentifierLiteral, Program, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::position::{Span, WithSpan};
    use crate::token::Token;

    #[test]
    fn let_statement() {
        let tests = vec![
            (
                "let x = 5;",
                WithSpan::new(IdentifierLiteral::from("x"), Span::new_unchecked(4, 5)),
                WithSpan::new(Expression::Number(5), Span::new_unchecked(8, 9)),
            ),
            (
                "let y = true;",
                WithSpan::new(IdentifierLiteral::from("y"), Span::new_unchecked(4, 5)),
                WithSpan::new(Expression::Boolean(true), Span::new_unchecked(8, 12)),
            ),
            (
                "let foobar = y;",
                WithSpan::new(
                    IdentifierLiteral::from("foobar"),
                    Span::new_unchecked(4, 10),
                ),
                WithSpan::new(
                    Expression::Identifier(IdentifierLiteral::from("y")),
                    Span::new_unchecked(13, 14),
                ),
            ),
        ];

        for (input, expected_ident, expected_value) in tests {
            let prog = setup(input, 1);

            let expected_stmt_span = Span::new_unchecked(0, input.len());
            assert_eq!(
                expected_stmt_span, prog.statements[0].span,
                "expected statement with span {} but got {}",
                expected_stmt_span, &prog.statements[0].span
            );

            match &prog.statements[0].value {
                Statement::Let { name: ident, value } => {
                    assert_eq!(
                        expected_ident, *ident,
                        "expected identifier {} at {} but got {} at {}",
                        expected_ident.value, expected_ident.span, ident.value, ident.span
                    );

                    assert_eq!(
                        expected_value, *value,
                        "expected value {} at {} but got {} at {}",
                        expected_value.value, expected_value.span, value.value, value.span
                    );
                }
                stmt => panic!("expected let statement but got {}", stmt),
            }
        }
    }

    #[test]
    fn return_statement() {
        let tests = vec![
            (
                "return 5;",
                WithSpan::new(Expression::Number(5), Span::new_unchecked(7, 8)),
            ),
            (
                "return true;",
                WithSpan::new(Expression::Boolean(true), Span::new_unchecked(7, 11)),
            ),
            (
                "return y;",
                WithSpan::new(
                    Expression::Identifier(IdentifierLiteral::from("y")),
                    Span::new_unchecked(7, 8),
                ),
            ),
        ];

        for (input, expected_value) in tests {
            let prog = setup(input, 1);

            let expected_stmt_span = Span::new_unchecked(0, input.len());
            assert_eq!(
                expected_stmt_span, prog.statements[0].span,
                "expected statement with span {} but got {}",
                expected_stmt_span, &prog.statements[0].span
            );

            match &prog.statements[0].value {
                Statement::Return { value } => {
                    assert_eq!(
                        expected_value, *value,
                        "expected value {} at {} but got {} at {}",
                        expected_value.value, expected_value.span, value.value, value.span
                    );
                }
                stmt => panic!("expected let statement but got {}", stmt),
            }
        }
    }

    #[test]
    fn identifier_expression() {
        let input = "foobar";

        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog, input.len());

        test_identifier(expr, WithSpan::new("foobar", Span::new_unchecked(0, 6)));
    }

    #[test]
    fn number_expression() {
        let input = "5";

        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog, input.len());

        test_number_literal(expr, WithSpan::new(5, Span::new_unchecked(0, 1)));
    }

    #[test]
    fn boolean_expression() {
        // Tests: (input, value)
        let tests = vec![
            ("true", WithSpan::new(true, Span::new_unchecked(0, 4))),
            ("false", WithSpan::new(false, Span::new_unchecked(0, 5))),
        ];

        for (input, expected_value) in tests {
            let prog = setup(input, 1);
            let expr = unwrap_expression(&prog, input.len());

            test_boolean_literal(expr, expected_value);
        }
    }

    #[test]
    fn nil_expression() {
        let input = "nil";

        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog, input.len());

        let expected_value = WithSpan::new(Expression::Nil, Span::new_unchecked(0, 3));

        assert_eq!(
            expected_value, *expr,
            "expected {} value at {} but got {} at {}",
            expected_value.value, expected_value.span, expr.value, expr.span
        );
    }

    #[test]
    fn prefix_number_expression() {
        // Tests: (input, operator, value)
        let tests = vec![
            (
                "!5",
                WithSpan::new(Token::Bang, Span::new_unchecked(0, 1)),
                WithSpan::new(5, Span::new_unchecked(1, 2)),
            ),
            (
                "-15",
                WithSpan::new(Token::Minus, Span::new_unchecked(0, 1)),
                WithSpan::new(15, Span::new_unchecked(1, 3)),
            ),
        ];

        for (input, expected_op, expected_right) in tests {
            let prog = setup(input, 1);

            let expr = unwrap_expression(&prog, input.len());

            match &expr.value {
                Expression::Prefix(expr) => {
                    assert_eq!(
                        expected_op,
                        expr.operator,
                        "expected operator {} at {} but got {} at {}",
                        expected_op.value,
                        expected_op.span,
                        expr.operator.value,
                        expr.operator.span,
                    );
                    test_number_literal(&expr.right, expected_right);
                }
                expr => panic!("expected prefix expression but got {}", expr),
            }
        }
    }

    #[test]
    fn prefix_boolean_expression() {
        // Tests: (input, operator, value)
        let tests = vec![
            (
                "!true",
                WithSpan::new(Token::Bang, Span::new_unchecked(0, 1)),
                WithSpan::new(true, Span::new_unchecked(1, 5)),
            ),
            (
                "!false",
                WithSpan::new(Token::Bang, Span::new_unchecked(0, 1)),
                WithSpan::new(false, Span::new_unchecked(1, 6)),
            ),
        ];

        for (input, expected_op, expected_right) in tests {
            let prog = setup(input, 1);

            let expr = unwrap_expression(&prog, input.len());
            match &expr.value {
                Expression::Prefix(expr) => {
                    assert_eq!(
                        expected_op,
                        expr.operator,
                        "expected operator {} at {} but got {} at {}",
                        expected_op.value,
                        expected_op.span,
                        expr.operator.value,
                        expr.operator.span,
                    );
                    test_boolean_literal(&expr.right, expected_right);
                }
                expr => panic!("expected prefix expression but got {}", expr),
            }
        }
    }

    #[test]
    fn infix_number_expression() {
        // Tests: (input, left_value, operator, right_value)
        let tests = vec![
            (
                "5 + 5",
                WithSpan::new(5, Span::new_unchecked(0, 1)),
                WithSpan::new(Token::Plus, Span::new_unchecked(2, 3)),
                WithSpan::new(5, Span::new_unchecked(4, 5)),
            ),
            (
                "5 - 5",
                WithSpan::new(5, Span::new_unchecked(0, 1)),
                WithSpan::new(Token::Minus, Span::new_unchecked(2, 3)),
                WithSpan::new(5, Span::new_unchecked(4, 5)),
            ),
            (
                "5 * 5",
                WithSpan::new(5, Span::new_unchecked(0, 1)),
                WithSpan::new(Token::Star, Span::new_unchecked(2, 3)),
                WithSpan::new(5, Span::new_unchecked(4, 5)),
            ),
            (
                "5 / 5",
                WithSpan::new(5, Span::new_unchecked(0, 1)),
                WithSpan::new(Token::Slash, Span::new_unchecked(2, 3)),
                WithSpan::new(5, Span::new_unchecked(4, 5)),
            ),
            (
                "5 > 5",
                WithSpan::new(5, Span::new_unchecked(0, 1)),
                WithSpan::new(Token::GreaterThan, Span::new_unchecked(2, 3)),
                WithSpan::new(5, Span::new_unchecked(4, 5)),
            ),
            (
                "5 < 5",
                WithSpan::new(5, Span::new_unchecked(0, 1)),
                WithSpan::new(Token::LessThan, Span::new_unchecked(2, 3)),
                WithSpan::new(5, Span::new_unchecked(4, 5)),
            ),
            (
                "5 == 5",
                WithSpan::new(5, Span::new_unchecked(0, 1)),
                WithSpan::new(Token::EqualEqual, Span::new_unchecked(2, 4)),
                WithSpan::new(5, Span::new_unchecked(5, 6)),
            ),
            (
                "5 != 5",
                WithSpan::new(5, Span::new_unchecked(0, 1)),
                WithSpan::new(Token::BangEqual, Span::new_unchecked(2, 4)),
                WithSpan::new(5, Span::new_unchecked(5, 6)),
            ),
        ];

        for (input, expected_left, expected_op, expected_right) in tests {
            let prog = setup(input, 1);

            let expr = unwrap_expression(&prog, input.len());

            match &expr.value {
                Expression::Infix(expr) => {
                    test_number_literal(&expr.left, expected_left);
                    assert_eq!(
                        expected_op,
                        expr.operator,
                        "expected operator {} at {} but got {} at {}",
                        expected_op.value,
                        expected_op.span,
                        expr.operator.value,
                        expr.operator.span,
                    );
                    test_number_literal(&expr.right, expected_right);
                }
                expr => panic!("expected prefix expression but got {}", expr),
            }
        }
    }

    #[test]
    fn infix_boolean_expression() {
        // Tests: (input, left_value, operator, right_value)
        let tests = vec![
            (
                "true == true",
                WithSpan::new(true, Span::new_unchecked(0, 4)),
                WithSpan::new(Token::EqualEqual, Span::new_unchecked(5, 7)),
                WithSpan::new(true, Span::new_unchecked(8, 12)),
            ),
            (
                "true != false",
                WithSpan::new(true, Span::new_unchecked(0, 4)),
                WithSpan::new(Token::BangEqual, Span::new_unchecked(5, 7)),
                WithSpan::new(false, Span::new_unchecked(8, 13)),
            ),
        ];

        for (input, expected_left, expected_op, expected_right) in tests {
            let prog = setup(input, 1);

            let expr = unwrap_expression(&prog, input.len());

            match &expr.value {
                Expression::Infix(expr) => {
                    test_boolean_literal(&expr.left, expected_left);
                    assert_eq!(
                        expected_op,
                        expr.operator,
                        "expected operator {} at {} but got {} at {}",
                        expected_op.value,
                        expected_op.span,
                        expr.operator.value,
                        expr.operator.span,
                    );
                    test_boolean_literal(&expr.right, expected_right);
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
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
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
    fn if_expression() {
        let input = "if x < y { x }";

        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog, input.len());

        match &expr.value {
            Expression::If(if_expr) => {
                test_if_condition(
                    &if_expr.condition,
                    WithSpan::new("x", Span::new_unchecked(3, 4)),
                    WithSpan::new(Token::LessThan, Span::new_unchecked(5, 6)),
                    WithSpan::new("y", Span::new_unchecked(7, 8)),
                );

                let expected_consequence_span = Span::new_unchecked(9, 14);
                assert_eq!(
                    expected_consequence_span, if_expr.consequence.span,
                    "expected if consequence's span at {} but got {}",
                    expected_consequence_span, if_expr.consequence.span
                );

                assert_eq!(if_expr.consequence.value.statements.len(), 1);

                match &if_expr.consequence.value.statements.first().unwrap().value {
                    Statement::Expression { expression } => test_identifier(
                        &expression,
                        WithSpan::new("x", Span::new_unchecked(11, 12)),
                    ),
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
        let expr = unwrap_expression(&prog, input.len());

        match &expr.value {
            Expression::If(if_expr) => {
                // NOTE: Not testing consequence because already done by if_expression()

                if let Some(alternative) = &if_expr.alternative {
                    let expected_alternative_span = Span::new_unchecked(20, 25);
                    assert_eq!(
                        expected_alternative_span, alternative.span,
                        "expected if alternative's span at {} but got {}",
                        expected_alternative_span, alternative.span
                    );

                    assert_eq!(alternative.value.statements.len(), 1);

                    match &alternative.value.statements.first().unwrap().value {
                        Statement::Expression { expression } => test_identifier(
                            &expression,
                            WithSpan::new("y", Span::new_unchecked(22, 23)),
                        ),
                        stmt => panic!("expected expression statement but got {:?}", stmt),
                    }
                } else {
                    panic!("expected alternative (else) block")
                }
            }
            expr => panic!("expected if expression but got {}", expr),
        }
    }

    #[test]
    fn function_literal() {
        let input = "fn(x, y) { x + y; }";

        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog, input.len());

        match &expr.value {
            Expression::Function(func) => {
                assert_eq!(
                    func.parameters.len(),
                    2,
                    "expected 2 parameters but got {:?}",
                    func.parameters
                );
                test_identifier_literal(
                    &func.parameters[0],
                    WithSpan::new("x", Span::new_unchecked(3, 4)),
                );
                test_identifier_literal(
                    &func.parameters[1],
                    WithSpan::new("y", Span::new_unchecked(6, 7)),
                );

                let expected_body_span = Span::new_unchecked(9, 19);
                assert_eq!(
                    expected_body_span, func.body.span,
                    "expected body span at {} but got {}",
                    expected_body_span, func.body.span
                );

                assert_eq!(
                    func.body.value.statements.len(),
                    1,
                    "expected 1 body statement but got {:?}",
                    func.body
                );

                match &func.body.value.statements.first().unwrap().value {
                    Statement::Expression { expression } => match &expression.value {
                        Expression::Infix(infix) => {
                            test_identifier(
                                &infix.left,
                                WithSpan::new("x", Span::new_unchecked(11, 12)),
                            );
                            let expected_op =
                                WithSpan::new(Token::Plus, Span::new_unchecked(13, 14));
                            assert_eq!(
                                expected_op,
                                infix.operator,
                                "expected operator {} at {} but got {} at {}",
                                expected_op.value,
                                expected_op.span,
                                infix.operator.value,
                                infix.operator.span,
                            );
                            test_identifier(
                                &infix.right,
                                WithSpan::new("y", Span::new_unchecked(15, 16)),
                            );
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
            ("fn(x) {}", vec![
                WithSpan::new("x", Span::new_unchecked(3, 4))
                ]),
            ("fn(x, y, z) {}", vec![
                WithSpan::new("x", Span::new_unchecked(3, 4)), WithSpan::new("y", Span::new_unchecked(6, 7)), WithSpan::new("z", Span::new_unchecked(9, 10))])
        ];

        for (input, expected) in tests {
            let prog = setup(input, 0);
            let expr = unwrap_expression(&prog, input.len());

            match &expr.value {
                Expression::Function(func) => {
                    assert_eq!(
                        expected.len(),
                        func.parameters.len(),
                        "expected {} parameters but got {:?}",
                        expected.len(),
                        func.parameters
                    );

                    for (ident, expected_value) in func.parameters.iter().zip(expected.iter()) {
                        test_identifier_literal(&ident, expected_value.clone());
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
        let expr = unwrap_expression(&prog, input.len());

        match &expr.value {
            Expression::Call(call) => {
                test_identifier(
                    &call.function,
                    WithSpan::new("add", Span::new_unchecked(0, 3)),
                );
                assert_eq!(
                    call.arguments.len(),
                    2,
                    "expected 2 call arguments but got {:?}",
                    call.arguments
                );

                test_number_literal(
                    &call.arguments[0],
                    WithSpan::new(1, Span::new_unchecked(4, 5)),
                );

                match &call.arguments[1].value {
                    Expression::Infix(infix) => {
                        test_number_literal(
                            &infix.left,
                            WithSpan::new(2, Span::new_unchecked(7, 8)),
                        );

                        let expected_op = WithSpan::new(Token::Star, Span::new_unchecked(9, 10));
                        assert_eq!(
                            expected_op,
                            infix.operator,
                            "expected operator {} at {} but got {} at {}",
                            expected_op.value,
                            expected_op.span,
                            infix.operator.value,
                            infix.operator.span,
                        );

                        test_number_literal(
                            &infix.right,
                            WithSpan::new(3, Span::new_unchecked(11, 12)),
                        );
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

    fn unwrap_expression(prog: &Program, length: usize) -> &WithSpan<Expression> {
        let expr = match &prog.statements.first().unwrap().value {
            Statement::Expression { expression } => expression,
            stmt => panic!("{:?} isn't an expression statement", stmt),
        };

        let expected_expr_span = Span::new_unchecked(0, length);
        assert_eq!(
            expected_expr_span, expr.span,
            "expected expression with span {} but got {}",
            expected_expr_span, expr.span
        );

        expr
    }

    fn test_identifier(expr: &WithSpan<Expression>, expected_value: WithSpan<&str>) {
        match &expr.value {
            Expression::Identifier(ident) => {
                assert_eq!(
                    expected_value.value, ident.name,
                    "expected identifier with name {} but got {}",
                    expected_value.value, ident.name
                );

                assert_eq!(
                    expected_value.span, expr.span,
                    "expected identifier's span at {} but got {}",
                    expected_value.span, expr.span
                );
            }
            _ => panic!(
                "expected identifier {} but got {}",
                expected_value.value, expr.value
            ),
        }
    }

    fn test_identifier_literal(
        ident: &WithSpan<IdentifierLiteral>,
        expected_value: WithSpan<&str>,
    ) {
        assert_eq!(
            expected_value.value, ident.value.name,
            "expected identifier with name {} but got {}",
            expected_value.value, ident.value.name
        );

        assert_eq!(
            expected_value.span, ident.span,
            "expected identifier's span at {} but got {}",
            expected_value.span, ident.span
        );
    }

    fn test_number_literal(expr: &WithSpan<Expression>, expected_value: WithSpan<u64>) {
        match expr.value {
            Expression::Number(value) => {
                assert_eq!(
                    expected_value.value, value,
                    "expected number {} but got {}",
                    expected_value.value, value
                );

                assert_eq!(
                    expected_value.span, expr.span,
                    "expected number's span at {} but got {}",
                    expected_value.span, expr.span,
                );
            }
            _ => panic!(
                "expected number literal {} but got {}",
                expected_value.value, expr.value
            ),
        }
    }

    fn test_boolean_literal(expr: &WithSpan<Expression>, expected_value: WithSpan<bool>) {
        match expr.value {
            Expression::Boolean(value) => {
                assert_eq!(
                    expected_value.value, value,
                    "expected boolean {} but got {}",
                    expected_value.value, value
                );

                assert_eq!(
                    expected_value.span, expr.span,
                    "expected boolean's span at {} but got {}",
                    expected_value.span, expr.span,
                );
            }
            _ => panic!(
                "expected boolean literal {} but got {}",
                expected_value.value, expr.value
            ),
        }
    }

    fn test_if_condition(
        expr: &WithSpan<Expression>,
        expected_left_ident: WithSpan<&str>,
        expected_operator: WithSpan<Token>,
        expected_right_ident: WithSpan<&str>,
    ) {
        match &expr.value {
            Expression::Infix(infix) => {
                test_identifier(&infix.left, expected_left_ident);

                assert_eq!(
                    infix.operator,
                    expected_operator,
                    "expected {} operator at {} but got {} at {}",
                    expected_operator.value,
                    expected_operator.span,
                    infix.operator.value,
                    infix.operator.span
                );

                test_identifier(&infix.right, expected_right_ident);
            }
            expr => panic!("expected infix expression (condition) but got {}", expr),
        }
    }
}
