use std::fmt::Display;

use crate::ast::{
    BlockStatement, CallExpression, Expression, FunctionLiteral, IdentifierLiteral, IfExpression,
    InfixExpression, PrefixExpression, Program, Statement,
};
use crate::position::{Span, WithSpan};
use crate::{lexer::Lexer, token::Token};

#[derive(Debug)]
pub enum ParseError {
    Unexpected(WithSpan<Token>),
    // TODO: Might want to use TokenKind to allow for Expected(TokenKind, Token)
    Expected(String, WithSpan<Token>),

    InvalidPrefixFn(WithSpan<Token>),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Unexpected(token) => {
                write!(f, "Unexpected token {} at {}", token.value, token.span)
            }
            ParseError::Expected(expected, got) => {
                write!(
                    f,
                    "Expected next token to be {}, but got {} instead at {}",
                    expected, got.value, got.span
                )
            }
            ParseError::InvalidPrefixFn(token) => {
                write!(
                    f,
                    "No prefix parsing function found for token {} at {}",
                    token.value, token.span
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

        let total_span = Span::union_span(left_span, self.current_token.span);

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

        let total_span = Span::union_span(left_span, self.current_token.span);

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

        let total_span = Span::union_span(left_span, self.current_token.span);

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

        let total_span = Span::union_span(left_span, parser.current_token.span);

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
        let total_span = Span::union_span(left_span, self.current_token.span);

        // TODO: Maybe make this part of the while loop to avoid looping twice

        return Ok(WithSpan::new(BlockStatement { statements }, total_span));
    }

    fn parse_function_literal(parser: &mut Parser<'_>) -> ParseResult<WithSpan<Expression>> {
        let left_span = parser.current_token.span;
        parser.expect_peek(Token::LeftParen)?;

        let parameters = parser.parse_function_parameters()?;

        parser.expect_peek(Token::LeftBrace)?;

        let body = parser.parse_block_statement()?;

        let total_span = Span::union_span(left_span, parser.current_token.span);

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

        let total_span = Span::union_span(left_span, parser.current_token.span);

        return Ok(WithSpan::new(exp.value, total_span));
    }

    fn parse_prefix_expression(parser: &mut Parser<'_>) -> ParseResult<WithSpan<Expression>> {
        let left_span = parser.current_token.span;
        let operator = parser.current_token.clone();
        // Consume the operator token
        parser.next_token();
        let right = parser.parse_expression(Precedence::Prefix)?;

        let total_span = Span::union_span(left_span, right.span);

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
        let total_span = Span::union_span(left.span, parser.current_token.span);
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

        let total_span = Span::union_span(left.span, parser.current_token.span);

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
    // TODO: Verify spans for each test (instead of just value)
    use crate::ast::{Expression, IdentifierLiteral, Program, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::token::Token;

    #[test]
    fn let_statement() {
        let tests = vec![
            ("let x = 5;", "x", Expression::Number(5)),
            ("let y = true;", "y", Expression::Boolean(true)),
            (
                "let foobar = y;",
                "foobar",
                Expression::Identifier(IdentifierLiteral::from("y")),
            ),
        ];

        for (input, expected_ident, expected_value) in tests {
            let prog = setup(input, 1);

            match &prog.statements[0].value {
                Statement::Let { name: ident, value } => {
                    assert_eq!(
                        expected_ident, ident.value.name,
                        "expected identifier {} but got {}",
                        expected_ident, ident.value
                    );

                    assert_eq!(
                        expected_value, value.value,
                        "expected value {} but got {}",
                        expected_value, value.value
                    );
                }
                stmt => panic!("expected let statement but got {}", stmt),
            }
        }
    }

    #[test]
    fn return_statement() {
        let tests = vec![
            ("return 5;", Expression::Number(5)),
            ("return true;", Expression::Boolean(true)),
            (
                "return y;",
                Expression::Identifier(IdentifierLiteral::from("y")),
            ),
        ];

        for (input, expected_value) in tests {
            let prog = setup(input, 1);

            match &prog.statements[0].value {
                Statement::Return { value } => {
                    assert_eq!(
                        expected_value, value.value,
                        "expected value {} but got {}",
                        expected_value, value.value
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
    fn number_expression() {
        let input = "5;";

        let prog = setup(input, 1);
        let expr = unwrap_expression(&prog);

        test_number_literal(expr, 5);
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
    fn prefix_number_expression() {
        // Tests: (input, operator, value)
        let tests: Vec<(&str, Token, u64)> =
            vec![("!5;", Token::Bang, 5), ("-15", Token::Minus, 15)];

        for (input, op, right) in tests {
            let prog = setup(input, 1);

            let expr = unwrap_expression(&prog);

            match expr {
                Expression::Prefix(expr) => {
                    assert_eq!(
                        op, expr.operator.value,
                        "expected operator {} but got {}",
                        op, expr.operator.value,
                    );
                    test_number_literal(&expr.right.value, right);
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
                        op, expr.operator.value,
                        "expected operator {} but got {}",
                        op, expr.operator.value,
                    );
                    test_boolean_literal(&expr.right.value, right);
                }
                expr => panic!("expected prefix expression but got {}", expr),
            }
        }
    }

    #[test]
    fn infix_number_expression() {
        // Tests: (input, left_value, operator, right_value)
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
                    test_number_literal(&expr.left.value, left);
                    assert_eq!(
                        op, expr.operator.value,
                        "expected operator {} but got {}",
                        op, expr.operator.value,
                    );
                    test_number_literal(&expr.right.value, right);
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
                    test_boolean_literal(&expr.left.value, left);
                    assert_eq!(
                        op, expr.operator.value,
                        "expected operator {} but got {}",
                        op, expr.operator.value,
                    );
                    test_boolean_literal(&expr.right.value, right);
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
        let expr = unwrap_expression(&prog);

        match expr {
            Expression::If(if_expr) => {
                test_if_condition(&if_expr.condition.value, "x", Token::LessThan, "y");

                assert_eq!(if_expr.consequence.value.statements.len(), 1);

                match &if_expr.consequence.value.statements.first().unwrap().value {
                    Statement::Expression { expression } => test_identifier(&expression.value, "x"),
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
                test_if_condition(&if_expr.condition.value, "x", Token::LessThan, "y");

                assert_eq!(if_expr.consequence.value.statements.len(), 1);

                match &if_expr.consequence.value.statements.first().unwrap().value {
                    Statement::Expression { expression } => test_identifier(&expression.value, "x"),
                    stmt => panic!("expected expression statement but got {:?}", stmt),
                }

                if let Some(alternative) = &if_expr.alternative {
                    assert_eq!(alternative.value.statements.len(), 1);

                    match &alternative.value.statements.first().unwrap().value {
                        Statement::Expression { expression } => {
                            test_identifier(&expression.value, "y")
                        }
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
        let expr = unwrap_expression(&prog);

        match expr {
            Expression::Function(func) => {
                assert_eq!(
                    func.parameters.len(),
                    2,
                    "expected 2 parameters but got {:?}",
                    func.parameters
                );
                test_identifier_literal(&func.parameters[0].value, "x");
                test_identifier_literal(&func.parameters[1].value, "y");
                assert_eq!(
                    func.body.value.statements.len(),
                    1,
                    "expected 1 body statement but got {:?}",
                    func.body
                );

                match &func.body.value.statements.first().unwrap().value {
                    Statement::Expression { expression } => match &expression.value {
                        Expression::Infix(infix) => {
                            assert_eq!(
                                infix.operator.value,
                                Token::Plus,
                                "expected + but got {}",
                                infix.operator.value
                            );
                            test_identifier(&infix.left.value, "x");
                            test_identifier(&infix.right.value, "y");
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
                        test_identifier_literal(&ident.value, expected_value);
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
                test_identifier(&call.function.value, "add");
                assert_eq!(
                    call.arguments.len(),
                    2,
                    "expected 2 call arguments but got {:?}",
                    call.arguments
                );

                test_number_literal(&call.arguments[0].value, 1);

                match &call.arguments[1].value {
                    Expression::Infix(expr) => {
                        test_number_literal(&expr.left.value, 2);
                        assert_eq!(
                            Token::Star,
                            expr.operator.value,
                            "expected operator * but got {}",
                            expr.operator.value,
                        );
                        test_number_literal(&expr.right.value, 3);
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
        match &prog.statements.first().unwrap().value {
            Statement::Expression { expression } => &expression.value,
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

    fn test_number_literal(expr: &Expression, expected_value: u64) {
        match expr {
            Expression::Number(num) => {
                assert_eq!(
                    expected_value, *num,
                    "expected {} but got {}",
                    expected_value, num
                )
            }
            _ => panic!(
                "expected number literal {} but got {}",
                expected_value, expr
            ),
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
                test_identifier(&infix.left.value, expected_left_ident);

                assert_eq!(
                    infix.operator.value, expected_operator,
                    "expected {} operator but got {}",
                    expected_operator, infix.operator.value
                );

                test_identifier(&infix.right.value, expected_right_ident);
            }
            expr => panic!("expected infix expression (condition) but got {}", expr),
        }
    }
}
