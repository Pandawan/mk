use std::iter::Peekable;
use std::str::Chars;

use crate::position::{BytePos, Span, WithSpan};
use crate::token::Token;

pub struct Lexer<'a> {
    current_position: BytePos,
    input_iter: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            current_position: BytePos::default(),
            input_iter: input.chars().peekable(),
        }
    }

    /// Consume the next character from the list.
    fn read_char(&mut self) -> Option<char> {
        let next = self.input_iter.next();
        if let Some(c) = next {
            self.current_position = self.current_position.shift(c);
        }
        next
    }

    /// Get the next character from the list without consuming it.
    fn peek_char(&mut self) -> Option<&char> {
        self.input_iter.peek()
    }

    /// Consume whitespace until a non-whitespace character is found.
    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peek_char() {
            if c.is_whitespace() {
                // Consume the whitespace
                self.read_char();
            } else {
                break;
            }
        }
    }

    /// Read the current and following characters as a number token.
    fn read_number(&mut self, first: char) -> Token {
        // Convert the first digit to a number
        let mut number: u64 = u64::from(
            first
                .to_digit(10)
                .expect("Only digit characters should be matched"),
        );
        while let Some(&ch) = self.peek_char() {
            if is_digit(ch) {
                number = number * 10
                    + u64::from(
                        self.read_char()
                            .unwrap()
                            .to_digit(10)
                            .expect("Only digit characters should be matched"),
                    );
            } else {
                break;
            }
        }
        return Token::Number(number);
    }

    fn read_identifier_or_keyword(&mut self, first: char) -> Token {
        let mut identifier = String::new();
        identifier.push(first);

        // Loop through all characters until no longer identifier character
        while let Some(&ch) = self.peek_char() {
            if is_identifier_char(ch) {
                identifier.push(ch);
                self.read_char();
            } else {
                break;
            }
        }

        if let Some(keyword_token) = Token::lookup_keyword(&identifier) {
            keyword_token
        } else {
            Token::Identifier(identifier)
        }
    }

    /// Read a new token from the characters list.
    pub fn next_token(&mut self) -> WithSpan<Token> {
        self.skip_whitespace();

        let initial_position = self.current_position;

        let tok = if let Some(c) = self.read_char() {
            match c {
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Star,
                '/' => Token::Slash,

                '=' => match self.peek_char() {
                    // Double equal
                    Some('=') => {
                        self.read_char();
                        Token::EqualEqual
                    }
                    _ => Token::Equal,
                },
                '!' => match self.peek_char() {
                    // Double equal
                    Some('=') => {
                        self.read_char();
                        Token::BangEqual
                    }
                    _ => Token::Bang,
                },
                '<' => match self.peek_char() {
                    // Double equal
                    Some('=') => {
                        self.read_char();
                        Token::LessEqual
                    }
                    _ => Token::LessThan,
                },
                '>' => match self.peek_char() {
                    // Double equal
                    Some('=') => {
                        self.read_char();
                        Token::GreaterEqual
                    }
                    _ => Token::GreaterThan,
                },

                ',' => Token::Comma,
                ';' => Token::Semicolon,
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,

                c if is_digit(c) => self.read_number(c),
                c if is_identifier_char(c) => self.read_identifier_or_keyword(c),

                _ => panic!("Unknown symbol {}", c), // TODO: Token::Illegal(c)? Some kind of error reporting
            }
        } else {
            Token::Eof
        };

        WithSpan::new(tok, Span::new(initial_position, self.current_position))
    }
}

fn is_digit(c: char) -> bool {
    c.is_digit(10)
}

fn is_identifier_char(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::position::{BytePos, Span, WithSpan};
    use crate::token::Token;

    fn tokenize(input: &str) -> Vec<WithSpan<Token>> {
        let mut lex = Lexer::new(input);
        let mut tokens = Vec::new();

        loop {
            let tok = lex.next_token();

            if tok.value == Token::Eof {
                return tokens;
            } else {
                tokens.push(tok);
            }
        }
    }

    #[test]
    fn test_spans_eof() {
        let input = "1";
        let mut lex = Lexer::new(input);

        // Skip the `1`
        lex.next_token();

        let final_span = Span::new_unchecked(1, 1);
        // Make sure that the span does not change upon hitting eof
        assert_eq!(lex.next_token().span, final_span);
        // Make sure that the span does not change upon hitting eof
        assert_eq!(lex.next_token().span, Span::new_unchecked(1, 1));
    }

    #[test]
    fn test_operators() {
        let input = "+ - * / = ! < > == != <= >=";
        let expected_tokens = vec![
            WithSpan::new(Token::Plus, Span::new_unchecked(0, 1)),
            WithSpan::new(Token::Minus, Span::new_unchecked(2, 3)),
            WithSpan::new(Token::Star, Span::new_unchecked(4, 5)),
            WithSpan::new(Token::Slash, Span::new_unchecked(6, 7)),
            WithSpan::new(Token::Equal, Span::new_unchecked(8, 9)),
            WithSpan::new(Token::Bang, Span::new_unchecked(10, 11)),
            WithSpan::new(Token::LessThan, Span::new_unchecked(12, 13)),
            WithSpan::new(Token::GreaterThan, Span::new_unchecked(14, 15)),
            WithSpan::new(Token::EqualEqual, Span::new_unchecked(16, 18)),
            WithSpan::new(Token::BangEqual, Span::new_unchecked(19, 21)),
            WithSpan::new(Token::LessEqual, Span::new_unchecked(22, 24)),
            WithSpan::new(Token::GreaterEqual, Span::new_unchecked(25, 27)),
        ];

        let tokens = tokenize(input);

        for (token, expected_token) in tokens.iter().zip(expected_tokens.iter()) {
            assert_eq!(
                token, expected_token,
                "expected {} at {} but got {} at {}",
                expected_token.value, expected_token.span, token.value, token.span,
            )
        }
    }

    #[test]
    fn test_delimiters() {
        let input = ", ; ( ) { }";
        let expected_tokens = vec![
            WithSpan::new(Token::Comma, Span::new_unchecked(0, 1)),
            WithSpan::new(Token::Semicolon, Span::new_unchecked(2, 3)),
            WithSpan::new(Token::LeftParen, Span::new_unchecked(4, 5)),
            WithSpan::new(Token::RightParen, Span::new_unchecked(6, 7)),
            WithSpan::new(Token::LeftBrace, Span::new_unchecked(8, 9)),
            WithSpan::new(Token::RightBrace, Span::new_unchecked(10, 11)),
        ];

        let tokens = tokenize(input);

        for (token, expected_token) in tokens.iter().zip(expected_tokens.iter()) {
            assert_eq!(
                token, expected_token,
                "expected {} at {} but got {} at {}",
                expected_token.value, expected_token.span, token.value, token.span,
            )
        }
    }

    #[test]
    fn test_identifier() {
        let input = "hello _world _hello_world_";
        let expected_tokens = vec![
            WithSpan::new(
                Token::Identifier("hello".to_owned()),
                Span::new_unchecked(0, 5),
            ),
            WithSpan::new(
                Token::Identifier("_world".to_owned()),
                Span::new_unchecked(6, 12),
            ),
            WithSpan::new(
                Token::Identifier("_hello_world_".to_owned()),
                Span::new_unchecked(13, 26),
            ),
        ];

        let tokens = tokenize(input);

        for (token, expected_token) in tokens.iter().zip(expected_tokens.iter()) {
            assert_eq!(
                token, expected_token,
                "expected {} at {} but got {} at {}",
                expected_token.value, expected_token.span, token.value, token.span,
            )
        }
    }

    #[test]
    fn test_number() {
        let input = "012312";
        let mut lex = Lexer::new(input);
        let token = lex.next_token();
        let expected_token = WithSpan::new(Token::Number(12312), Span::new_unchecked(0, 6));
        assert_eq!(
            token, expected_token,
            "expected {} at {} but got {} at {}",
            expected_token.value, expected_token.span, token.value, token.span,
        );
    }

    #[test]
    fn test_keywords() {
        let input = "true false nil fn let if else return";
        let expected_tokens = vec![
            WithSpan::new(Token::True, Span::new_unchecked(0, 4)),
            WithSpan::new(Token::False, Span::new_unchecked(5, 10)),
            WithSpan::new(Token::Nil, Span::new_unchecked(11, 14)),
            WithSpan::new(Token::Fn, Span::new_unchecked(15, 17)),
            WithSpan::new(Token::Let, Span::new_unchecked(18, 21)),
            WithSpan::new(Token::If, Span::new_unchecked(22, 24)),
            WithSpan::new(Token::Else, Span::new_unchecked(25, 29)),
            WithSpan::new(Token::Return, Span::new_unchecked(30, 36)),
        ];

        let tokens = tokenize(input);

        for (token, expected_token) in tokens.iter().zip(expected_tokens.iter()) {
            assert_eq!(
                token, expected_token,
                "expected {} at {} but got {} at {}",
                expected_token.value, expected_token.span, token.value, token.span,
            )
        }
    }

    #[test]
    fn test_eof() {
        let input = "";
        let mut lex = Lexer::new(input);
        assert_eq!(lex.next_token().value, Token::Eof);
    }
}
