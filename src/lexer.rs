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
    fn test_spans() {
        let input = "abc 12 + return";
        let expected_spans = [
            Span::new(BytePos::new(0), BytePos::new(3)),
            Span::new(BytePos::new(4), BytePos::new(6)),
            Span::new(BytePos::new(7), BytePos::new(8)),
            Span::new(BytePos::new(9), BytePos::new(15)),
        ];
        let tokens = tokenize(input);

        assert_eq!(expected_spans.len(), tokens.len());

        for (token, &expected_span) in tokens.iter().zip(expected_spans.iter()) {
            assert_eq!(token.span, expected_span);
        }
    }

    #[test]
    fn test_spans_eof() {
        let input = "1";
        let mut lex = Lexer::new(input);

        // Skip the `1`
        lex.next_token();

        let final_span = Span::new(BytePos::new(1), BytePos::new(1));
        // Make sure that the span does not change upon hitting eof
        assert_eq!(lex.next_token().span, final_span);
        // Make sure that the span does not change upon hitting eof
        assert_eq!(
            lex.next_token().span,
            Span::new(BytePos::new(1), BytePos::new(1))
        );
    }

    #[test]
    fn test_operators() {
        let input = "+-*/=! ==!=<><=>=";

        assert_eq!(
            tokenize(input)
                .iter()
                .map(|t| t.value.clone())
                .collect::<Vec<Token>>(),
            vec![
                Token::Plus,
                Token::Minus,
                Token::Star,
                Token::Slash,
                Token::Equal,
                Token::Bang,
                Token::EqualEqual,
                Token::BangEqual,
                Token::LessThan,
                Token::GreaterThan,
                Token::LessEqual,
                Token::GreaterEqual,
            ]
        )
    }

    #[test]
    fn test_delimiters() {
        let input = ",;(){}";

        assert_eq!(
            tokenize(input)
                .iter()
                .map(|t| t.value.clone())
                .collect::<Vec<Token>>(),
            vec![
                Token::Comma,
                Token::Semicolon,
                Token::LeftParen,
                Token::RightParen,
                Token::LeftBrace,
                Token::RightBrace,
            ]
        )
    }

    #[test]
    fn test_identifier() {
        let input = "hello _world _hello_world_";

        assert_eq!(
            tokenize(input)
                .iter()
                .map(|t| t.value.clone())
                .collect::<Vec<Token>>(),
            vec![
                Token::Identifier("hello".to_owned()),
                Token::Identifier("_world".to_owned()),
                Token::Identifier("_hello_world_".to_owned())
            ]
        )
    }

    #[test]
    fn test_number() {
        let input = "012312";
        let mut lex = Lexer::new(input);
        assert_eq!(lex.next_token().value, Token::Number(12312));
    }

    #[test]
    fn test_keywords() {
        let input = "true false fn let if else return";

        assert_eq!(
            tokenize(input)
                .iter()
                .map(|t| t.value.clone())
                .collect::<Vec<Token>>(),
            vec![
                Token::True,
                Token::False,
                Token::Fn,
                Token::Let,
                Token::If,
                Token::Else,
                Token::Return,
            ]
        )
    }

    #[test]
    fn test_eof() {
        let input = "";
        let mut lex = Lexer::new(input);
        assert_eq!(lex.next_token().value, Token::Eof);
    }
}
