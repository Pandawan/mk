use std::fmt::Display;
use std::iter::Peekable;
use std::num::{ParseFloatError, ParseIntError};
use std::str::Chars;

use crate::span::{BytePos, Span, WithSpan};
use crate::token::Token;

#[derive(Debug)]
pub enum LexError {
    UnexpectedToken(char),
    StringNotClosed(char),
    InvalidEscape(char),
    InvalidFloat(ParseFloatError),
    InvalidInt(ParseIntError),
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::UnexpectedToken(c) => write!(f, "Unexpected token \"{}\"", c),
            LexError::StringNotClosed(c) => write!(
                f,
                "Expected closing {} of string literal but reached EOF",
                c
            ),
            LexError::InvalidEscape(c) => write!(f, "Invalid escape sequence \\{}", c),
            LexError::InvalidFloat(err) => write!(f, "Invalid float: {}", err),
            LexError::InvalidInt(err) => write!(f, "Invalid int: {}", err),
        }
    }
}

type LexResult<T> = Result<T, LexError>;

pub struct Lexer<'a> {
    input_iter: Peekable<Chars<'a>>,
    current_position: BytePos,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input_iter: input.chars().peekable(),
            current_position: BytePos::new(0),
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

    /// Read the next characters as a string
    fn read_string(&mut self, opening: char) -> LexResult<Token> {
        let mut str = String::new();

        loop {
            let ch = self.read_char();

            match ch {
                // Closing string character matches that of opening
                Some(ch) if ch == opening => break,
                Some('\\') => match self.read_char() {
                    Some('\'') => str.push('\''),
                    Some('\"') => str.push('\"'),
                    Some('\\') => str.push('\\'),
                    Some('n') => str.push('\n'),
                    Some('r') => str.push('\r'),
                    Some('t') => str.push('\t'),
                    Some('0') => str.push('\0'),
                    // TODO: Allow Hex escape sequences in format \xFF

                    // TODO: I don't like repeating code like this
                    Some(ch) if ch == opening => break,
                    Some(ch) => return Err(LexError::InvalidEscape(ch)),
                    None => return Err(LexError::StringNotClosed(opening)),
                },

                // Any other character goes into the string
                Some(ch) => str.push(ch),
                None => return Err(LexError::StringNotClosed(opening)),
            }
        }

        Ok(Token::String(str))
    }

    /// Read the current and following characters as a number token.
    /// Source: https://michael-f-bryan.github.io/static-analyser-in-rust/book/lex.html
    fn read_number(&mut self, first: char) -> LexResult<Token> {
        let mut seen_dot = false;

        let mut s = String::new();

        // Add the first digit
        s.push(first);

        // Keep adding until next character is not a digit
        while let Some(&ch) = self.peek_char() {
            if is_digit(ch) {
                s.push(self.read_char().unwrap());
            }
            // Dot for decimals
            else if ch == '.' {
                if seen_dot == false {
                    seen_dot = true;
                    s.push(self.read_char().unwrap());
                } else {
                    break;
                }
            }
            // Underscore for separation
            else if ch == '_' {
                // Ignore it
                self.read_char();
            } else {
                break;
            }
        }

        if seen_dot {
            match s.parse() {
                Ok(value) => Ok(Token::Float(value)),
                Err(error) => Err(LexError::InvalidFloat(error)),
            }
        } else {
            match s.parse() {
                Ok(value) => Ok(Token::Integer(value)),
                Err(error) => Err(LexError::InvalidInt(error)),
            }
        }
    }

    /// Read the current and following tokens as an identifier or a keyword (if it exists).
    fn read_identifier_or_keyword(&mut self, first: char) -> LexResult<Token> {
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
            Ok(keyword_token)
        } else {
            Ok(Token::Identifier(identifier))
        }
    }

    /// Read a new token from the characters list.
    pub fn next_token(&mut self) -> LexResult<WithSpan<Token>> {
        self.skip_whitespace();

        let initial_position = self.current_position;

        let token = if let Some(c) = self.read_char() {
            match c {
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => match self.peek_char() {
                    // Double star
                    Some('*') => {
                        self.read_char();
                        Token::StarStar
                    }
                    _ => Token::Star,
                },
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
                '.' => Token::Dot,
                ';' => Token::Semicolon,
                ':' => Token::Colon,

                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                '[' => Token::LeftBracket,
                ']' => Token::RightBracket,

                '&' => match self.peek_char() {
                    // AndAnd: &&
                    Some('&') => {
                        self.read_char();
                        Token::AndAnd
                    }
                    _ => return Err(LexError::UnexpectedToken(c)),
                },
                '|' => match self.peek_char() {
                    // OrOr: ||
                    Some('|') => {
                        self.read_char();
                        Token::OrOr
                    }
                    _ => return Err(LexError::UnexpectedToken(c)),
                },

                '"' | '\'' => self.read_string(c)?,

                c if is_digit(c) => self.read_number(c)?,
                c if is_identifier_char(c) => self.read_identifier_or_keyword(c)?,

                _ => return Err(LexError::UnexpectedToken(c)),
            }
        } else {
            Token::Eof
        };

        let span = Span::new(initial_position, self.current_position);

        Ok(WithSpan::new(token, span))
    }
}

/// Whether or not the given character is a digit
fn is_digit(c: char) -> bool {
    c.is_digit(10)
}

/// Whether or not the given character is valid in an identifier
fn is_identifier_char(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

#[cfg(test)]
mod tests {
    use crate::lexer::{LexError, Lexer};
    use crate::span::{BytePos, Span};
    use crate::token::Token;

    #[test]
    fn test_operators() {
        let input = "+-*/=! **==!=<><=>= && ||";
        let mut lex = Lexer::new(input);

        assert_eq!(lex.next_token().unwrap().value, Token::Plus);
        assert_eq!(lex.next_token().unwrap().value, Token::Minus);
        assert_eq!(lex.next_token().unwrap().value, Token::Star);
        assert_eq!(lex.next_token().unwrap().value, Token::Slash);
        assert_eq!(lex.next_token().unwrap().value, Token::Equal);
        assert_eq!(lex.next_token().unwrap().value, Token::Bang);
        assert_eq!(lex.next_token().unwrap().value, Token::StarStar);

        assert_eq!(lex.next_token().unwrap().value, Token::EqualEqual);
        assert_eq!(lex.next_token().unwrap().value, Token::BangEqual);
        assert_eq!(lex.next_token().unwrap().value, Token::LessThan);
        assert_eq!(lex.next_token().unwrap().value, Token::GreaterThan);
        assert_eq!(lex.next_token().unwrap().value, Token::LessEqual);
        assert_eq!(lex.next_token().unwrap().value, Token::GreaterEqual);

        assert_eq!(lex.next_token().unwrap().value, Token::AndAnd);
        assert_eq!(lex.next_token().unwrap().value, Token::OrOr);
    }

    #[test]
    fn test_delimiters() {
        let input = ",.;:(){}[]";
        let mut lex = Lexer::new(input);

        assert_eq!(lex.next_token().unwrap().value, Token::Comma);
        assert_eq!(lex.next_token().unwrap().value, Token::Dot);
        assert_eq!(lex.next_token().unwrap().value, Token::Semicolon);
        assert_eq!(lex.next_token().unwrap().value, Token::Colon);

        assert_eq!(lex.next_token().unwrap().value, Token::LeftParen);
        assert_eq!(lex.next_token().unwrap().value, Token::RightParen);
        assert_eq!(lex.next_token().unwrap().value, Token::LeftBrace);
        assert_eq!(lex.next_token().unwrap().value, Token::RightBrace);
        assert_eq!(lex.next_token().unwrap().value, Token::LeftBracket);
        assert_eq!(lex.next_token().unwrap().value, Token::RightBracket);
    }

    #[test]
    fn test_identifier() {
        let input = "hello _world _hello_world_";
        let mut lex = Lexer::new(input);
        assert_eq!(
            lex.next_token().unwrap().value,
            Token::Identifier("hello".to_owned())
        );
        assert_eq!(
            lex.next_token().unwrap().value,
            Token::Identifier("_world".to_owned())
        );
        assert_eq!(
            lex.next_token().unwrap().value,
            Token::Identifier("_hello_world_".to_owned())
        );
    }

    #[test]
    fn test_integer() {
        let input = "012312 12_345";
        let mut lex = Lexer::new(input);
        assert_eq!(lex.next_token().unwrap().value, Token::Integer(12312));
        assert_eq!(lex.next_token().unwrap().value, Token::Integer(12345));
    }

    #[test]
    fn test_float() {
        let input = "012312.321 123_45.6_7";
        let mut lex = Lexer::new(input);
        assert_eq!(lex.next_token().unwrap().value, Token::Float(12312.321));
        assert_eq!(lex.next_token().unwrap().value, Token::Float(12345.67));
    }

    #[test]
    fn test_string() {
        let input = "\"foobar\" \'foo bar\' \"not closed";
        let mut lex = Lexer::new(input);
        assert_eq!(
            lex.next_token().unwrap().value,
            Token::String("foobar".to_string())
        );
        assert_eq!(
            lex.next_token().unwrap().value,
            Token::String("foo bar".to_string())
        );

        match lex.next_token() {
            Err(LexError::StringNotClosed('"')) => {}
            Err(LexError::StringNotClosed(c)) => {
                panic!("expected string not closed error for \" but got for {}", c)
            }
            result => panic!("expected unclosed string error for \" but got {:?}", result),
        }
    }

    #[test]
    fn test_keywords() {
        let input = "true false fn let if else return";
        let mut lex = Lexer::new(input);

        assert_eq!(lex.next_token().unwrap().value, Token::True);
        assert_eq!(lex.next_token().unwrap().value, Token::False);
        assert_eq!(lex.next_token().unwrap().value, Token::Fn);
        assert_eq!(lex.next_token().unwrap().value, Token::Let);
        assert_eq!(lex.next_token().unwrap().value, Token::If);
        assert_eq!(lex.next_token().unwrap().value, Token::Else);
        assert_eq!(lex.next_token().unwrap().value, Token::Return);
    }

    #[test]
    fn test_eof() {
        let input = "";
        let mut lex = Lexer::new(input);
        assert_eq!(lex.next_token().unwrap().value, Token::Eof)
    }

    #[test]
    fn test_spans_eof() {
        let input = "1";
        let mut lex = Lexer::new(input);

        // Skip the `1`
        lex.next_token().unwrap();

        let final_span = Span::new(BytePos::new(1), BytePos::new(1));
        // Make sure that the span does not change upon hitting eof
        assert_eq!(lex.next_token().unwrap().span, final_span);
        // Make sure that the span does not change upon hitting eof
        assert_eq!(
            lex.next_token().unwrap().span,
            Span::new(BytePos::new(1), BytePos::new(1))
        );
    }

    #[test]
    fn test_spans() {
        let input = "abc 12 + return";
        let mut lex = Lexer::new(input);

        assert_eq!(
            lex.next_token().unwrap().span,
            Span::new(BytePos::new(0), BytePos::new(3))
        );
        assert_eq!(
            lex.next_token().unwrap().span,
            Span::new(BytePos::new(4), BytePos::new(6))
        );
        assert_eq!(
            lex.next_token().unwrap().span,
            Span::new(BytePos::new(7), BytePos::new(8))
        );
        assert_eq!(
            lex.next_token().unwrap().span,
            Span::new(BytePos::new(9), BytePos::new(15))
        );
    }
}
