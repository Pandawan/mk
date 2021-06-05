use std::iter::Peekable;
use std::str::Chars;

use crate::token::Token;

pub struct Lexer<'a> {
    input_iter: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input_iter: input.chars().peekable(),
        }
    }

    /// Consume the next character from the list.
    fn read_char(&mut self) -> Option<char> {
        self.input_iter.next()
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
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if let Some(c) = self.read_char() {
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
        }
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
    use crate::token::Token;

    #[test]
    fn test_operators() {
        let input = "+-*/=! ==!=<><=>=";
        let mut lex = Lexer::new(input);

        assert_eq!(lex.next_token(), Token::Plus);
        assert_eq!(lex.next_token(), Token::Minus);
        assert_eq!(lex.next_token(), Token::Star);
        assert_eq!(lex.next_token(), Token::Slash);
        assert_eq!(lex.next_token(), Token::Equal);
        assert_eq!(lex.next_token(), Token::Bang);

        assert_eq!(lex.next_token(), Token::EqualEqual);
        assert_eq!(lex.next_token(), Token::BangEqual);
        assert_eq!(lex.next_token(), Token::LessThan);
        assert_eq!(lex.next_token(), Token::GreaterThan);
        assert_eq!(lex.next_token(), Token::LessEqual);
        assert_eq!(lex.next_token(), Token::GreaterEqual);
    }

    #[test]
    fn test_delimiters() {
        let input = ",;(){}";
        let mut lex = Lexer::new(input);

        assert_eq!(lex.next_token(), Token::Comma);
        assert_eq!(lex.next_token(), Token::Semicolon);
        assert_eq!(lex.next_token(), Token::LeftParen);
        assert_eq!(lex.next_token(), Token::RightParen);
        assert_eq!(lex.next_token(), Token::LeftBrace);
        assert_eq!(lex.next_token(), Token::RightBrace);
    }

    #[test]
    fn test_identifier() {
        let input = "hello _world _hello_world_";
        let mut lex = Lexer::new(input);
        assert_eq!(lex.next_token(), Token::Identifier("hello".to_owned()));
        assert_eq!(lex.next_token(), Token::Identifier("_world".to_owned()));
        assert_eq!(
            lex.next_token(),
            Token::Identifier("_hello_world_".to_owned())
        );
    }

    #[test]
    fn test_number() {
        let input = "012312";
        let mut lex = Lexer::new(input);
        assert_eq!(lex.next_token(), Token::Number(12312));
    }

    #[test]
    fn test_keywords() {
        let input = "true false func let if else return";
        let mut lex = Lexer::new(input);
        assert_eq!(lex.next_token(), Token::True);
        assert_eq!(lex.next_token(), Token::False);
        assert_eq!(lex.next_token(), Token::Func);
        assert_eq!(lex.next_token(), Token::Let);
        assert_eq!(lex.next_token(), Token::If);
        assert_eq!(lex.next_token(), Token::Else);
        assert_eq!(lex.next_token(), Token::Return);
    }

    #[test]
    fn test_eof() {
        let input = "";
        let mut lex = Lexer::new(input);
        assert_eq!(lex.next_token(), Token::Eof)
    }
}
