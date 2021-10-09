use std::fmt;

// TODO: Add some kind of Span to keep track of where each token is in the source (Perhaps a WithSpan<>)
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    Bang,
    StarStar,

    EqualEqual,
    BangEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,

    // Delimiters
    Comma,
    Dot,
    Semicolon,
    Colon,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    AndAnd, // &&
    OrOr,   // ||

    // Identifiers & Literals
    Identifier(String),
    Integer(i64),
    Float(f64),
    String(String),

    // Keywords
    True,
    False,
    Nil,
    Fn,
    Let,
    If,
    Else,
    Return,

    // Special
    Eof,
}

impl Token {
    /// Get the Token for the given keyword, if valid.
    pub fn lookup_keyword(s: &str) -> Option<Token> {
        use Token::*;

        match s {
            "true" => Some(True),
            "false" => Some(False),
            "nil" => Some(Nil),
            "fn" => Some(Fn),
            "let" => Some(Let),
            "if" => Some(If),
            "else" => Some(Else),
            "return" => Some(Return),
            _ => None,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;

        match self {
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            Equal => write!(f, "="),
            Bang => write!(f, "!"),
            StarStar => write!(f, "**"),

            EqualEqual => write!(f, "=="),
            BangEqual => write!(f, "!="),
            LessThan => write!(f, "<"),
            GreaterThan => write!(f, ">"),
            LessEqual => write!(f, "<="),
            GreaterEqual => write!(f, ">="),

            Comma => write!(f, ","),
            Dot => write!(f, "."),
            Semicolon => write!(f, ";"),
            Colon => write!(f, ":"),

            LeftParen => write!(f, "("),
            RightParen => write!(f, ")"),
            LeftBrace => write!(f, "{{"),
            RightBrace => write!(f, "}}"),
            LeftBracket => write!(f, "["),
            RightBracket => write!(f, "]"),

            AndAnd => write!(f, "&&"),
            OrOr => write!(f, "||"),

            Identifier(name) => write!(f, "{}", name),
            Integer(value) => write!(f, "{}", value),
            Float(value) => {
                // Force one-decimal value for floats with no decimal place
                // e.g. 1.0 instead of 1
                if value.fract() == 0.0 {
                    write!(f, "{:.1}", value)
                } else {
                    write!(f, "{}", value)
                }
            }
            String(value) => write!(f, "\"{}\"", value),

            True => write!(f, "true"),
            False => write!(f, "false"),
            Nil => write!(f, "nil"),
            Fn => write!(f, "fn"),
            Let => write!(f, "let"),
            If => write!(f, "if"),
            Else => write!(f, "else"),
            Return => write!(f, "return"),

            Eof => write!(f, "EOF"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::Token;

    #[test]
    fn float_formatting() {
        assert_eq!(format!("{}", Token::Float(12345.0)), "12345.0");
        assert_eq!(format!("{}", Token::Float(1.0)), "1.0");
        assert_eq!(format!("{}", Token::Float(0.0)), "0.0");
        assert_eq!(format!("{}", Token::Float(0.1)), "0.1");
        assert_eq!(format!("{}", Token::Float(0.12345)), "0.12345");
    }
}
