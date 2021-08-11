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
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    // Identifiers & Literals
    Identifier(String),
    Integer(i64),
    Float(f64),

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
            Semicolon => write!(f, ";"),
            LeftParen => write!(f, "("),
            RightParen => write!(f, ")"),
            LeftBrace => write!(f, "{{"),
            RightBrace => write!(f, "}}"),

            Identifier(name) => write!(f, "{}", name),
            Integer(value) => write!(f, "{}", value),
            /* TODO: This may not always print the correct value.
            e.g. `1.0` will print `1` */
            Float(value) => write!(f, "{}", value),

            True => write!(f, "true"),
            False => write!(f, "false"),
            Nil => write!(f, "nil"),
            Fn => write!(f, "fn"),
            Let => write!(f, "let"),
            If => write!(f, "if"),
            Else => write!(f, "else"),
            Return => write!(f, "return"),

            Eof => write!(f, "EOF"),
            // tok => write!(f, "{:?}", tok),
        }
    }
}
