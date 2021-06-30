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
    Number(u64),

    // Keywords
    True,
    False,
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
        match s {
            "true" => Some(Token::True),
            "false" => Some(Token::False),
            "fn" => Some(Token::Fn),
            "let" => Some(Token::Let),
            "if" => Some(Token::If),
            "else" => Some(Token::Else),
            "return" => Some(Token::Return),
            _ => None,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Equal => write!(f, "="),
            Token::Bang => write!(f, "!"),

            Token::EqualEqual => write!(f, "=="),
            Token::BangEqual => write!(f, "!="),
            Token::LessThan => write!(f, "<"),
            Token::GreaterThan => write!(f, ">"),
            Token::LessEqual => write!(f, "<="),
            Token::GreaterEqual => write!(f, ">="),

            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),

            Token::Identifier(name) => write!(f, "{}", name),
            Token::Number(val) => write!(f, "{}", val),

            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),

            Token::Eof => write!(f, "EOF"),
            // tok => write!(f, "{:?}", tok),
        }
    }
}
