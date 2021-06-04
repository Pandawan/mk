pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Let {
        // TODO: Add some kind of Span (maybe codemap::Span) to keep track of where each token is in the source
        /// The name/identifier of the variable
        name: String,
        /// The value being assigned
        value: Expression,
    },
}

#[derive(Debug)]
pub enum Expression {
    Identifier(String),
    Nil,
}
