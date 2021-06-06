use crate::token::Token;
use std::fmt::Display;

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

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let {
        /// The name/identifier of the variable
        name: String,
        /// The value being assigned
        value: Expression,
    },
    Return {
        /// The value being returned
        value: Expression,
    },
    Expression {
        /// The expression for this statement
        expression: Expression,
    },
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let { name, value } => write!(
                f,
                "{tok} {ident} = {val};",
                tok = Token::Let,
                ident = name,
                val = value
            ),
            Self::Return { value } => write!(f, "{} {}", Token::Return, value),
            Self::Expression { expression } => write!(f, "{}", expression),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    // Literal
    Identifier(String),
    Number(u64),
    Nil,

    // Complex
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(name) => write!(f, "{}", name),
            Self::Number(value) => write!(f, "{}", value),
            Self::Nil => write!(f, "nil"),

            Self::Prefix(prefix) => write!(f, "{}", prefix),
            Self::Infix(infix) => write!(f, "{}", infix),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    pub operator: Token,
    pub right: Expression,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({op}{r})", op = self.operator, r = self.right)
    }
}

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    pub left: Expression,
    pub operator: Token,
    pub right: Expression,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({l} {op} {r})",
            l = self.left,
            op = self.operator,
            r = self.right
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Program, Statement};
    #[test]
    fn test_display_program() {
        let program = Program {
            statements: vec![Statement::Let {
                name: "myVar".to_string(),
                value: Expression::Identifier("anotherVar".to_string()),
            }],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;")
    }
}
