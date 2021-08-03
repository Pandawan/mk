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

// TODO: Block Statements are special, should they still be part of the Statement enum?
#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    // Literal
    Identifier(IdentifierLiteral),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Nil,

    // Complex
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
    If(Box<IfExpression>),
    Function(Box<FunctionLiteral>),
    Call(Box<CallExpression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(name) => write!(f, "{}", name),
            Self::Integer(value) => write!(f, "{}", value),
            Self::Float(value) => write!(f, "{}", value),
            Self::Boolean(value) => write!(f, "{}", value),
            Self::Nil => write!(f, "nil"),

            Self::Prefix(prefix) => write!(f, "{}", prefix),
            Self::Infix(infix) => write!(f, "{}", infix),
            Self::If(if_exp) => write!(f, "{}", if_exp),
            Self::Function(func) => write!(f, "{}", func),
            Self::Call(call) => write!(f, "{}", call),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct IdentifierLiteral {
    pub name: String,
}

impl Display for IdentifierLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl From<&str> for IdentifierLiteral {
    fn from(name: &str) -> Self {
        return Self {
            name: name.to_owned(),
        };
    }
}

impl From<String> for IdentifierLiteral {
    fn from(name: String) -> Self {
        return Self { name };
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

#[derive(Debug, PartialEq)]
pub struct IfExpression {
    pub condition: Expression,
    // Block if condition is true
    pub consequence: BlockStatement,
    // Block if condition is false
    pub alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} {}", self.condition, self.consequence)?;

        if let Some(ref stmt) = self.alternative {
            write!(f, "else {}", stmt)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionLiteral {
    // Parameter identifiers
    pub parameters: Vec<IdentifierLiteral>,
    pub body: BlockStatement,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn({}) {}",
            self.parameters
                .iter()
                .map(|ident| ident.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            self.body
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct CallExpression {
    pub function: Expression,
    pub arguments: Vec<Expression>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.function,
            self.arguments
                .iter()
                .map(|ident| ident.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, IdentifierLiteral, Program, Statement};
    #[test]
    fn test_display_program() {
        let program = Program {
            statements: vec![Statement::Let {
                name: "myVar".to_string(),
                value: Expression::Identifier(IdentifierLiteral::from("anotherVar".to_string())),
            }],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;")
    }
}
