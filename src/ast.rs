use crate::{position::WithSpan, token::Token};
use std::fmt::Display;

pub struct Program {
    pub statements: Vec<WithSpan<Statement>>,
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
            write!(f, "{}", statement.value)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let {
        /// The name/identifier of the variable
        name: WithSpan<IdentifierLiteral>,
        /// The value being assigned
        value: WithSpan<Expression>,
    },
    Return {
        /// The value being returned
        value: WithSpan<Expression>,
    },
    Expression {
        /// The expression for this statement
        expression: WithSpan<Expression>,
    },
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let { name, value } => write!(
                f,
                "{tok} {ident} = {val};",
                tok = Token::Let,
                ident = name.value,
                val = value.value
            ),
            Self::Return { value } => write!(f, "{} {}", Token::Return, value.value),
            Self::Expression { expression } => write!(f, "{}", expression.value),
        }
    }
}

// TODO: Block Statements are special, should they still be part of the Statement enum?
#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    pub statements: Vec<WithSpan<Statement>>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt.value)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    // Literal
    Identifier(IdentifierLiteral),
    Number(u64),
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
            Self::Number(value) => write!(f, "{}", value),
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
    pub operator: WithSpan<Token>,
    pub right: WithSpan<Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({op}{r})",
            op = self.operator.value,
            r = self.right.value
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    pub left: WithSpan<Expression>,
    pub operator: WithSpan<Token>,
    pub right: WithSpan<Expression>,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({l} {op} {r})",
            l = self.left.value,
            op = self.operator.value,
            r = self.right.value
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct IfExpression {
    pub condition: WithSpan<Expression>,
    // Block if condition is true
    pub consequence: WithSpan<BlockStatement>,
    // Block if condition is false
    pub alternative: Option<WithSpan<BlockStatement>>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} {}", self.condition.value, self.consequence.value)?;

        if let Some(ref stmt) = self.alternative {
            write!(f, "else {}", stmt.value)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionLiteral {
    // Parameter identifiers
    pub parameters: Vec<WithSpan<IdentifierLiteral>>,
    pub body: WithSpan<BlockStatement>,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn({}) {}",
            self.parameters
                .iter()
                .map(|ident| ident.value.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            self.body.value
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct CallExpression {
    pub function: WithSpan<Expression>,
    pub arguments: Vec<WithSpan<Expression>>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.function.value,
            self.arguments
                .iter()
                .map(|ident| ident.value.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expression, IdentifierLiteral, Program, Statement},
        position::WithSpan,
    };
    #[test]
    fn test_display_program() {
        let program = Program {
            // Using WithSpan::empty b/c convert to string anyway
            statements: vec![WithSpan::empty(Statement::Let {
                name: WithSpan::empty(IdentifierLiteral::from("myVar".to_string())),
                value: WithSpan::empty(Expression::Identifier(IdentifierLiteral::from(
                    "anotherVar".to_string(),
                ))),
            })],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;")
    }
}
