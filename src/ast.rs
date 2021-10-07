use crate::token::Token;
use std::{fmt::Display, rc::Rc};

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
        let s = (&self.statements)
            .iter()
            .map(|stmt| stmt.to_string())
            .collect::<Vec<String>>()
            .join("; ");

        write!(f, "{}", s)?;

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
        use Statement::*;

        match self {
            Let { name, value } => write!(
                f,
                "{tok} {ident} = {val};",
                tok = Token::Let,
                ident = name,
                val = value
            ),
            Return { value } => write!(f, "{} {}", Token::Return, value),
            Expression { expression } => write!(f, "{}", expression),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    // Literal
    Identifier(IdentifierLiteral),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Nil,

    // Complex
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
    Assignment(Box<AssignmentExpression>),
    Block(Box<BlockExpression>),
    If(Box<IfExpression>),
    Function(Box<FunctionLiteral>),
    Call(Box<CallExpression>),
    Array(Box<ArrayLiteral>),
    Index(Box<IndexExpression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expression::*;

        match self {
            Identifier(name) => write!(f, "{}", name),
            Integer(value) => write!(f, "{}", value),
            Float(value) => write!(f, "{}", value),
            Boolean(value) => write!(f, "{}", value),
            String(value) => write!(f, "\"{}\"", value),
            Nil => write!(f, "nil"),

            Prefix(prefix) => write!(f, "{}", prefix),
            Infix(infix) => write!(f, "{}", infix),
            Assignment(assignment) => write!(f, "{}", assignment),
            Block(block) => write!(f, "{}", block),
            If(if_exp) => write!(f, "{}", if_exp),
            Function(func) => write!(f, "{}", func),
            Call(call) => write!(f, "{}", call),
            Array(arr) => write!(f, "{}", arr),
            Index(index) => write!(f, "{}", index),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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
        Self {
            name: name.to_owned(),
        }
    }
}

impl From<String> for IdentifierLiteral {
    fn from(name: String) -> Self {
        Self { name }
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
pub struct AssignmentExpression {
    // TODO: Add support for assigning array indices, etc. (allow any kind of expression and check at runtime?)
    pub identifier: IdentifierLiteral,
    pub value: Expression,
}

impl Display for AssignmentExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({l} = {r})", l = self.identifier, r = self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockExpression {
    pub statements: Vec<Statement>,
}

impl Display for BlockExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct IfExpression {
    pub condition: Expression,
    /// Block if condition is true
    pub consequence: BlockExpression,
    /// Expression (BlockExpression or IfExpression) if condition is false
    pub alternative: Option<Expression>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(if {} {}", self.condition, self.consequence)?;

        if let Some(ref alt) = self.alternative {
            write!(f, " else {}", alt)?;
        }

        write!(f, ")")?;

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionLiteral {
    /// Parameter identifiers
    pub parameters: Vec<IdentifierLiteral>,
    // TODO: I really don't like having to use Rc for the AST; it should not have to worry about the needs of the interpreter
    pub body: Rc<BlockExpression>,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(fn({}) {})",
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

#[derive(Debug, PartialEq)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.elements
                .iter()
                .map(|ident| ident.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct IndexExpression {
    pub left: Expression,
    pub index: Expression,
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
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
