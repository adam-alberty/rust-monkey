use std::fmt;

use crate::{
    ast::{Expression, Identifier},
    token::Token,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(s) => write!(f, "{s}"),
            Statement::Return(s) => write!(f, "{s}"),
            Statement::Expression(s) => write!(f, "{s}"),
            Statement::Block(s) => write!(f, "{s}"),
        }
    }
}

////////////////
// Statements //
////////////////

#[derive(Debug, PartialEq, Eq)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Expression>,
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(value) = &self.value {
            write!(f, "{} {} = {};", self.token.literal, self.name.value, value)
        } else {
            write!(f, "{} {} = ;", self.token.literal, self.name.value)
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Expression>,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{};", self.token.literal) // TODO - add self.return_value
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for s in &self.statements {
            write!(f, "{}", s)?;
        }

        Ok(())
    }
}
