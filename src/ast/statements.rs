use std::fmt;

use crate::{
    ast::{Expression, Identifier},
    token::Token,
};

pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(s) => write!(f, "{s}"),
            Statement::Return(s) => write!(f, "{s}"),
            Statement::Expression(s) => write!(f, "{s}"),
        }
    }
}

////////////////
// Statements //
////////////////

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

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Expression>,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{};", self.token.literal) // TODO - add self.return_value
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}
