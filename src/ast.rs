use std::fmt;

use crate::token::Token;

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

pub enum Expression {
    Ident(Identifier),
    IntegerLiteral(IntegerLiteral),
    Prefix(Prefix),
    Infix(Infix),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Ident(ident) => write!(f, "{}", ident.value),
            Expression::IntegerLiteral(int_literal) => write!(f, "{}", int_literal.value),
            Expression::Prefix(prefix) => write!(f, "({}{})", prefix.operator, prefix.right),
            Expression::Infix(infix) => {
                write!(f, "({} {} {})", infix.left, infix.operator, infix.right)
            }
        }
    }
}

// Expressions

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

pub struct Prefix {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

pub struct Infix {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

// Root program node

pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for statement in self.statements.iter() {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
}

// Statements

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Expression>,
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} = {};",
            self.token.literal,
            self.name.value,
            self.value
                .as_ref()
                .map(|v| v.to_string())
                .unwrap_or_default()
        )
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

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expression, Identifier, LetStatement, Program, Statement},
        token::{Token, TokenType},
    };

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                token: Token {
                    token_type: TokenType::Let,
                    literal: "let".to_string(),
                },
                name: Identifier {
                    value: "myVar".to_string(),
                    token: Token {
                        token_type: TokenType::Ident,
                        literal: "myVar".to_string(),
                    },
                },
                value: Some(Expression::Ident(Identifier {
                    token: Token {
                        token_type: TokenType::Ident,
                        literal: "anotherVar".to_string(),
                    },
                    value: "anotherVar".to_string(),
                })),
            })],
        };

        assert_eq!(
            "let myVar = anotherVar;",
            format!("{}", program),
            "program Display wrong. got={}",
            program
        )
    }
}
