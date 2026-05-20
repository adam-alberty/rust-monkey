use std::fmt;

use crate::token::Token;

pub enum Expression {
    Ident(Identifier),
    IntegerLiteral(IntegerLiteral),
    BooleanLiteral(BooleanLiteral),
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
            Expression::BooleanLiteral(bool) => {
                write!(f, "{}", bool.value)
            }
        }
    }
}

/////////////////
// Expressions //
/////////////////

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
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
