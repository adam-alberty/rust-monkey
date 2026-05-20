use std::fmt::{self};

use crate::{ast::BlockStatement, token::Token};

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Ident(Identifier),
    IntegerLiteral(IntegerLiteral),
    BooleanLiteral(BooleanLiteral),
    FunctionLiteral(FunctionLiteral),
    If(IfExpression),
    Call(CallExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Ident(ident) => write!(f, "{ident}"),
            Expression::IntegerLiteral(int_literal) => write!(f, "{int_literal}"),
            Expression::BooleanLiteral(bool_literal) => write!(f, "{bool_literal}"),
            Expression::FunctionLiteral(fn_literal) => write!(f, "{fn_literal}"),
            Expression::If(if_expression) => write!(f, "{if_expression}"),
            Expression::Call(call_expression) => write!(f, "{call_expression}"),
            Expression::Prefix(prefix_expression) => write!(f, "{prefix_expression}"),
            Expression::Infix(infix_expression) => write!(f, "{infix_expression}"),
        }
    }
}

/////////////////
// Expressions //
/////////////////

#[derive(Debug, PartialEq, Eq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl fmt::Display for BooleanLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({}) {}",
            self.token.literal,
            self.parameters
                .iter()
                .map(|ident| { ident.to_string() })
                .collect::<Vec<_>>()
                .join(", "),
            self.body
        )?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "if {} {}", self.condition, self.consequence)?;

        if let Some(alt) = &self.alternative {
            write!(f, "else {}", alt)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.function,
            self.arguments
                .iter()
                .map(|expr| { expr.to_string() })
                .collect::<Vec<_>>()
                .join(", "),
        )?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}
