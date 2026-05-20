
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
