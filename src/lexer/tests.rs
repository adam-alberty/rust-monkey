use crate::{lexer::Lexer, token::TokenType};

struct Test {
    expected_type: TokenType,
    expected_literal: &'static str,
}

#[test]
fn test_next_token() {
    let input = String::from(
        "
let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);

!-/*5;

5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
",
    );

    let tests = [
        Test {
            expected_type: TokenType::Let,
            expected_literal: "let",
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: "five",
        },
        Test {
            expected_type: TokenType::Assign,
            expected_literal: "=",
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: "5",
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: ";",
        },
        Test {
            expected_type: TokenType::Let,
            expected_literal: "let",
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: "ten",
        },
        Test {
            expected_type: TokenType::Assign,
            expected_literal: "=",
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: "10",
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: ";",
        },
        Test {
            expected_type: TokenType::Let,
            expected_literal: "let",
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: "add",
        },
        Test {
            expected_type: TokenType::Assign,
            expected_literal: "=",
        },
        Test {
            expected_type: TokenType::Function,
            expected_literal: "fn",
        },
        Test {
            expected_type: TokenType::Lparen,
            expected_literal: "(",
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: "x",
        },
        Test {
            expected_type: TokenType::Comma,
            expected_literal: ",",
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: "y",
        },
        Test {
            expected_type: TokenType::Rparen,
            expected_literal: ")",
        },
        Test {
            expected_type: TokenType::Lbrace,
            expected_literal: "{",
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: "x",
        },
        Test {
            expected_type: TokenType::Plus,
            expected_literal: "+",
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: "y",
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: ";",
        },
        Test {
            expected_type: TokenType::Rbrace,
            expected_literal: "}",
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: ";",
        },
        Test {
            expected_type: TokenType::Let,
            expected_literal: "let",
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: "result",
        },
        Test {
            expected_type: TokenType::Assign,
            expected_literal: "=",
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: "add",
        },
        Test {
            expected_type: TokenType::Lparen,
            expected_literal: "(",
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: "five",
        },
        Test {
            expected_type: TokenType::Comma,
            expected_literal: ",",
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: "ten",
        },
        Test {
            expected_type: TokenType::Rparen,
            expected_literal: ")",
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: ";",
        },
        Test {
            expected_type: TokenType::Bang,
            expected_literal: "!",
        },
        Test {
            expected_type: TokenType::Minus,
            expected_literal: "-",
        },
        Test {
            expected_type: TokenType::Slash,
            expected_literal: "/",
        },
        Test {
            expected_type: TokenType::Asterisk,
            expected_literal: "*",
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: "5",
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: ";",
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: "5",
        },
        Test {
            expected_type: TokenType::Lt,
            expected_literal: "<",
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: "10",
        },
        Test {
            expected_type: TokenType::Gt,
            expected_literal: ">",
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: "5",
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: ";",
        },
        Test {
            expected_type: TokenType::If,
            expected_literal: "if",
        },
        Test {
            expected_type: TokenType::Lparen,
            expected_literal: "(",
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: "5",
        },
        Test {
            expected_type: TokenType::Lt,
            expected_literal: "<",
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: "10",
        },
        Test {
            expected_type: TokenType::Rparen,
            expected_literal: ")",
        },
        Test {
            expected_type: TokenType::Lbrace,
            expected_literal: "{",
        },
        Test {
            expected_type: TokenType::Return,
            expected_literal: "return",
        },
        Test {
            expected_type: TokenType::True,
            expected_literal: "true",
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: ";",
        },
        Test {
            expected_type: TokenType::Rbrace,
            expected_literal: "}",
        },
        Test {
            expected_type: TokenType::Else,
            expected_literal: "else",
        },
        Test {
            expected_type: TokenType::Lbrace,
            expected_literal: "{",
        },
        Test {
            expected_type: TokenType::Return,
            expected_literal: "return",
        },
        Test {
            expected_type: TokenType::False,
            expected_literal: "false",
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: ";",
        },
        Test {
            expected_type: TokenType::Rbrace,
            expected_literal: "}",
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: "10",
        },
        Test {
            expected_type: TokenType::Eq,
            expected_literal: "==",
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: "10",
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: ";",
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: "10",
        },
        Test {
            expected_type: TokenType::NotEq,
            expected_literal: "!=",
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: "9",
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: ";",
        },
        Test {
            expected_type: TokenType::EOF,
            expected_literal: "",
        },
    ];

    let mut lexer = Lexer::new(input);

    for test in tests {
        let token = lexer.next_token();
        assert_eq!(
            test.expected_type, token.token_type,
            "token type wrong. {:?}",
            token
        );
        assert_eq!(
            test.expected_literal, token.literal,
            "literal wrong. {:?}",
            token
        );
    }
}
