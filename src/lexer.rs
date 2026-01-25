use crate::token::{Token, TokenType, lookup_ident};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    // Construct new lexer that takes in the source code
    pub fn new(input: String) -> Self {
        let mut l = Self {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        l.read_char();
        l
    }

    // Read next character of source code into `ch`
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position..]
                .chars()
                .next()
                .expect("valid unicode character");
        }
        self.position = self.read_position;
        self.read_position += self.ch.len_utf8();
    }

    // Peek next character
    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position..]
                .chars()
                .next()
                .expect("valid unicode character")
        }
    }

    // Reads string identifier
    fn read_identifier(&mut self) -> String {
        let position = self.position;

        while is_letter(char::from(self.ch)) {
            self.read_char();
        }

        String::from(&self.input[position..self.position])
    }

    fn read_number(&mut self) -> String {
        let position = self.position;

        while is_digit(self.ch) {
            self.read_char();
        }

        String::from(&self.input[position..self.position])
    }

    // Skips whitespace
    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }

    // Get next token from the source code
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    let ch = self.ch;
                    self.read_char();

                    Token {
                        token_type: TokenType::Eq,
                        literal: ch.to_string() + &self.ch.to_string(),
                    }
                } else {
                    Token {
                        token_type: TokenType::Assign,
                        literal: self.ch.to_string(),
                    }
                }
            }

            '+' => Token {
                token_type: TokenType::Plus,
                literal: self.ch.to_string(),
            },

            '-' => Token {
                token_type: TokenType::Minus,
                literal: self.ch.to_string(),
            },

            '!' => {
                if self.peek_char() == '=' {
                    let ch = self.ch;
                    self.read_char();

                    Token {
                        token_type: TokenType::NotEq,
                        literal: ch.to_string() + &self.ch.to_string(),
                    }
                } else {
                    Token {
                        token_type: TokenType::Bang,
                        literal: self.ch.to_string(),
                    }
                }
            }

            '/' => Token {
                token_type: TokenType::Slash,
                literal: self.ch.to_string(),
            },

            '*' => Token {
                token_type: TokenType::Asterisk,
                literal: self.ch.to_string(),
            },

            '<' => Token {
                token_type: TokenType::Lt,
                literal: self.ch.to_string(),
            },

            '>' => Token {
                token_type: TokenType::Gt,
                literal: self.ch.to_string(),
            },

            ';' => Token {
                token_type: TokenType::Semicolon,
                literal: self.ch.to_string(),
            },

            ',' => Token {
                token_type: TokenType::Comma,
                literal: self.ch.to_string(),
            },

            '(' => Token {
                token_type: TokenType::Lparen,
                literal: self.ch.to_string(),
            },

            ')' => Token {
                token_type: TokenType::Rparen,
                literal: self.ch.to_string(),
            },

            '{' => Token {
                token_type: TokenType::Lbrace,
                literal: self.ch.to_string(),
            },

            '}' => Token {
                token_type: TokenType::Rbrace,
                literal: self.ch.to_string(),
            },

            '\0' => Token {
                token_type: TokenType::EOF,
                literal: String::from(""),
            },

            _ => {
                // Letter
                if is_letter(char::from(self.ch)) {
                    let literal = self.read_identifier();
                    let token_type = lookup_ident(&literal);
                    return Token {
                        literal,
                        token_type,
                    };

                // Digit
                } else if is_digit(char::from(self.ch)) {
                    let literal = self.read_number();
                    return Token {
                        token_type: TokenType::Int,
                        literal,
                    };

                // Illegal
                } else {
                    Token {
                        token_type: TokenType::Illegal,
                        literal: self.ch.to_string(),
                    }
                }
            }
        };
        self.read_char();
        token
    }
}

fn is_letter(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

#[test]
fn test_next_token() {
    let input = String::from(
        "let five = 5;
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

    struct Test {
        expected_type: TokenType,
        expected_literal: String,
    }

    let tests = [
        Test {
            expected_type: TokenType::Let,
            expected_literal: String::from("let"),
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: String::from("five"),
        },
        Test {
            expected_type: TokenType::Assign,
            expected_literal: String::from("="),
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: String::from("5"),
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: String::from(";"),
        },
        Test {
            expected_type: TokenType::Let,
            expected_literal: String::from("let"),
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: String::from("ten"),
        },
        Test {
            expected_type: TokenType::Assign,
            expected_literal: String::from("="),
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: String::from("10"),
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: String::from(";"),
        },
        Test {
            expected_type: TokenType::Let,
            expected_literal: String::from("let"),
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: String::from("add"),
        },
        Test {
            expected_type: TokenType::Assign,
            expected_literal: String::from("="),
        },
        Test {
            expected_type: TokenType::Function,
            expected_literal: String::from("fn"),
        },
        Test {
            expected_type: TokenType::Lparen,
            expected_literal: String::from("("),
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: String::from("x"),
        },
        Test {
            expected_type: TokenType::Comma,
            expected_literal: String::from(","),
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: String::from("y"),
        },
        Test {
            expected_type: TokenType::Rparen,
            expected_literal: String::from(")"),
        },
        Test {
            expected_type: TokenType::Lbrace,
            expected_literal: String::from("{"),
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: String::from("x"),
        },
        Test {
            expected_type: TokenType::Plus,
            expected_literal: String::from("+"),
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: String::from("y"),
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: String::from(";"),
        },
        Test {
            expected_type: TokenType::Rbrace,
            expected_literal: String::from("}"),
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: String::from(";"),
        },
        Test {
            expected_type: TokenType::Let,
            expected_literal: String::from("let"),
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: String::from("result"),
        },
        Test {
            expected_type: TokenType::Assign,
            expected_literal: String::from("="),
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: String::from("add"),
        },
        Test {
            expected_type: TokenType::Lparen,
            expected_literal: String::from("("),
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: String::from("five"),
        },
        Test {
            expected_type: TokenType::Comma,
            expected_literal: String::from(","),
        },
        Test {
            expected_type: TokenType::Ident,
            expected_literal: String::from("ten"),
        },
        Test {
            expected_type: TokenType::Rparen,
            expected_literal: String::from(")"),
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: String::from(";"),
        },
        Test {
            expected_type: TokenType::Bang,
            expected_literal: String::from("!"),
        },
        Test {
            expected_type: TokenType::Minus,
            expected_literal: String::from("-"),
        },
        Test {
            expected_type: TokenType::Slash,
            expected_literal: String::from("/"),
        },
        Test {
            expected_type: TokenType::Asterisk,
            expected_literal: String::from("*"),
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: String::from("5"),
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: String::from(";"),
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: String::from("5"),
        },
        Test {
            expected_type: TokenType::Lt,
            expected_literal: String::from("<"),
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: String::from("10"),
        },
        Test {
            expected_type: TokenType::Gt,
            expected_literal: String::from(">"),
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: String::from("5"),
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: String::from(";"),
        },
        Test {
            expected_type: TokenType::If,
            expected_literal: String::from("if"),
        },
        Test {
            expected_type: TokenType::Lparen,
            expected_literal: String::from("("),
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: String::from("5"),
        },
        Test {
            expected_type: TokenType::Lt,
            expected_literal: String::from("<"),
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: String::from("10"),
        },
        Test {
            expected_type: TokenType::Rparen,
            expected_literal: String::from(")"),
        },
        Test {
            expected_type: TokenType::Lbrace,
            expected_literal: String::from("{"),
        },
        Test {
            expected_type: TokenType::Return,
            expected_literal: String::from("return"),
        },
        Test {
            expected_type: TokenType::True,
            expected_literal: String::from("true"),
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: String::from(";"),
        },
        Test {
            expected_type: TokenType::Rbrace,
            expected_literal: String::from("}"),
        },
        Test {
            expected_type: TokenType::Else,
            expected_literal: String::from("else"),
        },
        Test {
            expected_type: TokenType::Lbrace,
            expected_literal: String::from("{"),
        },
        Test {
            expected_type: TokenType::Return,
            expected_literal: String::from("return"),
        },
        Test {
            expected_type: TokenType::False,
            expected_literal: String::from("false"),
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: String::from(";"),
        },
        Test {
            expected_type: TokenType::Rbrace,
            expected_literal: String::from("}"),
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: String::from("10"),
        },
        Test {
            expected_type: TokenType::Eq,
            expected_literal: String::from("=="),
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: String::from("10"),
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: String::from(";"),
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: String::from("10"),
        },
        Test {
            expected_type: TokenType::NotEq,
            expected_literal: String::from("!="),
        },
        Test {
            expected_type: TokenType::Int,
            expected_literal: String::from("9"),
        },
        Test {
            expected_type: TokenType::Semicolon,
            expected_literal: String::from(";"),
        },
        Test {
            expected_type: TokenType::EOF,
            expected_literal: String::from(""),
        },
    ];

    let mut lexer = Lexer::new(input);

    for test in tests.into_iter() {
        let token = lexer.next_token();
        assert!(
            token.token_type == test.expected_type,
            "token type wrong. expected={:?}, got={:?}",
            test.expected_type,
            token.token_type
        );
        assert!(
            token.literal == test.expected_literal,
            "type correct, but literal wrong. expected={:?}, got={:?}",
            test.expected_literal,
            token.literal
        );
    }
}
