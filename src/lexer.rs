#[cfg(test)]
mod tests;

use crate::token::{Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    /// Constructs new lexer from the source.
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

    /// Gets next token from the source code.
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
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let token_type = lookup_ident(&literal);
                    return Token {
                        literal,
                        token_type,
                    };
                } else if is_digit(self.ch) {
                    let literal = self.read_number();
                    return Token {
                        token_type: TokenType::Int,
                        literal,
                    };
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

    /// Reads next character of source code.
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

    /// Peeks next character.
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

    /// Reads string identifier.
    fn read_identifier(&mut self) -> String {
        let position = self.position;

        while is_letter(char::from(self.ch)) {
            self.read_char();
        }

        String::from(&self.input[position..self.position])
    }

    /// Read number.
    fn read_number(&mut self) -> String {
        let position = self.position;

        while is_digit(self.ch) {
            self.read_char();
        }

        String::from(&self.input[position..self.position])
    }

    /// Skips whitespace.
    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }
}

/// Checks whether a character constitutes a letter in an identifier.
fn is_letter(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

/// Checks whether character constitutes a digit in a number.
fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

/// Gets token type based on keyword.
fn lookup_ident(ident: &str) -> TokenType {
    match ident {
        "fn" => TokenType::Function,
        "let" => TokenType::Let,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
        _ => TokenType::Ident,
    }
}
