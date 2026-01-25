use std::io::{Write, stdin, stdout};

use crate::{lexer::Lexer, token::TokenType};

pub fn start() {
    loop {
        let mut line = String::new();
        print!(">> ");
        stdout().flush().unwrap();
        let len = stdin().read_line(&mut line).expect("expected line");
        if len == 1 {
            println!("bye!");
            break;
        }

        let mut lexer = Lexer::new(line.clone());

        loop {
            let token = lexer.next_token();

            if token.token_type == TokenType::EOF {
                break;
            }

            print!("{:?}(\"{}\"), ", token.token_type, token.literal);
        }
        println!();
    }
}
