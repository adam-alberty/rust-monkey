use std::io::{self, Write};

use crate::{lexer::Lexer, parser::Parser};

/// Runs the REPL.
pub fn run() {
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        let len = io::stdin().read_line(&mut line).expect("expected line");
        if len == 1 {
            println!("bye!");
            break;
        }

        let mut parser = Parser::new(Lexer::new(line.clone()));

        let program = parser.parse_program();

        for error in parser.errors() {
            println!("\t{error}");
        }

        println!("{}", program);
        println!();
    }
}
