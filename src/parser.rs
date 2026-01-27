use crate::{
    ast,
    lexer::Lexer,
    token::{Token, TokenType},
};

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Parser {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        println!("cur {:?}", cur_token);
        println!("next {:?}", peek_token);

        Parser {
            lexer: lexer,
            cur_token: cur_token,
            peek_token: peek_token,
            errors: Vec::new(),
        }
    }

    fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program {
            statements: Vec::new(),
        };

        while !self.cur_token_is(TokenType::EOF) {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),

            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::Statement> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let name = ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        let let_statement = ast::LetStatement {
            token,
            name,
            value: None,
        };

        Some(ast::Statement::Let(let_statement))
    }

    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        let ret = ast::ReturnStatement {
            token: self.cur_token.clone(),
            return_value: None,
        };

        self.next_token();

        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(ast::Statement::Return(ret))
    }

    fn cur_token_is(&self, token_type: TokenType) -> bool {
        self.cur_token.token_type == token_type
    }

    fn peek_token_is(&self, token_type: TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(token_type.clone()) {
            self.next_token();
            true
        } else {
            self.peek_error(token_type);
            false
        }
    }

    fn peek_error(&mut self, token_type: TokenType) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            token_type, self.peek_token
        ));
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{self, Program},
        lexer::Lexer,
        parser::Parser,
    };

    struct Test {
        expected_identifier: &'static str,
    }

    #[test]
    fn test_let_statements() {
        let input = String::from(
            "
let x = 5;
let y = 10;
let foobar = 838383;
",
        );

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_number_of_statements(&program, 3);

        let tests = [
            Test {
                expected_identifier: "x",
            },
            Test {
                expected_identifier: "y",
            },
            Test {
                expected_identifier: "foobar",
            },
        ];

        for (i, test) in tests.iter().enumerate() {
            match &program.statements[i] {
                ast::Statement::Let(let_statement) => {
                    assert_eq!(let_statement.token.literal, "let");
                    assert_eq!(let_statement.name.value, test.expected_identifier);
                    assert_eq!(let_statement.name.token.literal, test.expected_identifier);
                }
                _ => panic!("not a let statement"),
            };
        }
    }

    #[test]
    fn test_return_statements() {
        let input = String::from(
            "
return 5;
return 10;
return 993322;
",
        );

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_number_of_statements(&program, 3);

        for statement in program.statements.iter() {
            match statement {
                ast::Statement::Return(return_statement) => {
                    assert_eq!(return_statement.token.literal, "return");
                }
                _ => panic!("not a return statement"),
            };
        }
    }

    fn check_number_of_statements(program: &Program, num_statements: usize) {
        assert!(
            program.statements.len() == num_statements,
            "program does not contain {} statements. got={}",
            num_statements,
            program.statements.len()
        );
    }

    fn check_parser_errors(parser: &Parser) {
        assert!(
            parser.errors().is_empty(),
            "parser has {} errors: {}",
            parser.errors().len(),
            parser.errors().join("\n")
        );
    }
}
