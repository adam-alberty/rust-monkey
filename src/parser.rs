use std::collections::HashMap;

use crate::{
    ast,
    lexer::Lexer,
    token::{Token, TokenType},
};

type PrefixParseFn = fn(&mut Parser) -> Option<ast::Expression>;
type InfixParseFn = fn(&mut Parser, ast::Expression) -> Option<ast::Expression>;

#[derive(Eq, PartialEq, Ord, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Parser {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        let mut parser = Parser {
            lexer: lexer,
            cur_token: cur_token,
            peek_token: peek_token,
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(TokenType::Ident, Parser::parse_identifier);

        parser
    }

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
    }

    fn parse_identifier(&mut self) -> Option<ast::Expression> {
        Some(ast::Expression::Ident(ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.clone().literal,
        }))
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
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, String> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
        let init_token = self.cur_token.clone();

        let stmt = ast::ExpressionStatement {
            token: init_token,
            expression: self.parse_expression(Precedence::Lowest),
        };

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        };

        Some(ast::Statement::Expression(stmt))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<ast::Expression> {
        if let Some(prefix_fn) = &self
            .prefix_parse_fns
            .get(&self.cur_token.token_type)
            .copied()
        {
            prefix_fn(self)
        } else {
            self.errors.push(format!(
                "no prefix parse function for {:?}",
                self.cur_token.token_type
            ));
            None
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

    #[test]
    fn test_identifier_expressions() {
        let input = String::from("foobar;");

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_number_of_statements(&program, 1);

        match &program.statements[0] {
            ast::Statement::Expression(expr_statement) => match &expr_statement.expression {
                ast::Expression::Ident(ident) => {
                    assert_eq!(
                        "foobar", ident.value,
                        "ident value not `foobar`. got={}",
                        ident.value
                    );
                    assert_eq!(
                        "foobar", ident.token.literal,
                        "ident token literal not `foobar`. got={}",
                        ident.token.literal
                    );
                }
                _ => panic!("expression not an identifier"),
            },
            _ => panic!("not an expression statement"),
        };
    }
}
