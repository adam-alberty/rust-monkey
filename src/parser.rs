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

fn token_precedence(token_type: &TokenType) -> Precedence {
    match token_type {
        TokenType::Eq | TokenType::NotEq => Precedence::Equals,
        TokenType::Lt | TokenType::Gt => Precedence::LessGreater,
        TokenType::Plus | TokenType::Minus => Precedence::Sum,
        TokenType::Slash | TokenType::Asterisk => Precedence::Product,
        _ => Precedence::Lowest,
    }
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
        parser.register_prefix(TokenType::Int, Parser::parse_integer_literal);

        parser.register_prefix(TokenType::Bang, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::Minus, Parser::parse_prefix_expression);

        parser.register_prefix(TokenType::True, Parser::parse_boolean);
        parser.register_prefix(TokenType::False, Parser::parse_boolean);

        parser.register_infix(TokenType::Plus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Minus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Slash, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Asterisk, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Eq, Parser::parse_infix_expression);
        parser.register_infix(TokenType::NotEq, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Lt, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Gt, Parser::parse_infix_expression);

        parser
    }

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
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

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
        let stmt = ast::ExpressionStatement {
            token: self.cur_token.clone(),
            expression: self.parse_expression(Precedence::Lowest)?,
        };

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        };

        Some(ast::Statement::Expression(stmt))
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
            value: None, // TODO remove dummy expression
        };

        Some(ast::Statement::Let(let_statement))
    }

    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        let ret = ast::ReturnStatement {
            token: self.cur_token.clone(),
            return_value: None, // TODO - remove Dummy expression
        };

        self.next_token();

        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(ast::Statement::Return(ret))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<ast::Expression> {
        let prefix_fn = self
            .prefix_parse_fns
            .get(&self.cur_token.token_type)
            .copied();

        let mut left_exp = match prefix_fn {
            Some(prefix) => prefix(self)?,
            None => {
                self.no_prefix_parse_fn_error(self.cur_token.token_type.clone());
                return None;
            }
        };

        while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
            let infix_fn = self
                .infix_parse_fns
                .get(&self.peek_token.token_type)
                .copied();

            let infix = match infix_fn {
                Some(infix) => infix,
                None => return Some(left_exp),
            };

            self.next_token();

            left_exp = infix(self, left_exp)?;
        }

        Some(left_exp)
    }

    fn parse_identifier(&mut self) -> Option<ast::Expression> {
        Some(ast::Expression::Ident(ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.clone().literal,
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<ast::Expression> {
        if let Ok(value) = self.cur_token.literal.parse() {
            Some(ast::Expression::IntegerLiteral(ast::IntegerLiteral {
                token: self.cur_token.clone(),
                value,
            }))
        } else {
            self.errors.push(format!(
                "could not parse {:?} as integer",
                self.cur_token.literal
            ));
            None
        }
    }

    fn parse_boolean(&mut self) -> Option<ast::Expression> {
        Some(ast::Expression::Boolean(ast::Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token_is(TokenType::True),
        }))
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::Expression> {
        let token = self.cur_token.clone();

        self.next_token();

        let right = match self.parse_expression(Precedence::Prefix) {
            Some(expr) => expr,
            None => return None, // propagate parse failure
        };

        Some(ast::Expression::Prefix(ast::Prefix {
            token: token.clone(),
            operator: token.literal,
            right: Box::new(right),
        }))
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> Option<ast::Expression> {
        let token = self.cur_token.clone();
        let precedence = self.cur_precedence();

        self.next_token();
        let right = self.parse_expression(precedence)?;

        Some(ast::Expression::Infix(ast::Infix {
            token: token.clone(),
            left: Box::new(left),
            operator: token.literal,
            right: Box::new(right),
        }))
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

    // Errors

    fn peek_error(&mut self, token_type: TokenType) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            token_type, self.peek_token
        ));
    }

    fn no_prefix_parse_fn_error(&mut self, token_type: TokenType) {
        self.errors.push(format!(
            "no prefix parse function for {:?} found",
            token_type
        ));
    }

    fn peek_precedence(&self) -> Precedence {
        token_precedence(&self.peek_token.token_type)
    }

    fn cur_precedence(&self) -> Precedence {
        token_precedence(&self.cur_token.token_type)
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
    fn test_identifier_expression() {
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

    #[test]
    fn test_integer_literal_expression() {
        let input = String::from("5;");

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_number_of_statements(&program, 1);

        match &program.statements[0] {
            ast::Statement::Expression(expr_statement) => match &expr_statement.expression {
                ast::Expression::IntegerLiteral(ident) => {
                    assert_eq!(5, ident.value, "literal value not 5. got={}", ident.value);
                    assert_eq!(
                        "5", ident.token.literal,
                        "literal value not `5`. got={}",
                        ident.token.literal
                    );
                }
                _ => panic!("expression not an identifier"),
            },
            _ => panic!("not an expression statement"),
        };
    }

    #[test]
    fn test_boolean_literal_expression() {
        let input = String::from("true;");

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_number_of_statements(&program, 1);

        match &program.statements[0] {
            ast::Statement::Expression(expr_statement) => match &expr_statement.expression {
                ast::Expression::Boolean(bool) => {
                    assert_eq!(true, bool.value, "boolean value not true");
                    assert_eq!(
                        "true", bool.token.literal,
                        "literal value not `true`. got={}",
                        bool.token.literal
                    );
                }
                _ => panic!("expression not a boolean"),
            },
            _ => panic!("not an expression statement"),
        };
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct PrefixTest {
            input: &'static str,
            operator: &'static str,
            integer_value: i64,
        }

        let prefix_tests = [
            PrefixTest {
                input: "!5;",
                operator: "!",
                integer_value: 5,
            },
            PrefixTest {
                input: "-15;",
                operator: "-",
                integer_value: 15,
            },
        ];

        for test in prefix_tests {
            let mut parser = Parser::new(Lexer::new(test.input.to_string()));
            let program = parser.parse_program();

            check_parser_errors(&parser);
            check_number_of_statements(&program, 1);

            match &program.statements[0] {
                ast::Statement::Expression(expr_statement) => match &expr_statement.expression {
                    ast::Expression::Prefix(prefix) => {
                        assert_eq!(
                            test.operator, prefix.operator,
                            "expression operator is not '{}'. got={}",
                            test.operator, prefix.operator
                        );
                        test_integer_literal(&prefix.right, test.integer_value);
                    }
                    _ => panic!("not a prefix expression"),
                },
                _ => panic!("not an expression statement"),
            };
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        struct InfixTest {
            input: &'static str,
            left_value: i64,
            operator: &'static str,
            right_value: i64,
        }

        let infix_tests = [
            InfixTest {
                input: "5 + 10;",
                left_value: 5,
                operator: "+",
                right_value: 10,
            },
            InfixTest {
                input: "5 - 5;",
                left_value: 5,
                operator: "-",
                right_value: 5,
            },
            InfixTest {
                input: "5 * 5;",
                left_value: 5,
                operator: "*",
                right_value: 5,
            },
            InfixTest {
                input: "5 / 5;",
                left_value: 5,
                operator: "/",
                right_value: 5,
            },
            InfixTest {
                input: "5 > 5;",
                left_value: 5,
                operator: ">",
                right_value: 5,
            },
            InfixTest {
                input: "5 < 5;",
                left_value: 5,
                operator: "<",
                right_value: 5,
            },
            InfixTest {
                input: "5 == 5;",
                left_value: 5,
                operator: "==",
                right_value: 5,
            },
            InfixTest {
                input: "5 != 5;",
                left_value: 5,
                operator: "!=",
                right_value: 5,
            },
        ];

        for test in infix_tests {
            let mut parser = Parser::new(Lexer::new(test.input.to_string()));
            let program = parser.parse_program();

            check_parser_errors(&parser);
            check_number_of_statements(&program, 1);

            match &program.statements[0] {
                ast::Statement::Expression(expr_statement) => match &expr_statement.expression {
                    ast::Expression::Infix(infix) => {
                        assert_eq!(
                            test.operator, infix.operator,
                            "expression operator is not '{}'. got={}",
                            test.operator, infix.operator
                        );
                        test_integer_literal(&infix.left, test.left_value);
                        test_integer_literal(&infix.right, test.right_value);
                    }
                    _ => panic!("not an infix expression"),
                },
                _ => panic!("not an expression statement"),
            };
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        struct Test {
            input: &'static str,
            expected: &'static str,
        }

        let tests = [
            Test {
                input: "-a * b",
                expected: "((-a) * b)",
            },
            Test {
                input: "!-a",
                expected: "(!(-a))",
            },
            Test {
                input: "a + b + c",
                expected: "((a + b) + c)",
            },
            Test {
                input: "a + b - c",
                expected: "((a + b) - c)",
            },
            Test {
                input: "a * b * c",
                expected: "((a * b) * c)",
            },
            Test {
                input: "a * b / c",
                expected: "((a * b) / c)",
            },
            Test {
                input: "a + b / c",
                expected: "(a + (b / c))",
            },
            Test {
                input: "a + b * c + d / e - f",
                expected: "(((a + (b * c)) + (d / e)) - f)",
            },
            Test {
                input: "3 + 4; -5 * 5",
                expected: "(3 + 4)((-5) * 5)",
            },
            Test {
                input: "5 > 4 == 3 < 4",
                expected: "((5 > 4) == (3 < 4))",
            },
            Test {
                input: "5 < 4 != 3 > 4",
                expected: "((5 < 4) != (3 > 4))",
            },
            Test {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            },
            Test {
                input: "true",
                expected: "true",
            },
            Test {
                input: "false",
                expected: "false",
            },
            Test {
                input: "3 > 5 == false",
                expected: "((3 > 5) == false)",
            },
            Test {
                input: "3 < 5 == true",
                expected: "((3 < 5) == true)",
            },
            Test {
                input: "1 + (2 + 3) + 4",
                expected: "((1 + (2 + 3)) + 4)",
            },
            Test {
                input: "(5 + 5) * 2",
                expected: "((5 + 5) * 2)",
            },
            Test {
                input: "2 / (5 + 5)",
                expected: "(2 / (5 + 5))",
            },
            Test {
                input: "-(5 + 5)",
                expected: "(-(5 + 5))",
            },
            Test {
                input: "!(true == true)",
                expected: "(!(true == true))",
            },
        ];

        for test in tests {
            let mut parser = Parser::new(Lexer::new(test.input.to_string()));
            let program = parser.parse_program();

            check_parser_errors(&parser);
            assert_eq!(test.expected, program.to_string());
        }
    }

    fn test_identifier(expr: &ast::Expression, value: String) {
        match expr {
            ast::Expression::Ident(ident) => {
                assert_eq!(value, ident.value);
                assert_eq!(value, ident.token.literal);
            }
            _ => panic!("not an identifier. got={}", expr),
        }
    }

    fn test_integer_literal(expr: &ast::Expression, value: i64) {
        match expr {
            ast::Expression::IntegerLiteral(integer_literal) => {
                assert_eq!(value, integer_literal.value);
            }
            _ => panic!("not an integer literal. got={}", expr),
        }
    }
}
