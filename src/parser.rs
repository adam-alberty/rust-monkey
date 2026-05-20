#[cfg(test)]
mod tests;

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

        parser.register_prefix(TokenType::Lparen, Parser::parse_grouped_expression);

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
        Some(ast::Expression::BooleanLiteral(ast::BooleanLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token_is(TokenType::True),
        }))
    }

    fn parse_grouped_expression(&mut self) -> Option<ast::Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::Rparen) {
            None
        } else {
            exp
        }
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
