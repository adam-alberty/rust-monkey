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
            ast::Expression::BooleanLiteral(bool) => {
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

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";

    let mut parser = Parser::new(Lexer::new(input.to_string()));
    let program = parser.parse_program();

    println!("THIS IS THE PROGRAM>>>>> {}", program);

    check_parser_errors(&parser);
    check_number_of_statements(&program, 1);

    match &program.statements[0] {
        ast::Statement::Expression(expr_statement) => match &expr_statement.expression {
            ast::Expression::If(if_expression) => {
                assert_eq!(if_expression.consequence.statements.len(), 1);
                assert!(if_expression.alternative.is_none());

                test_infix_expression(
                    &if_expression.condition,
                    ExpectedValue::Ident("x"),
                    "<",
                    ExpectedValue::Ident("y"),
                );

                match &if_expression.consequence.statements[0] {
                    ast::Statement::Expression(expression_statement) => {
                        test_identifier(&expression_statement.expression, "x");
                    }
                    _ => panic!("not an expression statement"),
                }
            }
            _ => panic!("not an if expression"),
        },
        _ => panic!("not an expression statement"),
    };
}

fn test_identifier(expr: &ast::Expression, value: &str) {
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

enum ExpectedValue {
    Int(i64),
    Ident(&'static str),
}

fn test_literal_expression(expr: &ast::Expression, expected: ExpectedValue) {
    match expected {
        ExpectedValue::Int(v) => {
            test_integer_literal(expr, v);
        }
        ExpectedValue::Ident(v) => {
            test_identifier(expr, v);
        }
    }
}

fn test_infix_expression(
    expr: &ast::Expression,
    left: ExpectedValue,
    operator: &str,
    right: ExpectedValue,
) {
    match expr {
        ast::Expression::Infix(infix) => {
            test_literal_expression(&infix.left, left);
            assert_eq!(infix.operator, operator);
            test_literal_expression(&infix.right, right);
        }
        _ => panic!("exp is not ast::Expression::Infix. got={expr:?}"),
    }
}
