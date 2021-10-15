// Copyright (C) 2019-2021 Aleo Systems Inc.
// This file is part of the Leo library.

// The Leo library is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// The Leo library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with the Leo library. If not, see <https://www.gnu.org/licenses/>.

use crate::{tokenizer, ParserContext, SpannedToken};
use leo_ast::{Expression, ExpressionStatement, Statement, ValueExpression};
use leo_errors::{emitter::Handler, LeoError, Span};
use leo_test_framework::{
    runner::{Namespace, ParseType, Runner},
    Test,
};
use serde_yaml::Value;
use tokenizer::Token;

struct TokenNamespace;

impl Namespace for TokenNamespace {
    fn parse_type(&self) -> ParseType {
        ParseType::Line
    }

    fn run_test(&self, test: Test) -> Result<Value, String> {
        let output = tokenizer::tokenize("test", test.content.into());
        output
            .map(|tokens| {
                Value::String(
                    tokens
                        .into_iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>()
                        .join(","),
                )
            })
            .map_err(|x| x.to_string())
    }
}

fn not_fully_consumed(tokens: &mut ParserContext) -> Result<(), String> {
    if !tokens.has_next() {
        return Ok(());
    }
    let mut out = "did not consume all input: ".to_string();
    while tokens.has_next() {
        out.push_str(&tokens.expect_any().unwrap().to_string());
        out.push('\n');
    }
    Err(out)
}

fn with_handler<T>(
    tokens: Vec<SpannedToken>,
    logic: impl FnOnce(&mut ParserContext<'_>) -> Result<T, LeoError>,
) -> Result<T, String> {
    let (handler, buf) = Handler::new_with_buf();
    let mut tokens = ParserContext::new(&handler, tokens);
    let parsed = handler
        .extend_if_error(logic(&mut tokens))
        .map_err(|_| buf.extract().to_string())?;
    not_fully_consumed(&mut tokens)?;
    Ok(parsed)
}

fn implcit_value_expr() -> Expression {
    Expression::Value(ValueExpression::Implicit("".into(), Span::default()))
}

struct ParseExpressionNamespace;

impl Namespace for ParseExpressionNamespace {
    fn parse_type(&self) -> ParseType {
        ParseType::Line
    }

    fn run_test(&self, test: Test) -> Result<Value, String> {
        let tokenizer = tokenizer::tokenize("test", test.content.into()).map_err(|x| x.to_string())?;
        if tokenizer
            .iter()
            .all(|x| matches!(x.token, Token::CommentLine(_) | Token::CommentBlock(_)))
        {
            return Ok(serde_yaml::to_value(&implcit_value_expr()).expect("serialization failed"));
        }
        let expr = with_handler(tokenizer, |p| p.parse_expression())?;
        Ok(serde_yaml::to_value(&expr).expect("serialization failed"))
    }
}

struct ParseStatementNamespace;

impl Namespace for ParseStatementNamespace {
    fn parse_type(&self) -> ParseType {
        ParseType::ContinuousLines
    }

    fn run_test(&self, test: Test) -> Result<Value, String> {
        let tokenizer = tokenizer::tokenize("test", test.content.into()).map_err(|x| x.to_string())?;
        if tokenizer
            .iter()
            .all(|x| matches!(x.token, Token::CommentLine(_) | Token::CommentBlock(_)))
        {
            return Ok(serde_yaml::to_value(&Statement::Expression(ExpressionStatement {
                expression: implcit_value_expr(),
                span: Span::default(),
            }))
            .expect("serialization failed"));
        }
        let stmt = with_handler(tokenizer, |p| p.parse_statement())?;
        Ok(serde_yaml::to_value(&stmt).expect("serialization failed"))
    }
}

struct ParseNamespace;

impl Namespace for ParseNamespace {
    fn parse_type(&self) -> ParseType {
        ParseType::Whole
    }

    fn run_test(&self, test: Test) -> Result<Value, String> {
        let tokenizer = tokenizer::tokenize("test", test.content.into()).map_err(|x| x.to_string())?;
        let program = with_handler(tokenizer, |p| p.parse_program())?;
        Ok(serde_yaml::to_value(&program).expect("serialization failed"))
    }
}

struct TestRunner;

impl Runner for TestRunner {
    fn resolve_namespace(&self, name: &str) -> Option<Box<dyn Namespace>> {
        Some(match name {
            "Parse" => Box::new(ParseNamespace),
            "ParseStatement" => Box::new(ParseStatementNamespace),
            "ParseExpression" => Box::new(ParseExpressionNamespace),
            "Token" => Box::new(TokenNamespace),
            _ => return None,
        })
    }
}

#[test]
pub fn parser_tests() {
    leo_test_framework::run_tests(&TestRunner, "parser");
}
