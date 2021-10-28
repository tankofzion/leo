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

use crate::{BlockStatement, Expression, FromAst, Node, PartialType, Scope, Statement, Type};
use leo_errors::{Result, Span};

use serde::Serialize;
use std::cell::Cell;

#[derive(Clone, Serialize)]
pub struct ConditionalStatement<'a> {
    pub parent: Cell<Option<&'a Statement<'a>>>,
    pub span: Option<Span>,
    pub condition: Cell<&'a Expression<'a>>,
    pub result: Cell<&'a Statement<'a>>,
    pub next: Cell<Option<&'a Statement<'a>>>,
}

impl<'a> Node for ConditionalStatement<'a> {
    fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
}

impl<'a> FromAst<'a, leo_ast::ConditionalStatement> for ConditionalStatement<'a> {
    fn from_ast(
        scope: &'a Scope<'a>,
        statement: &leo_ast::ConditionalStatement,
        _expected_type: Option<PartialType<'a>>,
    ) -> Result<Self> {
        let condition = <&Expression<'a>>::from_ast(scope, &statement.condition, Some(Type::Boolean.into()))?;
        let result = scope.context.alloc_statement(Statement::Block(BlockStatement::from_ast(
            scope,
            &statement.block,
            None,
        )?));
        let next = statement
            .next
            .as_deref()
            .map(|next| -> Result<&'a Statement<'a>> { <&'a Statement<'a>>::from_ast(scope, next, None) })
            .transpose()?;

        Ok(ConditionalStatement {
            parent: Cell::new(None),
            span: Some(statement.span.clone()),
            condition: Cell::new(condition),
            result: Cell::new(result),
            next: Cell::new(next),
        })
    }
}

impl<'a> Into<leo_ast::ConditionalStatement> for &ConditionalStatement<'a> {
    fn into(self) -> leo_ast::ConditionalStatement {
        leo_ast::ConditionalStatement {
            condition: self.condition.get().into(),
            block: match self.result.get() {
                Statement::Block(block) => block.into(),
                _ => unimplemented!(),
            },
            next: self.next.get().map(|e| Box::new(e.into())),
            span: self.span.clone().unwrap_or_default(),
        }
    }
}
