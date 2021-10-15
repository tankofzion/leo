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

use crate::{ConstValue, Expression, ExpressionNode, FromAst, Node, PartialType, Scope, Type};
use leo_errors::{Result, Span};

use std::cell::Cell;

#[derive(Clone)]
pub struct ErrExpression<'a> {
    pub parent: Cell<Option<&'a Expression<'a>>>,
    pub span: Option<Span>,
}

impl<'a> Node for ErrExpression<'a> {
    fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
}

impl<'a> ExpressionNode<'a> for ErrExpression<'a> {
    fn set_parent(&self, parent: &'a Expression<'a>) {
        self.parent.replace(Some(parent));
    }

    fn get_parent(&self) -> Option<&'a Expression<'a>> {
        self.parent.get()
    }

    fn enforce_parents(&self, _: &'a Expression<'a>) {}

    fn get_type(&self) -> Option<Type<'a>> {
        Some(Type::Err)
    }

    fn is_mut_ref(&self) -> bool {
        // Speculative guess that this results in fewer knock-on errors.
        true
    }

    fn const_value(&self) -> Option<ConstValue> {
        Some(ConstValue::Err)
    }

    fn is_consty(&self) -> bool {
        // Speculative guess that this results in fewer knock-on errors.
        // We assume this because a runtime context will allow a const expression but not vice-versa.
        true
    }
}

impl<'a> FromAst<'a, leo_ast::ErrExpression> for ErrExpression<'a> {
    fn from_ast(_: &'a Scope<'a>, value: &leo_ast::ErrExpression, _: Option<PartialType<'a>>) -> Result<Self> {
        Ok(Self {
            parent: Cell::new(None),
            span: Some(value.span.clone()),
        })
    }
}

impl<'a> Into<leo_ast::ErrExpression> for &ErrExpression<'a> {
    fn into(self) -> leo_ast::ErrExpression {
        leo_ast::ErrExpression {
            span: self.span.clone().unwrap_or_default(),
        }
    }
}
