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

use crate::{
    CharValue, ConstInt, ConstValue, Expression, ExpressionNode, FromAst, GroupValue, Node, PartialType, Scope, Type,
};

use leo_errors::{AsgError, Result, Span};
use std::convert::TryInto;

use std::cell::Cell;

#[derive(Clone)]
pub struct Constant<'a> {
    pub id: u32,
    pub parent: Cell<Option<&'a Expression<'a>>>,
    pub span: Option<Span>,
    pub value: ConstValue<'a>, // should not be compound constants
}

impl<'a> Node for Constant<'a> {
    fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
}

impl<'a> ExpressionNode<'a> for Constant<'a> {
    fn set_parent(&self, parent: &'a Expression<'a>) {
        self.parent.replace(Some(parent));
    }

    fn get_parent(&self) -> Option<&'a Expression<'a>> {
        self.parent.get()
    }

    fn enforce_parents(&self, _expr: &'a Expression<'a>) {}

    fn get_type(&'a self) -> Option<Type<'a>> {
        self.value.get_type()
    }

    fn is_mut_ref(&self) -> bool {
        false
    }

    fn const_value(&self) -> Option<ConstValue<'a>> {
        Some(self.value.clone())
    }

    fn is_consty(&self) -> bool {
        true
    }
}

impl<'a> FromAst<'a, leo_ast::ValueExpression> for Constant<'a> {
    fn from_ast(
        scope: &'a Scope<'a>,
        value: &leo_ast::ValueExpression,
        expected_type: Option<PartialType<'a>>,
    ) -> Result<Constant<'a>> {
        use leo_ast::ValueExpression::*;
        Ok(match value {
            Address(value, span) => {
                match expected_type.map(PartialType::full).flatten() {
                    Some(Type::Address) | None => (),
                    Some(x) => {
                        return Err(AsgError::unexpected_type(x, Type::Address, span).into());
                    }
                }
                Constant {
                    id: scope.context.get_id(),
                    parent: Cell::new(None),
                    span: Some(span.clone()),
                    value: ConstValue::Address(value.clone()),
                }
            }
            Boolean(value, span) => {
                match expected_type.map(PartialType::full).flatten() {
                    Some(Type::Boolean) | None => (),
                    Some(x) => {
                        return Err(AsgError::unexpected_type(x, Type::Boolean, span).into());
                    }
                }
                Constant {
                    id: scope.context.get_id(),
                    parent: Cell::new(None),
                    span: Some(span.clone()),
                    value: ConstValue::Boolean(
                        value
                            .parse::<bool>()
                            .map_err(|_| AsgError::invalid_boolean(value, span))?,
                    ),
                }
            }
            Char(value) => {
                match expected_type.map(PartialType::full).flatten() {
                    Some(Type::Char) | None => (),
                    Some(x) => {
                        return Err(AsgError::unexpected_type(x, Type::Char, value.span()).into());
                    }
                }

                Constant {
                    id: scope.context.get_id(),
                    parent: Cell::new(None),
                    span: Some(value.span().clone()),
                    value: ConstValue::Char(CharValue::from(value.clone())),
                }
            }
            Field(value, span) => {
                match expected_type.map(PartialType::full).flatten() {
                    Some(Type::Field) | None => (),
                    Some(x) => {
                        return Err(AsgError::unexpected_type(x, Type::Field, span).into());
                    }
                }
                Constant {
                    id: scope.context.get_id(),
                    parent: Cell::new(None),
                    span: Some(span.clone()),
                    value: ConstValue::Field(value.parse().map_err(|_| AsgError::invalid_int(value, span))?),
                }
            }
            Group(value) => {
                match expected_type.map(PartialType::full).flatten() {
                    Some(Type::Group) | None => (),
                    Some(x) => {
                        return Err(AsgError::unexpected_type(x, Type::Group, value.span()).into());
                    }
                }
                Constant {
                    id: scope.context.get_id(),
                    parent: Cell::new(None),
                    span: Some(value.span().clone()),
                    value: ConstValue::Group((&**value).clone().try_into()?),
                }
            }
            Implicit(value, span) => match expected_type {
                None => return Err(AsgError::unresolved_type("unknown", span).into()),
                Some(PartialType::Integer(Some(sub_type), _)) | Some(PartialType::Integer(None, Some(sub_type))) => {
                    Constant {
                        id: scope.context.get_id(),
                        parent: Cell::new(None),
                        span: Some(span.clone()),
                        value: ConstValue::Int(ConstInt::parse(&sub_type, value, span)?),
                    }
                }
                Some(PartialType::Type(Type::Field)) => Constant {
                    id: scope.context.get_id(),
                    parent: Cell::new(None),
                    span: Some(span.clone()),
                    value: ConstValue::Field(value.parse().map_err(|_| AsgError::invalid_int(value, span))?),
                },
                Some(PartialType::Type(Type::Group)) => Constant {
                    id: scope.context.get_id(),
                    parent: Cell::new(None),
                    span: Some(span.clone()),
                    value: ConstValue::Group(GroupValue::Single(
                        value.parse().map_err(|_| AsgError::invalid_int(value, span))?,
                    )),
                },
                Some(PartialType::Type(Type::Address)) => Constant {
                    id: scope.context.get_id(),
                    parent: Cell::new(None),
                    span: Some(span.clone()),
                    value: ConstValue::Address(value.clone()),
                },
                Some(x) => {
                    return Err(AsgError::unexpected_type(x, "unknown", span).into());
                }
            },
            Integer(int_type, value, span) => {
                match expected_type {
                    Some(PartialType::Integer(Some(sub_type), _)) if &sub_type == int_type => (),
                    Some(PartialType::Integer(None, Some(_))) => (),
                    None => (),
                    Some(x) => {
                        return Err(AsgError::unexpected_type(x, int_type, span).into());
                    }
                }
                Constant {
                    id: scope.context.get_id(),
                    parent: Cell::new(None),
                    span: Some(span.clone()),
                    value: ConstValue::Int(ConstInt::parse(int_type, value, span)?),
                }
            }
            String(_str_type, _span) => {
                unimplemented!("strings do not exist on ASG level")
            }
        })
    }
}

impl<'a> Into<leo_ast::ValueExpression> for &Constant<'a> {
    fn into(self) -> leo_ast::ValueExpression {
        match &self.value {
            ConstValue::Address(value) => {
                leo_ast::ValueExpression::Address(value.clone(), self.span.clone().unwrap_or_default())
            }
            ConstValue::Boolean(value) => {
                leo_ast::ValueExpression::Boolean(value.to_string().into(), self.span.clone().unwrap_or_default())
            }
            ConstValue::Char(value) => match value {
                CharValue::Scalar(scalar) => leo_ast::ValueExpression::Char(leo_ast::CharValue {
                    character: leo_ast::Char::Scalar(*scalar),
                    span: self.span.clone().unwrap_or_default(),
                }),
                CharValue::NonScalar(non_scalar) => leo_ast::ValueExpression::Char(leo_ast::CharValue {
                    character: leo_ast::Char::NonScalar(*non_scalar),
                    span: self.span.clone().unwrap_or_default(),
                }),
            },
            ConstValue::Field(value) => {
                leo_ast::ValueExpression::Field(value.to_string().into(), self.span.clone().unwrap_or_default())
            }
            ConstValue::Group(value) => leo_ast::ValueExpression::Group(Box::new(match value {
                GroupValue::Single(single) => {
                    leo_ast::GroupValue::Single(single.to_string().into(), self.span.clone().unwrap_or_default())
                }
                GroupValue::Tuple(left, right) => leo_ast::GroupValue::Tuple(leo_ast::GroupTuple {
                    x: left.into(),
                    y: right.into(),
                    span: self.span.clone().unwrap_or_default(),
                }),
            })),
            ConstValue::Int(int) => leo_ast::ValueExpression::Integer(
                int.get_int_type(),
                int.raw_value().into(),
                self.span.clone().unwrap_or_default(),
            ),
            ConstValue::Tuple(_) => unimplemented!(),
            ConstValue::Array(_) => unimplemented!(),
            ConstValue::Circuit(_, _) => unimplemented!(),
        }
    }
}
