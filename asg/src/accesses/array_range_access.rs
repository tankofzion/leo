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
use leo_ast::IntegerType;
use leo_errors::{AsgError, Result, Span};

use std::cell::Cell;

#[derive(Clone)]
pub struct ArrayRangeAccess<'a> {
    pub parent: Cell<Option<&'a Expression<'a>>>,
    pub span: Option<Span>,
    pub array: Cell<&'a Expression<'a>>,
    pub left: Cell<Option<&'a Expression<'a>>>,
    pub right: Cell<Option<&'a Expression<'a>>>,
    // this is either const(right) - const(left) OR the length inferred by type checking
    // special attention must be made to update this if semantic-altering changes are made to left or right.
    pub length: u32,
}

impl<'a> Node for ArrayRangeAccess<'a> {
    fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
}

impl<'a> ExpressionNode<'a> for ArrayRangeAccess<'a> {
    fn set_parent(&self, parent: &'a Expression<'a>) {
        self.parent.replace(Some(parent));
    }

    fn get_parent(&self) -> Option<&'a Expression<'a>> {
        self.parent.get()
    }

    fn enforce_parents(&self, expr: &'a Expression<'a>) {
        self.array.get().set_parent(expr);
        self.array.get().enforce_parents(self.array.get());
        if let Some(left) = self.left.get() {
            left.set_parent(expr);
        }
        if let Some(right) = self.right.get() {
            right.set_parent(expr);
        }
    }

    fn get_type(&self) -> Option<Type<'a>> {
        let element = match self.array.get().get_type() {
            Some(Type::Array(element, _)) => element,
            _ => return None,
        };

        Some(Type::Array(element, self.length))
    }

    fn is_mut_ref(&self) -> bool {
        self.array.get().is_mut_ref()
    }

    fn const_value(&self) -> Option<ConstValue<'a>> {
        let mut array = match self.array.get().const_value()? {
            ConstValue::Array(values) => values,
            _ => return None,
        };
        let const_left = match self.left.get().map(|x| x.const_value()) {
            Some(Some(ConstValue::Int(x))) => x.to_usize()?,
            None => 0,
            _ => return None,
        };
        let const_right = match self.right.get().map(|x| x.const_value()) {
            Some(Some(ConstValue::Int(x))) => x.to_usize()?,
            None => array.len(),
            _ => return None,
        };
        if const_left > const_right || const_right as usize > array.len() {
            return None;
        }

        Some(ConstValue::Array(array.drain(const_left..const_right).collect()))
    }

    fn is_consty(&self) -> bool {
        self.array.get().is_consty()
    }
}

impl<'a> FromAst<'a, leo_ast::accesses::ArrayRangeAccess> for ArrayRangeAccess<'a> {
    fn from_ast(
        scope: &'a Scope<'a>,
        value: &leo_ast::accesses::ArrayRangeAccess,
        expected_type: Option<PartialType<'a>>,
    ) -> Result<ArrayRangeAccess<'a>> {
        let (expected_array, expected_len) = match expected_type.clone() {
            Some(PartialType::Array(element, len)) => (Some(PartialType::Array(element, None)), len),
            None => (None, None),
            Some(x) => {
                return Err(AsgError::unexpected_type("array", x, &value.span).into());
            }
        };
        let array = <&Expression<'a>>::from_ast(scope, &*value.array, expected_array)?;
        let array_type = array.get_type();
        let (parent_element, parent_size) = match array_type {
            Some(Type::Array(inner, size)) => (inner, size),
            type_ => {
                return Err(AsgError::unexpected_type(
                    "array",
                    type_.map(|x| x.to_string()).unwrap_or_else(|| "unknown".to_string()),
                    &value.span,
                )
                .into());
            }
        };

        let left = value
            .left
            .as_deref()
            .map(|left| {
                <&Expression<'a>>::from_ast(scope, left, Some(PartialType::Integer(None, Some(IntegerType::U32))))
            })
            .transpose()?;
        let right = value
            .right
            .as_deref()
            .map(|right| {
                <&Expression<'a>>::from_ast(scope, right, Some(PartialType::Integer(None, Some(IntegerType::U32))))
            })
            .transpose()?;

        let const_left = match left.map(|x| x.const_value()) {
            Some(Some(ConstValue::Int(x))) => x.to_usize().map(|x| x as u32),
            None => Some(0),
            _ => None,
        };
        let const_right = match right.map(|x| x.const_value()) {
            Some(Some(ConstValue::Int(inner_value))) => {
                let u32_value = inner_value.to_usize().map(|x| x as u32);
                if let Some(inner_value) = u32_value {
                    if inner_value > parent_size {
                        let error_span = if let Some(right) = right {
                            right.span().cloned().unwrap_or_default()
                        } else {
                            value.span.clone()
                        };
                        return Err(AsgError::array_index_out_of_bounds(inner_value, &error_span).into());
                    } else if let Some(left) = const_left {
                        if left > inner_value {
                            let error_span = if let Some(right) = right {
                                right.span().cloned().unwrap_or_default()
                            } else {
                                value.span.clone()
                            };
                            return Err(AsgError::array_index_out_of_bounds(inner_value, &error_span).into());
                        }
                    }
                }
                u32_value
            }
            None => Some(parent_size),
            _ => None,
        };

        let mut length = if let (Some(left), Some(right)) = (const_left, const_right) {
            Some(right - left)
        } else {
            None
        };

        if let Some(expected_len) = expected_len {
            if let Some(length) = length {
                if length != expected_len {
                    let concrete_type = Type::Array(parent_element, length);
                    return Err(
                        AsgError::unexpected_type(expected_type.as_ref().unwrap(), concrete_type, &value.span).into(),
                    );
                }
            }
            if let Some(left_value) = const_left {
                if left_value + expected_len > parent_size {
                    let error_span = if let Some(left) = left {
                        left.span().cloned().unwrap_or_default()
                    } else {
                        value.span.clone()
                    };
                    return Err(AsgError::array_index_out_of_bounds(left_value, &error_span).into());
                }
            }
            length = Some(expected_len);
        }
        if length.is_none() {
            return Err(AsgError::unknown_array_size(&value.span).into());
        }

        Ok(ArrayRangeAccess {
            parent: Cell::new(None),
            span: Some(value.span.clone()),
            array: Cell::new(array),
            left: Cell::new(left),
            right: Cell::new(right),
            length: length.unwrap(),
        })
    }
}

impl<'a> Into<leo_ast::accesses::ArrayRangeAccess> for &ArrayRangeAccess<'a> {
    fn into(self) -> leo_ast::accesses::ArrayRangeAccess {
        leo_ast::accesses::ArrayRangeAccess {
            array: Box::new(self.array.get().into()),
            left: self.left.get().map(|left| Box::new(left.into())),
            right: self.right.get().map(|right| Box::new(right.into())),
            span: self.span.clone().unwrap_or_default(),
        }
    }
}
