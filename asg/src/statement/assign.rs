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
    CircuitMember, ConstInt, ConstValue, Expression, ExpressionNode, FromAst, Identifier, IntegerType, Node,
    PartialType, Scope, Statement, Type, Variable,
};
pub use leo_ast::AssignOperation;
use leo_ast::AssigneeAccess as AstAssigneeAccess;
use leo_errors::{AsgError, Result, Span};

use serde::Serialize;
use std::cell::Cell;

#[derive(Clone, Serialize)]
pub enum AssignAccess<'a> {
    ArrayRange(Cell<Option<&'a Expression<'a>>>, Cell<Option<&'a Expression<'a>>>),
    ArrayIndex(Cell<&'a Expression<'a>>),
    Tuple(usize),
    Member(Identifier),
}

#[derive(Clone, Serialize)]
pub struct AssignStatement<'a> {
    pub parent: Cell<Option<&'a Statement<'a>>>,
    pub span: Option<Span>,
    pub operation: AssignOperation,
    pub target_variable: Cell<&'a Variable<'a>>,
    pub target_accesses: Vec<AssignAccess<'a>>,
    pub value: Cell<&'a Expression<'a>>,
}

impl<'a> Node for AssignStatement<'a> {
    fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
}

impl<'a> FromAst<'a, leo_ast::AssignStatement> for &'a Statement<'a> {
    fn from_ast(
        scope: &'a Scope<'a>,
        statement: &leo_ast::AssignStatement,
        _expected_type: Option<PartialType<'a>>,
    ) -> Result<Self> {
        let (name, span) = (
            &statement.assignee.identifier.name.clone(),
            &statement.assignee.identifier.span,
        );

        let variable = if name.as_ref() == "input" {
            if let Some(input) = scope.resolve_input() {
                input.container
            } else {
                return Err(AsgError::illegal_input_variable_reference(&statement.span).into());
            }
        } else {
            scope
                .resolve_variable(name)
                .ok_or_else(|| AsgError::unresolved_reference(name, span))?
        };

        if !variable.borrow().mutable {
            return Err(AsgError::immutable_assignment(name, &statement.span).into());
        }
        let mut target_type: Option<PartialType> = Some(variable.borrow().type_.clone().into());

        let mut target_accesses = vec![];
        for access in statement.assignee.accesses.iter() {
            target_accesses.push(match access {
                AstAssigneeAccess::ArrayRange(left, right) => {
                    let index_type = Some(PartialType::Integer(None, Some(IntegerType::U32)));
                    let left = left
                        .as_ref()
                        .map(|left: &leo_ast::Expression| -> Result<&'a Expression<'a>> {
                            <&Expression<'a>>::from_ast(scope, left, index_type.clone())
                        })
                        .transpose()?;
                    let right = right
                        .as_ref()
                        .map(|right: &leo_ast::Expression| -> Result<&'a Expression<'a>> {
                            <&Expression<'a>>::from_ast(scope, right, index_type)
                        })
                        .transpose()?;

                    match &target_type {
                        Some(PartialType::Array(item, len)) => {
                            if let (Some(left), Some(right)) = (
                                left.as_ref()
                                    .map(|x| x.const_value())
                                    .unwrap_or_else(|| Some(ConstValue::Int(ConstInt::U32(0)))),
                                right
                                    .as_ref()
                                    .map(|x| x.const_value())
                                    .unwrap_or_else(|| Some(ConstValue::Int(ConstInt::U32(len.map(|x| x as u32)?)))),
                            ) {
                                let left = match left {
                                    ConstValue::Int(x) => x.to_usize().ok_or_else(|| {
                                        AsgError::invalid_assign_index(name, x.to_string(), &statement.span)
                                    })?,
                                    _ => unimplemented!(),
                                };
                                let right = match right {
                                    ConstValue::Int(x) => x.to_usize().ok_or_else(|| {
                                        AsgError::invalid_assign_index(name, x.to_string(), &statement.span)
                                    })?,
                                    _ => unimplemented!(),
                                };
                                if right >= left {
                                    target_type = Some(PartialType::Array(item.clone(), Some((right - left) as u32)))
                                } else {
                                    return Err(AsgError::invalid_backwards_assignment(
                                        name,
                                        left,
                                        right,
                                        &statement.span,
                                    )
                                    .into());
                                }
                            }
                        }
                        _ => return Err(AsgError::index_into_non_array(name, &statement.span).into()),
                    }

                    AssignAccess::ArrayRange(Cell::new(left), Cell::new(right))
                }
                AstAssigneeAccess::ArrayIndex(index) => {
                    target_type = match target_type.clone() {
                        Some(PartialType::Array(item, _)) => item.map(|x| *x),
                        _ => return Err(AsgError::index_into_non_array(name, &statement.span).into()),
                    };
                    AssignAccess::ArrayIndex(Cell::new(<&Expression<'a>>::from_ast(
                        scope,
                        index,
                        Some(PartialType::Integer(None, Some(IntegerType::U32))),
                    )?))
                }
                AstAssigneeAccess::Tuple(index, span) => {
                    let index = index
                        .value
                        .parse::<usize>()
                        .map_err(|_| AsgError::parse_index_error(span))?;
                    target_type = match target_type {
                        Some(PartialType::Tuple(types)) => types
                            .get(index)
                            .cloned()
                            .ok_or_else(|| AsgError::tuple_index_out_of_bounds(index, &statement.span))?,
                        _ => return Err(AsgError::index_into_non_tuple(name, &statement.span).into()),
                    };
                    AssignAccess::Tuple(index)
                }
                AstAssigneeAccess::Member(name) => {
                    target_type = match target_type {
                        Some(PartialType::Type(Type::Circuit(circuit))) => {
                            let circuit = circuit;

                            let members = circuit.members.borrow();
                            let member = members.get(name.name.as_ref()).ok_or_else(|| {
                                AsgError::unresolved_circuit_member(
                                    &circuit.name.borrow().name,
                                    &name.name,
                                    &statement.span,
                                )
                            })?;

                            let x = match &member {
                                CircuitMember::Variable(type_) => type_.clone(),
                                CircuitMember::Function(_) => {
                                    return Err(AsgError::illegal_function_assign(&name.name, &statement.span).into());
                                }
                            };
                            Some(x.partial())
                        }
                        _ => {
                            return Err(AsgError::index_into_non_tuple(
                                &statement.assignee.identifier.name,
                                &statement.span,
                            )
                            .into());
                        }
                    };
                    AssignAccess::Member(name.clone())
                }
            });
        }
        let value = <&Expression<'a>>::from_ast(scope, &statement.value, target_type)?;

        let statement = scope.context.alloc_statement(Statement::Assign(AssignStatement {
            parent: Cell::new(None),
            span: Some(statement.span.clone()),
            operation: statement.operation,
            target_variable: Cell::new(variable),
            target_accesses,
            value: Cell::new(value),
        }));

        {
            let mut variable = variable.borrow_mut();
            variable.assignments.push(statement);
        }

        Ok(statement)
    }
}

impl<'a> Into<leo_ast::AssignStatement> for &AssignStatement<'a> {
    fn into(self) -> leo_ast::AssignStatement {
        leo_ast::AssignStatement {
            operation: self.operation,
            assignee: leo_ast::Assignee {
                identifier: self.target_variable.get().borrow().name.clone(),
                accesses: self
                    .target_accesses
                    .iter()
                    .map(|access| match access {
                        AssignAccess::ArrayRange(left, right) => {
                            AstAssigneeAccess::ArrayRange(left.get().map(|e| e.into()), right.get().map(|e| e.into()))
                        }
                        AssignAccess::ArrayIndex(index) => AstAssigneeAccess::ArrayIndex(index.get().into()),
                        AssignAccess::Tuple(index) => AstAssigneeAccess::Tuple(
                            leo_ast::PositiveNumber {
                                value: index.to_string().into(),
                            },
                            self.span.clone().unwrap_or_default(),
                        ),
                        AssignAccess::Member(name) => AstAssigneeAccess::Member(name.clone()),
                    })
                    .collect(),
                span: self.span.clone().unwrap_or_default(),
            },
            value: self.value.get().into(),
            span: self.span.clone().unwrap_or_default(),
        }
    }
}
