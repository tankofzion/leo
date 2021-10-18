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

//! Methods to enforce logical expressions in a compiled Leo program.

use crate::Program;
use leo_asg::CastExpression;
use leo_errors::Result;
use snarkvm_ir::{CastData, Instruction, Value};

impl<'a> Program<'a> {
    pub fn enforce_cast(&mut self, cast: &'a CastExpression<'a>) -> Result<Value> {
        let inner = self.enforce_expression(cast.inner.get())?;

        let output = self.alloc();

        let type_ = Value::Str(dbg!(cast.target_type.to_string()));

        self.emit(Instruction::Cast(CastData {
            destination: output,
            arguments: vec![inner, type_],
        }));
        Ok(Value::Ref(output))
    }
}
