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

//! Enforces a branch of a conditional or iteration statement in a compiled Leo program.

use crate::{program::ConstrainedProgram, GroupType, IndicatorAndConstrainedValue, StatementResult};
use leo_asg::BlockStatement;

use snarkvm_fields::PrimeField;
use snarkvm_gadgets::boolean::Boolean;
use snarkvm_r1cs::ConstraintSystem;

fn indicator_to_string(indicator: &Boolean) -> String {
    indicator
        .get_value()
        .map(|b| b.to_string())
        .unwrap_or_else(|| "[input]".to_string())
}


impl<'a, F: PrimeField, G: GroupType<F>> ConstrainedProgram<'a, F, G> {
    /// Evaluates a branch of one or more statements and returns a result in
    /// the given scope.
    #[allow(clippy::too_many_arguments)]
    pub fn evaluate_block<CS: ConstraintSystem<F>>(
        &mut self,
        cs: &mut CS,
        indicator: &Boolean,
        block: &BlockStatement<'a>,
    ) -> StatementResult<Vec<IndicatorAndConstrainedValue<'a, F, G>>> {
        let mut results = Vec::with_capacity(block.statements.len());
        // Evaluate statements. Only allow a single return argument to be returned.
        for statement in block.statements.iter() {
            println!("Indicator for statement: {};", indicator_to_string(&indicator));

            let value = self.enforce_statement(cs, indicator, statement.get())?;

            results.extend(value);
        }

        Ok(results)
    }
}
