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

#[macro_use]
extern crate thiserror;
extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;

pub mod ast;
pub use ast::*;

pub mod errors;
pub use errors::*;

// pub mod parser;
// pub use parser::*;

pub(crate) mod tokenizer;
pub(crate) use tokenizer::*;

#[cfg(test)]
mod test;

/// Creates a new input AST from a given file path and input source code text.
#[wasm_bindgen]
pub fn parse_input() {}
