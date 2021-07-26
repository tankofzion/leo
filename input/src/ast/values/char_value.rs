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

use crate::common::Span;

use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Char {
    Scalar(char),
    NonScalar(u32),
}

impl fmt::Display for Char {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Scalar(c) => write!(f, "{}", c),
            Self::NonScalar(c) => write!(f, "\\u{{{:X}}}", c),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct CharValue {
    pub value: Char,
    pub span: Span,
}

impl fmt::Display for CharValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
