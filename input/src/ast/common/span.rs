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

use std::fmt;

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Span<'a> {
    pub line_start: usize,
    pub line_stop: usize,
    pub col_start: usize,
    pub col_stop: usize,
    pub path: String,
    pub content: &'a [u8],
}

impl<'a> fmt::Display for Span<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.line_start == self.line_stop {
            write!(f, "{}:{}-{}", self.line_start, self.col_start, self.col_stop)
        } else {
            write!(
                f,
                "{}:{}-{}:{}",
                self.line_start, self.col_start, self.line_stop, self.col_stop
            )
        }
    }
}

// impl<'a> std::ops::Add for &Span<'a> {
//     type Output = Span<'a>;

//     fn add(self, other: &Span<'a>) -> Span<'a> {
//         self.clone() + other.clone()
//     }
// }

// impl<'a> std::ops::Add for Span<'a> {
//     type Output = Self;

//     #[allow(clippy::comparison_chain)]
//     fn add(self, other: Self) -> Self {
//         if self.line_start == other.line_stop {
//             Span {
//                 line_start: self.line_start,
//                 line_stop: self.line_stop,
//                 col_start: self.col_start.min(other.col_start),
//                 col_stop: self.col_stop.max(other.col_stop),
//                 path: self.path,
//                 content: self.content,
//             }
//         } else {
//             let mut new_content = vec![];
//             let self_lines = self.content.lines().collect::<Vec<_>>();
//             let other_lines = other.content.lines().collect::<Vec<_>>();
//             for line in self.line_start.min(other.line_start)..self.line_stop.max(other.line_stop) + 1 {
//                 if line >= self.line_start && line <= self.line_stop {
//                     new_content.push(self_lines.get(line - self.line_start).copied().unwrap_or_default());
//                 } else if line >= other.line_start && line <= other.line_stop {
//                     new_content.push(other_lines.get(line - other.line_start).copied().unwrap_or_default());
//                 } else if new_content.last().map(|x| *x != "...").unwrap_or(true) {
//                     new_content.push("...");
//                 }
//             }
//             let new_content = new_content.join("\n").into();
//             if self.line_start < other.line_stop {
//                 Span {
//                     line_start: self.line_start,
//                     line_stop: other.line_stop,
//                     col_start: self.col_start,
//                     col_stop: other.col_stop,
//                     path: self.path,
//                     content: new_content,
//                 }
//             } else {
//                 Span {
//                     line_start: other.line_start,
//                     line_stop: self.line_stop,
//                     col_start: other.col_start,
//                     col_stop: self.col_stop,
//                     path: self.path,
//                     content: new_content,
//                 }
//             }
//         }
//     }
// }
