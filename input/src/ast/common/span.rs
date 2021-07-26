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

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(Clone, Copy, Debug, Default)]
pub struct Span {
    pub line_start: usize,
    pub line_stop: usize,
    pub col_start: usize,
    pub col_stop: usize,
    pub path: &'static str,
    pub content: &'static [u8],
}

#[wasm_bindgen]
impl Span {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            line_start: 0,
            line_stop: 0,
            col_start: 0,
            col_stop: 0,
            path: "",
            content: &[],
        }
    }
}

impl fmt::Display for Span {
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

impl std::ops::Add for &Span {
    type Output = Span;

    fn add(self, other: &Span) -> Span {
        self.clone() + other.clone()
    }
}

fn lines(bytes: &[u8]) -> Vec<&[u8]> {
    let mut lines = vec![];

    let mut start = 0;
    for (index, byte) in bytes.iter().enumerate() {
        if *byte == b'\n' {
            lines.push(&bytes[start..index]);
            start = index + 1;
        }
    }

    lines
}

impl<'a> std::ops::Add for Span {
    type Output = Self;

    #[allow(clippy::comparison_chain)]
    fn add(self, other: Self) -> Self {
        if self.line_start == other.line_stop {
            Span {
                line_start: self.line_start,
                line_stop: self.line_stop,
                col_start: self.col_start.min(other.col_start),
                col_stop: self.col_stop.max(other.col_stop),
                path: self.path,
                content: self.content,
            }
        } else {
            let mut new_content = vec![];
            let self_lines = lines(self.content);
            let other_lines = lines(other.content);
            let elipses_bytes = "...".as_bytes();

            for line in self.line_start.min(other.line_start)..self.line_stop.max(other.line_stop) + 1 {
                if line >= self.line_start && line <= self.line_stop {
                    new_content.push(self_lines.get(line - self.line_start).copied().unwrap_or_default());
                } else if line >= other.line_start && line <= other.line_stop {
                    new_content.push(other_lines.get(line - other.line_start).copied().unwrap_or_default());
                } else if new_content.last().map(|x| *x != elipses_bytes).unwrap_or(true) {
                    new_content.push(elipses_bytes.clone());
                }
            }

            let new_content = new_content.join(&b'\n');
            let content: &mut [u8] = &mut [];
            content.copy_from_slice(&new_content[0..]);
            if self.line_start < other.line_stop {
                Span {
                    line_start: self.line_start,
                    line_stop: other.line_stop,
                    col_start: self.col_start,
                    col_stop: other.col_stop,
                    path: self.path,
                    content,
                }
            } else {
                Span {
                    line_start: other.line_start,
                    line_stop: self.line_stop,
                    col_start: other.col_start,
                    col_stop: self.col_stop,
                    path: self.path,
                    content,
                }
            }
        }
    }
}
