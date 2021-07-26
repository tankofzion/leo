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

use crate::ast::values::Char;

use std::{fmt, string};

/// Represents all valid Leo syntax tokens.
#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
    // Lexical Grammar
    // Literals
    CommentLine(&'a [u8]),
    CommentBlock(&'a [u8]),
    StringLit(Vec<Char>),
    Ident(&'a [u8]),
    Int(&'a [u8]),
    True,
    False,
    AddressLit(&'a [u8]),
    CharLit(Char),

    // Symbols
    Colon,
    Comma,
    DoubleQuote,
    Eq,
    LeftParen,
    LeftSquare,
    Minus,
    RightParen,
    RightSquare,
    Semicolon,
    SingleQuote,

    // Section Headers
    Constants,
    Main,
    Record,
    Registers,
    State,
    StateLeaf,

    // Syntactic Grammr
    // Types
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    Field,
    Group,
    Bool,
    Address,
    Char,

    // Protected
    // A list of tokens that indentifiers cannot be.
    As,
    Console,
    Const,
    Else,
    For,
    Function,
    If,
    Import,
    Input,
    In,
    Let,
    Mut,
    Return,
    Static,
    String,
    Test,

    // Meta
    Eof,
}

/// Represents all valid Leo keyword tokens.
pub const KEYWORD_TOKENS: &[Token] = &[
    Token::Address,
    Token::As,
    Token::Bool,
    Token::Char,
    Token::Console,
    Token::Const,
    Token::Constants,
    Token::Else,
    Token::False,
    Token::Field,
    Token::For,
    Token::Function,
    Token::Group,
    Token::I8,
    Token::I16,
    Token::I32,
    Token::I64,
    Token::I128,
    Token::If,
    Token::Import,
    Token::In,
    Token::Input,
    Token::Let,
    Token::Main,
    Token::Mut,
    Token::Record,
    Token::Registers,
    Token::Return,
    Token::Static,
    Token::State,
    Token::StateLeaf,
    Token::String,
    Token::True,
    Token::U8,
    Token::U16,
    Token::U32,
    Token::U64,
    Token::U128,
];

impl<'a> Token<'a> {
    ///
    /// Returns `true` if the `self` token equals a Leo keyword.
    ///
    pub fn is_keyword(&self) -> bool {
        KEYWORD_TOKENS.iter().any(|x| x == self)
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match self {
            CommentLine(s) => write!(f, "{}", string::String::from_utf8_lossy(s)),
            CommentBlock(s) => write!(f, "{}", string::String::from_utf8_lossy(s)),
            StringLit(string) => {
                write!(f, "\"")?;
                for character in string.iter() {
                    write!(f, "{}", character)?;
                }
                write!(f, "\"")
            }
            Ident(s) => write!(f, "{}", string::String::from_utf8_lossy(s)),
            Int(s) => write!(f, "{}", string::String::from_utf8_lossy(s)),
            True => write!(f, "true"),
            False => write!(f, "false"),
            AddressLit(s) => write!(f, "{}", string::String::from_utf8_lossy(s)),
            CharLit(s) => write!(f, "{}", s),
            Colon => write!(f, ":"),
            Comma => write!(f, ","),
            DoubleQuote => write!(f, "\""),
            Eq => write!(f, "="),
            LeftParen => write!(f, "("),
            LeftSquare => write!(f, "["),
            Minus => write!(f, "-"),
            RightParen => write!(f, ")"),
            RightSquare => write!(f, "]"),
            Semicolon => write!(f, ";"),
            SingleQuote => write!(f, "'"),
            Constants => write!(f, "constants"),
            Main => write!(f, "main"),
            Record => write!(f, "record"),
            Registers => write!(f, "registers"),
            State => write!(f, "state"),
            StateLeaf => write!(f, "state_leaf"),
            U8 => write!(f, "u8"),
            U16 => write!(f, "u16"),
            U32 => write!(f, "u32"),
            U64 => write!(f, "u64"),
            U128 => write!(f, "u128"),
            I8 => write!(f, "i8"),
            I16 => write!(f, "i16"),
            I32 => write!(f, "i32"),
            I64 => write!(f, "i64"),
            I128 => write!(f, "i128"),
            Field => write!(f, "field"),
            Group => write!(f, "group"),
            Bool => write!(f, "bool"),
            Address => write!(f, "address"),
            Char => write!(f, "char"),
            As => write!(f, "as"),
            Console => write!(f, "console"),
            Const => write!(f, "const"),
            Else => write!(f, "else"),
            For => write!(f, "for"),
            Function => write!(f, "function"),
            If => write!(f, "if"),
            Import => write!(f, "import"),
            Input => write!(f, "input"),
            In => write!(f, "in"),
            Let => write!(f, "let"),
            Mut => write!(f, "mut"),
            Return => write!(f, "return"),
            Static => write!(f, "static"),
            String => write!(f, "string"),
            Test => write!(f, "test"),
            Eof => write!(f, ""),
        }
    }
}
