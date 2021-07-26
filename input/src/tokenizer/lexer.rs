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
    ast::{common::Span, values::Char},
    tokenizer::Token,
};

use std::{borrow::Borrow, fmt, string};

///
/// Returns the length of the given `wanted` string if the string can be eaten, otherwise returns [`None`].
/// A string can be eaten if its bytes are at the front of the given `input` array.
///
fn eat(input: &[u8], wanted: &str) -> Option<usize> {
    let wanted = wanted.as_bytes();
    if input.len() < wanted.len() {
        return None;
    }
    if &input[0..wanted.len()] == wanted {
        return Some(wanted.len());
    }
    None
}

///
/// Returns a new `StrTendril` string if an identifier can be eaten, otherwise returns [`None`].
/// An identifier can be eaten if its bytes are at the front of the given `input_tendril` string.
///
fn eat_identifier(input: &[u8]) -> Option<&[u8]> {
    if input.is_empty() {
        return None;
    }

    if !input[0].is_ascii_alphabetic() {
        return None;
    }

    let mut i = 1usize;
    while i < input.len() {
        if !input[i].is_ascii_alphanumeric() && input[i] != b'_' {
            break;
        }
        i += 1;
    }
    Some(&input[0..i])
}

impl<'a> Token<'a> {
    ///
    /// Returns a `char` if a character can be eaten, otherwise returns [`None`].
    ///
    fn eat_char(input: &[u8], escaped: bool, hex: bool, unicode: bool) -> Option<Char> {
        if input.is_empty() {
            return None;
        }

        if escaped {
            let string = string::String::from_utf8_lossy(input);
            let escaped = &string[1..string.len()];

            if escaped.len() != 1 {
                return None;
            }

            if let Some(character) = escaped.chars().next() {
                return match character {
                    '0' => Some(Char::Scalar(0 as char)),
                    't' => Some(Char::Scalar(9 as char)),
                    'n' => Some(Char::Scalar(10 as char)),
                    'r' => Some(Char::Scalar(13 as char)),
                    '\"' => Some(Char::Scalar(34 as char)),
                    '\'' => Some(Char::Scalar(39 as char)),
                    '\\' => Some(Char::Scalar(92 as char)),
                    _ => None,
                };
            } else {
                return None;
            }
        }

        if hex {
            let string = string::String::from_utf8_lossy(input);
            let hex_string = &string[2..string.len()];

            if hex_string.len() != 2 {
                return None;
            }

            if let Ok(ascii_number) = u8::from_str_radix(&hex_string, 16) {
                // According to RFC, we allow only values less than 128.
                if ascii_number > 127 {
                    return None;
                }

                return Some(Char::Scalar(ascii_number as char));
            }
        }

        if unicode {
            let string = string::String::from_utf8_lossy(input);
            if &string[string.len() - 1..] != "}" {
                return None;
            }

            let unicode_number = &string[3..string.len() - 1];
            let len = unicode_number.len();
            if !(1..=6).contains(&len) {
                return None;
            }

            if let Ok(hex) = u32::from_str_radix(&unicode_number, 16) {
                if let Some(character) = std::char::from_u32(hex) {
                    // scalar
                    return Some(Char::Scalar(character));
                } else if hex <= 0x10FFFF {
                    return Some(Char::NonScalar(hex));
                }
            }
        }

        if string::String::from_utf8_lossy(input).chars().count() != 1 {
            return None;
        } else if let Some(character) = string::String::from_utf8_lossy(input).chars().next() {
            return Some(Char::Scalar(character));
        }

        None
    }

    ///
    /// Returns a tuple: [(integer length, integer token)] if an integer can be eaten, otherwise returns [`None`].
    /// An integer can be eaten if its bytes are at the front of the given `input_tendril` string.
    ///
    fn eat_integer(input: &[u8]) -> (usize, Option<Token>) {
        if input.is_empty() {
            return (0, None);
        }

        if !input[0].is_ascii_digit() {
            return (0, None);
        }
        let mut i = 1;
        let mut is_hex = false;
        while i < input.len() {
            if i == 1 && input[0] == b'0' && input[i] == b'x' {
                is_hex = true;
                i += 1;
                continue;
            }
            if is_hex {
                if !input[i].is_ascii_hexdigit() {
                    break;
                }
            } else if !input[i].is_ascii_digit() {
                break;
            }

            i += 1;
        }
        (i, Some(Token::Int(&input[0..i])))
    }

    /// Returns the number of bytes in an emoji via a bit mask.
    fn utf8_byte_count(byte: u8) -> u8 {
        let mut mask = 0x80;
        let mut result = 0;
        while byte & mask > 0 {
            result += 1;
            mask >>= 1;
        }
        if result == 0 {
            1
        } else if result > 4 {
            4
        } else {
            result
        }
    }

    ///
    /// Returns a tuple: [(token length, token)] if the next token can be eaten, otherwise returns [`None`].
    /// The next token can be eaten if the bytes at the front of the given `input_tendril` string can be scanned into a token.
    ///
    pub(crate) fn eat(input: &[u8]) -> (usize, Option<Token>) {
        if input.is_empty() {
            return (0, None);
        }

        match input[0] {
            x if x.is_ascii_whitespace() => return (1, None),
            b'"' => {
                let mut i = 1;
                let mut len: u8 = 1;
                let mut start = 1;
                let mut in_escape = false;
                let mut escaped = false;
                let mut hex = false;
                let mut unicode = false;
                let mut end = false;
                let mut string = Vec::new();

                while i < input.len() {
                    // If it's an emoji get the length.
                    if input[i] & 0x80 > 0 {
                        len = Self::utf8_byte_count(input[i]);
                        i += (len as usize) - 1;
                    }

                    if !in_escape {
                        if input[i] == b'"' {
                            end = true;
                            break;
                        } else if input[i] == b'\\' {
                            in_escape = true;
                            start = i;
                            i += 1;
                            continue;
                        }
                    } else {
                        len += 1;

                        match input[i] {
                            b'x' => {
                                hex = true;
                            }
                            b'u' => {
                                unicode = true;
                            }
                            b'}' if unicode => {
                                in_escape = false;
                            }
                            _ if !hex && !unicode => {
                                escaped = true;
                                in_escape = false;
                            }
                            _ if hex && len == 4 => {
                                in_escape = false;
                            }
                            _ => {}
                        }
                    }

                    if !in_escape {
                        match Self::eat_char(&input[start..(len as usize)], escaped, hex, unicode) {
                            Some(character) => {
                                len = 1;
                                escaped = false;
                                hex = false;
                                unicode = false;
                                string.push(character.into());
                            }
                            None => return (0, None),
                        }
                    }

                    i += 1;

                    if !escaped && !hex && !unicode {
                        start = i;
                    }
                }

                if i == input.len() || !end {
                    return (0, None);
                }

                return (i + 1, Some(Token::StringLit(string)));
            }
            b'\'' => {
                let mut i = 1;
                let mut in_escape = false;
                let mut escaped = false;
                let mut hex = false;
                let mut unicode = false;
                let mut end = false;

                while i < input.len() {
                    if !in_escape {
                        if input[i] == b'\'' {
                            end = true;
                            break;
                        } else if input[i] == b'\\' {
                            in_escape = true;
                        }
                    } else {
                        if input[i] == b'x' {
                            hex = true;
                        } else if input[i] == b'u' {
                            if input[i + 1] == b'{' {
                                unicode = true;
                            } else {
                                return (0, None);
                            }
                        } else {
                            escaped = true;
                        }

                        in_escape = false;
                    }

                    i += 1;
                }

                if !end {
                    return (0, None);
                }

                return match Self::eat_char(&input[1..(i - 1)], escaped, hex, unicode) {
                    Some(character) => (i + 1, Some(Token::CharLit(character))),
                    None => (0, None),
                };
            }
            x if x.is_ascii_digit() => return Self::eat_integer(&input),
            b'(' => return (1, Some(Token::LeftParen)),
            b')' => return (1, Some(Token::RightParen)),
            b'-' => return (1, Some(Token::Minus)),
            b'/' => {
                if eat(input, "//").is_some() {
                    let eol = input.iter().position(|x| *x == b'\n');
                    let len = if let Some(eol) = eol { eol + 1 } else { input.len() };
                    return (len, Some(Token::CommentLine(&input[0..len])));
                } else if eat(input, "/*").is_some() {
                    if input.is_empty() {
                        return (0, None);
                    }
                    let eol = input.windows(2).skip(2).position(|x| x[0] == b'*' && x[1] == b'/');
                    let len = if let Some(eol) = eol { eol + 4 } else { input.len() };
                    return (len, Some(Token::CommentBlock(&input[0..len])));
                }

                return (0, None);
            }
            b':' => return (1, Some(Token::Colon)),
            b';' => return (1, Some(Token::Semicolon)),
            b'=' => return (1, Some(Token::Eq)),
            b'[' => return (1, Some(Token::LeftSquare)),
            b']' => return (1, Some(Token::RightSquare)),
            _ => (),
        }

        if let Some(ident) = eat_identifier(&input) {
            return (
                ident.len(),
                Some(match string::String::from_utf8_lossy(ident).borrow() {
                    x if x.starts_with("aleo1") => Token::AddressLit(ident),
                    "address" => Token::Address,
                    "as" => Token::As,
                    "bool" => Token::Bool,
                    "char" => Token::Char,
                    "console" => Token::Console,
                    "const" => Token::Const,
                    "constants" => Token::Constants,
                    "else" => Token::Else,
                    "false" => Token::False,
                    "field" => Token::Field,
                    "for" => Token::For,
                    "function" => Token::Function,
                    "group" => Token::Group,
                    "i8" => Token::I8,
                    "i16" => Token::I16,
                    "i32" => Token::I32,
                    "i64" => Token::I64,
                    "i128" => Token::I128,
                    "if" => Token::If,
                    "import" => Token::Import,
                    "in" => Token::In,
                    "input" => Token::Input,
                    "let" => Token::Let,
                    "main" => Token::Main,
                    "mut" => Token::Mut,
                    "record" => Token::Record,
                    "registers" => Token::Registers,
                    "return" => Token::Return,
                    "static" => Token::Static,
                    "state" => Token::State,
                    "state_leaf" => Token::StateLeaf,
                    "string" => Token::String,
                    "true" => Token::True,
                    "u8" => Token::U8,
                    "u16" => Token::U16,
                    "u32" => Token::U32,
                    "u64" => Token::U64,
                    "u128" => Token::U128,
                    _ => Token::Ident(ident),
                }),
            );
        }

        return (0, None);
    }
}

#[derive(Clone)]
pub struct SpannedToken<'a> {
    pub token: Token<'a>,
    pub span: Span,
}

impl<'a> fmt::Display for SpannedToken<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}' @ ", self.token.to_string().trim())
        // self.span.fmt(f)
    }
}

impl<'a> fmt::Debug for SpannedToken<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <SpannedToken as fmt::Display>::fmt(self, f)
    }
}

///
/// Returns true if the given string looks like Aleo address.
/// This method DOES NOT check if the address is valid on-chain.
///
pub(crate) fn check_address(address: &str) -> bool {
    // "aleo1" (LOWERCASE_LETTER | ASCII_DIGIT){58}
    if !address.starts_with("aleo1") || address.len() != 63 {
        return false;
    }
    address
        .chars()
        .skip(5)
        .all(|x| x.is_ascii_lowercase() || x.is_ascii_digit())
}
