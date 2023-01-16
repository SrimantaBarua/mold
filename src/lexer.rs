use std::{iter::Peekable, str::CharIndices};

#[derive(Clone, Debug)]
pub enum TokenType<'a> {
    LeftParen,           // (
    RightParen,          // )
    True,                // #t
    False,               // #f
    Quote,               // '
    QuasiQuote,          // `
    Unquote,             // ,
    UnquoteSplicing,     // ,@
    Dot,                 // .
    Integer(i32),        // 1, -1, #b0110, #o7346, #d971, #x9ab7
    Double(f64),         // .2, 0.2, 1.5e+10, 2.
    String(&'a str),     // "hello"
    Identifier(&'a str), // hello
    // Special tokens
    Error(String),
}

#[derive(Debug)]
pub struct Token<'a> {
    pub(crate) typ: TokenType<'a>,
    pub(crate) line_number: usize,
}

pub struct Lexer<'a> {
    source: &'a str,
    line_number: usize,
    start_offset: usize,
    iter: Peekable<CharIndices<'a>>,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        self.skip_whitespace_and_comments();
        self.start_offset = self.current_offset();
        match self.iter.next()?.1 {
            '(' => Some(self.make_token(TokenType::LeftParen)),
            ')' => Some(self.make_token(TokenType::RightParen)),
            '\'' => Some(self.make_token(TokenType::Quote)),
            '`' => Some(self.make_token(TokenType::QuasiQuote)),
            ',' => match self.iter.peek() {
                Some((_, '@')) => {
                    self.iter.next();
                    Some(self.make_token(TokenType::UnquoteSplicing))
                }
                _ => Some(self.make_token(TokenType::Unquote)),
            },
            '"' => Some(self.string()),
            '+' | '-' => match self.iter.peek() {
                Some((_, c)) if c.is_ascii_digit() => Some(self.number(10, true)),
                _ => Some(self.identifier()),
            },
            '.' => match self.iter.peek() {
                Some((_, c)) if c.is_ascii_digit() => Some(self.number(10, true)),
                Some((_, c)) if c.is_alphabetic() || is_extended_identifier_character(*c) => {
                    Some(self.identifier())
                }
                _ => Some(self.make_token(TokenType::Dot)),
            },
            '#' => match self.iter.next() {
                Some((_, 'f')) => Some(self.make_token(TokenType::False)),
                Some((_, 't')) => Some(self.make_token(TokenType::True)),
                Some((_, 'b')) => Some(self.number(2, false)),
                Some((_, 'o')) => Some(self.number(8, false)),
                Some((_, 'd')) => Some(self.number(10, false)),
                Some((_, 'x')) => Some(self.number(16, false)),
                _ => {
                    let current_offset = self.current_offset();
                    Some(self.make_token(TokenType::Error(format!(
                        "expected t|f|b|o|d|x after '#', found: {}",
                        &self.source[self.start_offset..current_offset]
                    ))))
                }
            },
            c if c.is_ascii_digit() => Some(self.number(10, false)),
            c if c.is_alphabetic() || is_extended_identifier_character(c) => {
                Some(self.identifier())
            }
            _ => {
                // FIXME: Advance till the next sane stopping point then report and error (e.g.
                //        whitespace, or parenthesis).
                let current_offset = self.current_offset();
                Some(self.make_token(TokenType::Error(format!(
                    "unrecognized token: {}",
                    &self.source[self.start_offset..current_offset]
                ))))
            }
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer {
            source,
            line_number: 1,
            start_offset: 0,
            iter: source.char_indices().peekable(),
        }
    }

    fn identifier(&mut self) -> Token<'a> {
        while let Some((_, c)) = self.iter.peek() {
            if c.is_alphanumeric() || is_extended_identifier_character(*c) {
                self.iter.next();
                continue;
            }
            break;
        }
        let current_offset = self.current_offset();
        self.make_token(TokenType::Identifier(
            &self.source[self.start_offset..current_offset],
        ))
    }

    fn number(&mut self, radix: u32, mut is_float: bool) -> Token<'a> {
        debug_assert!(!is_float || (is_float && radix == 10));
        while let Some((_, c)) = self.iter.peek() {
            if c.is_ascii_digit() {
                self.iter.next();
                continue;
            }
            if *c == '.' {
                if radix != 10 || is_float {
                    break;
                }
                is_float = true;
                self.iter.next();
                continue;
            }
            break;
        }
        if radix == 10 {
            match self.iter.peek() {
                Some((_, 'e')) | Some((_, 'E')) => {
                    is_float = true;
                    self.iter.next();
                    while let Some((_, c)) = self.iter.peek() {
                        if *c == '+' || *c == '-' || c.is_ascii_digit() {
                            self.iter.next();
                            continue;
                        }
                        break;
                    }
                }
                _ => {}
            }
        }
        let mut start_offset = self.start_offset;
        if self.source.as_bytes()[start_offset] == b'#' {
            start_offset += 2;
        }
        let source = &self.source[start_offset..self.current_offset()];
        if is_float {
            match source.parse::<f64>() {
                Ok(num) => self.make_token(TokenType::Double(num)),
                Err(e) => self.make_token(TokenType::Error(format!(
                    "failed to parse floating point number: {}: {}",
                    source, e
                ))),
            }
        } else {
            match i32::from_str_radix(source, radix) {
                Ok(num) => self.make_token(TokenType::Integer(num)),
                Err(e) => self.make_token(TokenType::Error(format!(
                    "failed to parse integer: {}: {}",
                    source, e
                ))),
            }
        }
    }

    fn string(&mut self) -> Token<'a> {
        let start_line = self.line_number;
        while let Some((_, c)) = self.iter.next() {
            match c {
                '\n' => self.line_number += 1,
                '\\' => {
                    // FIXME: Make sure this is a valid escape sequence
                    self.iter.next();
                }
                '"' => {
                    let end = self.current_offset() - 1;
                    return Token {
                        typ: TokenType::String(&self.source[self.start_offset + 1..end]),
                        line_number: start_line,
                    };
                }
                _ => {}
            }
        }
        Token {
            typ: TokenType::Error(format!(
                "unterminated string: {}",
                &self.source[self.start_offset..]
            )),
            line_number: start_line,
        }
    }

    fn make_token(&self, typ: TokenType<'a>) -> Token<'a> {
        Token {
            typ,
            line_number: self.line_number,
        }
    }

    fn current_offset(&mut self) -> usize {
        self.iter
            .peek()
            .map(|(i, _)| *i)
            .unwrap_or_else(|| self.source.len())
    }

    fn skip_whitespace_and_comments(&mut self) {
        while let Some((_, c)) = self.iter.peek() {
            match c {
                ';' => {
                    while let Some((_, c)) = self.iter.next() {
                        if c == '\n' {
                            self.line_number += 1;
                            break;
                        }
                    }
                }
                '\n' => {
                    self.iter.next();
                    self.line_number += 1;
                }
                c if c.is_whitespace() => {
                    self.iter.next();
                }
                _ => break,
            }
        }
    }
}

fn is_extended_identifier_character(c: char) -> bool {
    match c {
        '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>' | '?' | '@'
        | '^' | '_' | '~' => true,
        _ => false,
    }
}
