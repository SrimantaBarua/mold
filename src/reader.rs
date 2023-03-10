use crate::heap::{Ptr, Str};
use crate::lexer::{Token, TokenType};
use crate::{Heap, Lexer, Value};

#[derive(Debug)]
pub enum ReaderResult<'a> {
    Eof,
    Error {
        message: String,
        line_number: usize,
    },
    Value {
        expression: Value<'a>,
        line_number: usize,
    },
}

macro_rules! advance_reader {
    ($reader:expr) => {{
        let line_number = $reader.current.line_number;
        if let Some(message) = $reader.advance() {
            return ReaderResult::Error {
                message,
                line_number,
            };
        }
    }};
}

struct Reader<'a, 'b>
where
    'a: 'b,
{
    current: Token<'a>,
    lexer: &'b mut Lexer<'a>,
    heap: &'b Heap,
}

impl<'a, 'b> Reader<'a, 'b> {
    fn expression(&mut self) -> ReaderResult<'b> {
        match self.current.typ.clone() {
            TokenType::LeftParen => self.list(),
            TokenType::RightParen => ReaderResult::Error {
                message: format!("unexpected ')' at the start of an expression"),
                line_number: self.current.line_number,
            },
            TokenType::True => ReaderResult::Value {
                expression: Value::t(),
                line_number: self.current.line_number,
            },
            TokenType::False => ReaderResult::Value {
                expression: Value::f(),
                line_number: self.current.line_number,
            },
            TokenType::Quote => self.wrap_expression(self.heap.new_str("quote")),
            TokenType::QuasiQuote => self.wrap_expression(self.heap.new_str("quasiquote")),
            TokenType::Unquote => self.wrap_expression(self.heap.new_str("unquote")),
            TokenType::UnquoteSplicing => {
                self.wrap_expression(self.heap.new_str("unquote-splicing"))
            }
            TokenType::Dot => ReaderResult::Error {
                message: format!("unexpected '.' at the start of an expression"),
                line_number: self.current.line_number,
            },
            TokenType::Integer(i) => ReaderResult::Value {
                expression: Value::int(i),
                line_number: self.current.line_number,
            },
            TokenType::Double(f) => ReaderResult::Value {
                expression: Value::double(f),
                line_number: self.current.line_number,
            },
            TokenType::String(s) => ReaderResult::Value {
                expression: Value::str(self.heap.new_str(s)),
                line_number: self.current.line_number,
            },
            TokenType::Identifier(i) => ReaderResult::Value {
                expression: Value::symbol(self.heap.new_str(i)),
                line_number: self.current.line_number,
            },
            TokenType::Error(message) => ReaderResult::Error {
                message,
                line_number: self.current.line_number,
            },
        }
    }

    fn list(&mut self) -> ReaderResult<'b> {
        let line_number = self.current.line_number;
        advance_reader!(self);
        let result = self.heap.new_cons(Value::null(), Value::null());
        let mut current_cons = result;
        let mut first = true;
        loop {
            match self.current.typ {
                TokenType::RightParen => {
                    // Safe because we're not calling the GC in the middle of read
                    break ReaderResult::Value {
                        expression: unsafe { result.cdr().extend_lifetime() },
                        line_number,
                    };
                }
                TokenType::Dot => {
                    if first {
                        break ReaderResult::Error {
                            message: "'.' can't be the first element in an expression".to_owned(),
                            line_number: self.current.line_number,
                        };
                    }
                    advance_reader!(self);
                    match self.expression() {
                        ReaderResult::Value { expression, .. } => {
                            current_cons.set_cdr(expression);
                            advance_reader!(self);
                            if !std::matches!(self.current.typ, TokenType::RightParen) {
                                break ReaderResult::Error {
                                    message: "'.' should be the penultimate element in a list"
                                        .to_owned(),
                                    line_number: self.current.line_number,
                                };
                            }
                            // Safe because we're not calling the GC in the middle of read
                            break ReaderResult::Value {
                                expression: unsafe { result.cdr().extend_lifetime() },
                                line_number,
                            };
                        }
                        ReaderResult::Eof => unreachable!(),
                        error => break error,
                    }
                }
                _ => match self.expression() {
                    ReaderResult::Value { expression, .. } => {
                        current_cons
                            .set_cdr(Value::cons(self.heap.new_cons(expression, Value::null())));
                        // Safe because we're not calling the GC in the middle of read
                        current_cons =
                            unsafe { current_cons.cdr().as_cons_unchecked().extend_lifetime() };
                        advance_reader!(self);
                        first = false;
                    }
                    ReaderResult::Eof => unreachable!(),
                    error => break error,
                },
            }
        }
    }

    fn wrap_expression(&mut self, with: Ptr<'_, Str>) -> ReaderResult<'b> {
        let line_number = self.current.line_number;
        advance_reader!(self);
        match self.expression() {
            ReaderResult::Value { expression, .. } => ReaderResult::Value {
                expression: Value::cons(self.heap.new_cons(
                    Value::symbol(with),
                    Value::cons(self.heap.new_cons(expression, Value::null())),
                )),
                line_number,
            },
            ReaderResult::Eof => unreachable!(),
            error => error,
        }
    }

    fn advance(&mut self) -> Option<String> {
        self.current = match self.lexer.next() {
            None => return Some("unterminated expression".to_owned()),
            Some(tok) => tok,
        };
        None
    }
}

// FIXME: The reader should also return the line number at the start of the expression that was
//        read.
pub fn read<'b>(lexer: &'b mut Lexer, heap: &'b Heap) -> ReaderResult<'b> {
    let current = match lexer.next() {
        None => return ReaderResult::Eof,
        Some(tok) => tok,
    };
    let mut reader = Reader {
        current,
        lexer,
        heap,
    };
    reader.expression()
}
