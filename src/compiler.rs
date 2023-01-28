use crate::lexer::{Token, TokenType};
use crate::{Chunk, Lexer, Mold, Op, Ptr, Value, ValueType};

/*
#[derive(Debug)]
pub enum CompilerError<'a> {
    CarIsNotSymbol(Ptr<'a, Cons>),
    CdrIsNotCons(Ptr<'a, Cons>),
    InvalidArgCount { expected: usize, found: usize },
}

struct Compiler<'a> {
    chunk: Ptr<'a, Chunk>,
    line_number: usize,
    mold: &'a Mold,
}

// This verifies that cdr is either a cons or null (in which case it returns None)
fn take_car(cons: Ptr<'_, Cons>) -> Result<(Value<'_>, Option<Ptr<'_, Cons>>), CompilerError<'_>> {
    // Safe because we don't GC within the compiler
    let first = unsafe { cons.car().extend_lifetime() };
    let rest = unsafe { cons.cdr().extend_lifetime() };
    let rest = if rest.is_null() {
        None
    } else if rest.is_cons() {
        rest.as_cons()
    } else {
        return Err(CompilerError::CdrIsNotCons(cons));
    };
    Ok((first, rest))
}

impl<'a> Compiler<'a> {
    fn expression(&mut self, expression: Value<'a>) -> Result<(), CompilerError<'a>> {
        match expression.typ() {
            ValueType::Null => self.chunk.push_op(Op::Null, self.line_number),
            ValueType::True => self.chunk.push_op(Op::True, self.line_number),
            ValueType::False => self.chunk.push_op(Op::False, self.line_number),
            ValueType::Integer | ValueType::Double | ValueType::Str => {
                self.push_constant_op(expression)
            }
            ValueType::Symbol => {
                unimplemented!()
            }
            // Safe because we've just checked the type
            ValueType::Cons => self.cons(unsafe { expression.as_cons_unchecked() })?,
        }
        Ok(())
    }

    // NOTE: I'm only allowing symbols as the first element in a cons that I evaluate
    fn cons(&mut self, cons: Ptr<'a, Cons>) -> Result<(), CompilerError<'a>> {
        let (first, rest) = take_car(cons)?;
        let first = first
            .as_symbol()
            .ok_or_else(|| CompilerError::CarIsNotSymbol(cons))?;
        if self.mold.builtin.plus.ptr_eq(first) {
            return self.builtin_plus(rest);
        }
        unimplemented!("function or macro")
    }

    fn builtin_plus(&mut self, rest: Option<Ptr<'a, Cons>>) -> Result<(), CompilerError<'a>> {
        if let Some(mut rest) = rest {
            let mut args = Vec::new();
            loop {
                let (car, cdr) = take_car(rest)?;
                args.push(car);
                if let Some(cdr) = cdr {
                    rest = cdr;
                } else {
                    break;
                }
            }
            // FIXME: + should take an arbitrary number of args
            if args.len() != 2 {
                return Err(CompilerError::InvalidArgCount {
                    expected: 2,
                    found: args.len(),
                });
            }
            self.expression(args[0])?;
            self.expression(args[1])?;
            self.chunk.push_op(Op::Add, self.line_number);
        } else {
            // (+) evaluates to 0
            self.push_constant_op(Value::int(0));
        }
        Ok(())
    }

    fn push_constant_op(&mut self, value: Value<'a>) {
        let index = self.chunk.push_constant(value);
        match index {
            0 => self.chunk.push_op(Op::Const0, self.line_number),
            1 => self.chunk.push_op(Op::Const1, self.line_number),
            2 => self.chunk.push_op(Op::Const2, self.line_number),
            3 => self.chunk.push_op(Op::Const3, self.line_number),
            4 => self.chunk.push_op(Op::Const4, self.line_number),
            5 => self.chunk.push_op(Op::Const5, self.line_number),
            6 => self.chunk.push_op(Op::Const6, self.line_number),
            7 => self.chunk.push_op(Op::Const7, self.line_number),
            8..=263 => {
                self.chunk.push_op(Op::Const1B, self.line_number);
                self.chunk.push_u8((index - 8) as u8, self.line_number);
            }
            264..=65799 => {
                self.chunk.push_op(Op::Const2B, self.line_number);
                self.chunk.push_u16((index - 264) as u16, self.line_number);
            }
            _ => panic!("constant indices >= 65800 unsupported"),
        }
    }
}
*/

pub enum CompilerResult<'a> {
    Eof,
    HadError,
    Chunk(Ptr<'a, Chunk>),
}

struct Compiler<'a, 'b, ErrStream>
where
    ErrStream: std::io::Write,
    'a: 'b,
{
    module: &'a str,
    current: Token<'a>,
    chunk: Ptr<'b, Chunk>,
    lexer: &'b mut Lexer<'a>,
    mold: &'b Mold<ErrStream>,
    had_error: bool,
    in_panic_mode: bool,
}

macro_rules! compiler_error {
    ($compiler:expr, $($arg:tt)*) => {{ $compiler.error_fmt(format_args!($($arg)*)) }};
}

impl<'a, 'b, ErrStream> Compiler<'a, 'b, ErrStream>
where
    ErrStream: std::io::Write,
    'a: 'b,
{
    fn expression(&mut self, is_top_level: bool) {
        match &self.current.typ {
            TokenType::LeftParen => self.list(is_top_level),
            TokenType::RightParen => {
                compiler_error!(self, "unexpected ')' at the start of an expression")
            }
            TokenType::True => self.push_constant_op(Value::t(), self.current.line_number),
            TokenType::False => self.push_constant_op(Value::f(), self.current.line_number),
            TokenType::Quote => unimplemented!("quote"),
            TokenType::QuasiQuote => unimplemented!("quasiquote"),
            TokenType::Unquote => unimplemented!("unquote"),
            TokenType::UnquoteSplicing => unimplemented!("unquote-splicing"),
            TokenType::Dot => compiler_error!(self, "unexpected '.' at the start of an expression"),
            TokenType::Integer(i) => {
                self.push_constant_op(Value::int(*i), self.current.line_number)
            }
            TokenType::Double(f) => {
                self.push_constant_op(Value::double(*f), self.current.line_number)
            }
            TokenType::String(s) => self.push_constant_op(
                Value::str(self.mold.heap.new_str(s)),
                self.current.line_number,
            ),
            TokenType::Identifier(i) => unimplemented!("identifier"),
            TokenType::Error(message) => compiler_error!(self, "{}", message.clone()),
        }
    }

    fn list(&mut self, is_top_level: bool) {
        unimplemented!()
    }

    fn push_constant_op(&mut self, value: Value<'b>, line_number: usize) {
        let index = self.chunk.push_constant(value);
        match index {
            0 => self.chunk.push_op(Op::Const0, line_number),
            1 => self.chunk.push_op(Op::Const1, line_number),
            2 => self.chunk.push_op(Op::Const2, line_number),
            3 => self.chunk.push_op(Op::Const3, line_number),
            4 => self.chunk.push_op(Op::Const4, line_number),
            5 => self.chunk.push_op(Op::Const5, line_number),
            6 => self.chunk.push_op(Op::Const6, line_number),
            7 => self.chunk.push_op(Op::Const7, line_number),
            8..=263 => {
                self.chunk.push_op(Op::Const1B, line_number);
                self.chunk.push_u8((index - 8) as u8, line_number);
            }
            264..=65799 => {
                self.chunk.push_op(Op::Const2B, line_number);
                self.chunk.push_u16((index - 264) as u16, line_number);
            }
            _ => panic!("constant indices >= 65800 unsupported"),
        }
    }

    fn error_fmt(&mut self, args: core::fmt::Arguments<'_>) {
        self.in_panic_mode = true;
        self.had_error = true;
        write!(
            self.mold.errors.borrow_mut(),
            "compiler error: {}:{}: {}\n",
            self.module,
            self.current.line_number,
            args
        )
        .unwrap()
    }
}

pub fn compile<'a, 'b, ErrStream>(
    module: &'a str,
    lexer: &'b mut Lexer<'a>,
    mold: &'b Mold<ErrStream>,
) -> CompilerResult<'b>
where
    ErrStream: std::io::Write,
    'a: 'b,
{
    let current = match lexer.next() {
        None => return CompilerResult::Eof,
        Some(tok) => tok,
    };
    let chunk = Chunk::new(&mold.heap);
    let mut compiler = Compiler {
        module,
        current,
        chunk,
        lexer,
        mold,
        had_error: false,
        in_panic_mode: false,
    };
    compiler.expression(true);
    if compiler.had_error {
        return CompilerResult::HadError;
    }
    CompilerResult::Chunk(compiler.chunk)
}
