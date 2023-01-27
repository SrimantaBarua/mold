use crate::heap::Cons;
use crate::{Chunk, Mold, Op, Ptr, Value, ValueType};

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

pub fn compile<'a>(
    name: impl ToString,
    line_number: usize,
    expression: Value<'a>,
    mold: &'a Mold,
) -> Result<Ptr<'a, Chunk>, CompilerError<'a>> {
    let mut compiler = Compiler {
        chunk: Chunk::new(name, &mold.heap),
        line_number,
        mold,
    };
    compiler.expression(expression)?;
    Ok(compiler.chunk)
}
