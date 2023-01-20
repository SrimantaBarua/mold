use crate::{Chunk, Heap, Op, Ptr, Value, ValueType};

#[derive(Debug)]
pub struct CompilerError;

struct Compiler<'a> {
    chunk: Ptr<'a, Chunk>,
    line_number: usize,
    heap: &'a Heap,
}

impl<'a> Compiler<'a> {
    fn expression(&mut self, expression: Value<'a>) -> Result<(), CompilerError> {
        match expression.typ() {
            ValueType::Null => self.chunk.push_op(Op::Null, self.line_number),
            ValueType::True => self.chunk.push_op(Op::True, self.line_number),
            ValueType::False => self.chunk.push_op(Op::False, self.line_number),
            ValueType::Integer | ValueType::Double | ValueType::Str => {
                self.push_constant(expression)
            }
            ValueType::Symbol => {
                unimplemented!()
            }
            ValueType::Cons => {
                unimplemented!()
            }
        }
        Ok(())
    }

    fn push_constant(&mut self, value: Value<'a>) {
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
    heap: &'a Heap,
) -> Result<Ptr<'a, Chunk>, CompilerError> {
    let mut compiler = Compiler {
        chunk: Chunk::new(name, heap),
        line_number,
        heap,
    };
    compiler.expression(expression)?;
    Ok(compiler.chunk)
}
