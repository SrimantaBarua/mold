use crate::heap::{Gc, Str};
use crate::{Heap, MoldObject, ObjectType, Ptr, Value, ValueStore};

#[derive(Debug)]
#[repr(u8)]
pub enum Op {
    // push simple constants on the stack
    Null = 0,
    True,
    False,
    Pop,
    // push constants from chunk on the stack
    Const1B, // index = next u8 = 0..=255
    Const2B, // index = next u16 + 256 = 256..=65791
    // Global variables (within the current module)
    SetGlobal,
    GetGlobal,
    // Local variables
    GetLocal1B,
    GetLocal2B,
    SetLocal1B,
    SetLocal2B,
}

#[derive(Debug)]
struct LineNumber {
    line: usize,
    run_length: usize,
}

pub struct Chunk {
    module: Gc<Str>,
    opcodes: Vec<u8>,
    lines: Vec<LineNumber>,
    constants: Vec<ValueStore>,
}

impl MoldObject for Chunk {
    const TYPE: ObjectType = ObjectType::Chunk;
}

impl Chunk {
    pub fn new<'a>(module: Ptr<'_, Str>, heap: &'a Heap) -> Ptr<'a, Chunk> {
        heap.new_object(Chunk {
            module: module.downgrade(),
            opcodes: Vec::new(),
            lines: Vec::new(),
            constants: Vec::new(),
        })
    }

    pub fn code_len(&self) -> usize {
        self.opcodes.len()
    }

    pub fn module(&self) -> Ptr<'_, Str> {
        // Safe provided Chunk is heap-allocated.
        unsafe { self.module.to_ptr() }
    }

    pub fn push_op(&mut self, op: Op, line_number: usize) {
        self.push_u8(op as u8, line_number)
    }

    pub fn push_u8(&mut self, byte: u8, line_number: usize) {
        self.opcodes.push(byte);
        self.push_line_number(line_number, 1);
    }

    pub fn push_u16(&mut self, word: u16, line_number: usize) {
        self.opcodes.push(((word >> 8) & 0xff) as u8);
        self.opcodes.push((word & 0xff) as u8);
        self.push_line_number(line_number, 2);
    }

    pub fn push_constant(&mut self, constant: Value<'_>) -> usize {
        self.constants.push(constant.into_inner());
        self.constants.len() - 1
    }

    pub fn get_op(&self, offset: usize) -> Op {
        match self.get_u8(offset) {
            0 => Op::Null,
            1 => Op::True,
            2 => Op::False,
            3 => Op::Pop,
            4 => Op::Const1B,
            5 => Op::Const2B,
            6 => Op::SetGlobal,
            7 => Op::GetGlobal,
            8 => Op::GetLocal1B,
            9 => Op::GetLocal2B,
            10 => Op::SetLocal1B,
            11 => Op::SetLocal2B,
            b => panic!("invalid opcode: {}", b),
        }
    }

    pub fn get_u8(&self, offset: usize) -> u8 {
        self.opcodes[offset]
    }

    pub fn get_u16(&self, offset: usize) -> u16 {
        ((self.opcodes[offset] as u16) << 8) | (self.opcodes[offset + 1] as u16)
    }

    pub fn get_constant(&self, index: usize) -> Value<'_> {
        // Safe provided Chunk is heap-allocated.
        unsafe { Value::new(self.constants[index]) }
    }

    pub fn get_line_at_offset(&self, offset: usize) -> usize {
        assert!(offset < self.opcodes.len());
        let mut current_offset = 0;
        for line in &self.lines {
            if current_offset + line.run_length > offset {
                return line.line;
            }
            current_offset += line.run_length;
        }
        panic!("could not find line for offset {}", offset);
    }

    fn push_line_number(&mut self, line: usize, run_length: usize) {
        if let Some(last) = self.lines.last_mut() {
            if last.line == line {
                last.run_length += run_length;
                return;
            }
        }
        self.lines.push(LineNumber { line, run_length });
    }

    fn print_constant_op(
        &self,
        name: &str,
        constant_index: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{} {}  ({:?})\n",
            name,
            constant_index,
            self.get_constant(constant_index)
        )
    }
}

impl std::fmt::Debug for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (mut ip, mut last_line) = (0, usize::MAX);
        while ip < self.opcodes.len() {
            write!(f, "{:#12x}: ", ip)?;
            let line = self.get_line_at_offset(ip);
            if line != last_line {
                write!(f, "{:8}  ", line)?;
                last_line = line;
            } else {
                f.write_str("       |  ")?;
            }
            ip += 1;
            match self.get_op(ip - 1) {
                Op::Null => f.write_str("OP_NULL\n")?,
                Op::True => f.write_str("OP_TRUE\n")?,
                Op::False => f.write_str("OP_FALSE\n")?,
                Op::Pop => f.write_str("OP_POP\n")?,
                Op::Const1B => {
                    let index = self.get_u8(ip) as usize;
                    ip += 1;
                    self.print_constant_op("OP_CONST1B", index, f)?;
                }
                Op::Const2B => {
                    let index = self.get_u16(ip) as usize + 256;
                    ip += 2;
                    self.print_constant_op("OP_CONST2B", index, f)?;
                }
                Op::SetGlobal => f.write_str("OP_SETGLOBAL\n")?,
                Op::GetGlobal => f.write_str("OP_GETGLOBAL\n")?,
                Op::GetLocal1B => {
                    let index = self.get_u8(ip) as usize;
                    ip += 1;
                    write!(f, "OP_GETLOCAL1B {}\n", index)?;
                }
                Op::GetLocal2B => {
                    let index = self.get_u16(ip) as usize + 256;
                    ip += 2;
                    write!(f, "OP_GETLOCAL2B {}\n", index)?;
                }
                Op::SetLocal1B => {
                    let index = self.get_u8(ip) as usize;
                    ip += 1;
                    write!(f, "OP_SETLOCAL1B {}\n", index)?;
                }
                Op::SetLocal2B => {
                    let index = self.get_u16(ip) as usize + 256;
                    ip += 2;
                    write!(f, "OP_SETLOCAL2B {}\n", index)?;
                }
            }
        }
        Ok(())
    }
}
