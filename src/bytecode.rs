use crate::{Heap, MoldObject, ObjectType, Ptr, Value, ValueStore};

#[derive(Debug)]
#[repr(u8)]
pub enum Op {
    // push simple constants on the stack
    Null = 0,
    True,
    False,
    // push constants from chunk on the stack
    Const0,
    Const1,
    Const2,
    Const3,
    Const4,
    Const5,
    Const6,
    Const7,
    Const1B, // index = next u8 + 8 = 8..=263
    Const2B, // index = next u16 + 8 + 256 = 264..=65799
}

#[derive(Debug)]
struct LineNumber {
    line: usize,
    run_length: usize,
}

pub struct Chunk {
    name: String,
    opcodes: Vec<u8>,
    lines: Vec<LineNumber>,
    constants: Vec<ValueStore>,
}

impl MoldObject for Chunk {
    const TYPE: ObjectType = ObjectType::Chunk;
}

impl Chunk {
    pub fn new(name: impl ToString, heap: &Heap) -> Ptr<'_, Chunk> {
        heap.new_object(Chunk {
            name: name.to_string(),
            opcodes: Vec::new(),
            lines: Vec::new(),
            constants: Vec::new(),
        })
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
            3 => Op::Const0,
            4 => Op::Const1,
            5 => Op::Const2,
            6 => Op::Const3,
            7 => Op::Const4,
            8 => Op::Const5,
            9 => Op::Const6,
            10 => Op::Const7,
            11 => Op::Const1B,
            12 => Op::Const2B,
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
        write!(f, "{}:\n", self.name)?;
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
                Op::Const0 => self.print_constant_op("OP_CONST0", 0, f)?,
                Op::Const1 => self.print_constant_op("OP_CONST1", 1, f)?,
                Op::Const2 => self.print_constant_op("OP_CONST2", 2, f)?,
                Op::Const3 => self.print_constant_op("OP_CONST3", 3, f)?,
                Op::Const4 => self.print_constant_op("OP_CONST4", 4, f)?,
                Op::Const5 => self.print_constant_op("OP_CONST5", 5, f)?,
                Op::Const6 => self.print_constant_op("OP_CONST6", 6, f)?,
                Op::Const7 => self.print_constant_op("OP_CONST7", 7, f)?,
                Op::Const1B => {
                    let index = self.get_u8(ip) as usize;
                    ip += 1;
                    self.print_constant_op("OP_CONST1B", index, f)?;
                }
                Op::Const2B => {
                    let index = self.get_u16(ip) as usize;
                    ip += 2;
                    self.print_constant_op("OP_CONST2B", index, f)?;
                }
            }
        }
        Ok(())
    }
}
