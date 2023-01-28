mod bytecode;
mod compiler;
mod heap;
mod lexer;
mod reader;

use std::{borrow::Cow, cell::RefCell};

pub use bytecode::{Chunk, Op};
pub use compiler::compile;
pub use heap::{Heap, Ptr, Value};
pub use lexer::Lexer;
pub use reader::{read, ReaderResult};

pub(crate) use heap::{MoldObject, ObjectType, ValueStore, ValueType};

use crate::compiler::CompilerResult;

pub enum MoldError<'a> {
    Compile {
        module: &'a str,
        line: usize,
        message: Cow<'a, str>,
    },
}

pub struct Mold<ErrStream>
where
    ErrStream: std::io::Write,
{
    heap: Heap,
    errors: RefCell<ErrStream>,
}

impl<ErrStream> Mold<ErrStream>
where
    ErrStream: std::io::Write,
{
    pub fn new(error_stream: ErrStream) -> Mold<ErrStream> {
        let heap = Heap::new();
        Mold {
            heap,
            errors: RefCell::new(error_stream),
        }
    }

    // Returns `true` on success, `false` on failure.
    pub fn interpret(&mut self, module: &str, source: &str) -> bool {
        let mut lexer = Lexer::new(source);
        loop {
            /*
            let (expression, line_number) = match read(&mut lexer, &self.heap) {
                ReaderResult::Eof => break,
                ReaderResult::Error {
                    message,
                    line_number,
                } => {
                    // FIXME: Report error here
                    panic!("reader error: {}: {}", line_number, message);
                }
                ReaderResult::Value {
                    expression,
                    line_number,
                } => (expression, line_number),
            };
            */
            match compile(module, &mut lexer, self) {
                CompilerResult::Eof => break true,
                CompilerResult::HadError => break false,
                CompilerResult::Chunk(chunk) => {
                    println!("Chunk:\n{:?}", *chunk);
                }
            }
        }
    }
}
