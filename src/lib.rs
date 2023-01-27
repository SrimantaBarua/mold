mod bytecode;
mod compiler;
mod heap;
mod lexer;
mod reader;

pub use bytecode::{Chunk, Op};
pub use compiler::compile;
pub use heap::{Heap, Ptr, Value};
use heap::{Root, Str};
pub use lexer::Lexer;
pub use reader::{read, ReaderResult};

pub(crate) use heap::{MoldObject, ObjectType, ValueStore, ValueType};

struct Builtin {
    plus: Root<Str>, // +
}

impl Builtin {
    fn new(heap: &Heap) -> Builtin {
        let plus = heap.new_str("+").root(heap);
        Builtin { plus }
    }
}

pub struct Mold {
    heap: Heap,
    builtin: Builtin,
}

impl Mold {
    pub fn new() -> Mold {
        let heap = Heap::new();
        let builtin = Builtin::new(&heap);
        Mold { heap, builtin }
    }

    // FIXME: Figure out error reporting
    pub fn interpret(&mut self, module: &str, source: &str) {
        let mut lexer = Lexer::new(source);
        loop {
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
            // FIXME: Report error here
            let chunk = compile(module, line_number, expression, self).unwrap();
            println!("Chunk: {:?}", *chunk);
        }
    }
}
