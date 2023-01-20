mod bytecode;
mod compiler;
mod heap;
mod lexer;
mod reader;

pub use bytecode::{Chunk, Op};
pub use compiler::compile;
pub use heap::{Heap, Ptr, Value};
pub use lexer::Lexer;
pub use reader::{read, ReaderResult};

pub(crate) use heap::{MoldObject, ObjectType, ValueStore, ValueType};
