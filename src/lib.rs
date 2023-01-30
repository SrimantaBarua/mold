mod bytecode;
mod compiler;
mod heap;
mod lexer;
mod reader;
mod vm;

use std::{borrow::Cow, cell::RefCell, collections::HashMap};

pub use bytecode::{Chunk, Op};
pub use compiler::compile;
use heap::{Gc, Str};
pub use heap::{Heap, Ptr, Value};
pub use lexer::Lexer;
pub use reader::{read, ReaderResult};

pub(crate) use heap::{MoldObject, ObjectType, ValueStore};

use crate::compiler::CompilerResult;

pub enum MoldError<'a> {
    Compile {
        module: &'a str,
        line: usize,
        message: Cow<'a, str>,
    },
}

struct Globals(HashMap<Gc<Str>, HashMap<Gc<Str>, ValueStore>>);

impl Globals {
    fn get<'a>(&mut self, module: Ptr<'a, Str>, global: Ptr<'a, Str>) -> Option<Value<'a>> {
        let module = module.downgrade();
        let global = global.downgrade();
        self.0
            .get(&module)
            .and_then(|mod_globals| mod_globals.get(&global))
            .map(|v| {
                // Safe because Globals is a source of roots, so value is still reachable
                unsafe { Value::new(*v) }
            })
    }

    fn set<'a>(&mut self, module: Ptr<'a, Str>, global: Ptr<'a, Str>, value: Value<'a>) {
        self.0
            .entry(module.downgrade())
            .or_default()
            .insert(global.downgrade(), value.into_inner());
    }
}

impl std::fmt::Debug for Globals {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Globals:\n")?;
        for (module, symbols) in self.0.iter() {
            // Safe because Globals is a source of roots
            let module = unsafe { module.to_ptr() };
            write!(f, "  {}:\n", module.as_str())?;
            for (symbol, value) in symbols.iter() {
                // Safe because Globals is a source of roots
                let symbol = unsafe { symbol.to_ptr() };
                let value = unsafe { Value::new(*value) };
                write!(f, "    {}: {:?}\n", symbol.as_str(), value)?;
            }
        }
        Ok(())
    }
}

struct ModuleSource(HashMap<Gc<Str>, String>);

impl ModuleSource {
    fn get<'a>(&'a self, module: Ptr<'_, Str>) -> Option<&'a str> {
        self.0.get(&module.downgrade()).map(|s| s.as_ref())
    }

    fn set(&mut self, module: Ptr<'_, Str>, source: impl Into<String>) {
        self.0.insert(module.downgrade(), source.into());
    }
}

pub struct Mold<ErrStream>
where
    ErrStream: std::io::Write,
{
    heap: Heap,
    errors: RefCell<ErrStream>,
    globals: Globals,
    module_source: ModuleSource,
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
            globals: Globals(HashMap::new()),
            module_source: ModuleSource(HashMap::new()),
        }
    }

    // Returns `true` on success, `false` on failure.
    pub fn interpret(&mut self, module: &str, source: &str) -> bool {
        let mut lexer = Lexer::new(source);
        self.module_source.set(self.heap.new_str(module), source);
        loop {
            let chunk = match compile(module, &mut lexer, self) {
                CompilerResult::Eof => break true,
                CompilerResult::HadError => break false,
                CompilerResult::Chunk(chunk) => {
                    println!("Chunk:\n{:?}", *chunk);
                    chunk
                }
            };
            let fiber = vm::Fiber::new(chunk, &self.heap);
            let fiber_other_root = fiber.root(&self.heap);
            if !self.evaluate(fiber.root(&self.heap)) {
                break false;
            }
            eprintln!("VM state:\n{:?}{:?}", *fiber_other_root, self.globals);
            // At the end of execution, unroot the fiber (RAII does this for us). Unless we suspend
            // and move the fiber to the scheduler (TODO).
        }
    }
}
