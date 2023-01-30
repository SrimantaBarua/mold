use std::cell::{Cell, RefCell};

use crate::heap::{Gc, Root};
use crate::{Chunk, Heap, Mold, MoldObject, ObjectType, Op, Ptr, Value, ValueStore};

struct CallFrame {
    chunk: Gc<Chunk>,
    ip: Cell<usize>,
}

impl CallFrame {
    fn chunk(&self) -> Ptr<'_, Chunk> {
        // Safe because we only get a reference to the CallFrame from a reference to a Fiber, which
        // means that the Fiber is still reachable, which then means that the chunk is still
        // reachable.
        unsafe { self.chunk.to_ptr() }
    }
}

pub struct Fiber {
    call_stack: Vec<CallFrame>,
    value_stack: RefCell<Vec<ValueStore>>,
}

impl Fiber {
    pub(crate) fn new<'a>(chunk: Ptr<'a, Chunk>, heap: &'a Heap) -> Ptr<'a, Fiber> {
        heap.new_object(Fiber {
            call_stack: vec![CallFrame {
                chunk: chunk.downgrade(),
                ip: Cell::new(0),
            }],
            value_stack: RefCell::new(Vec::new()),
        })
    }

    fn stack_push(&self, value: Value<'_>) {
        self.value_stack.borrow_mut().push(value.into_inner())
    }

    fn stack_pop(&self) -> Option<Value<'_>> {
        self.value_stack.borrow_mut().pop().map(|v| {
            // Safe because we have a reference to the fiber, which means its stack is still
            // reachable.
            unsafe { Value::new(v) }
        })
    }

    fn current_frame(&self) -> &CallFrame {
        self.call_stack.last().expect("bug: call stack underflow")
    }
}

impl MoldObject for Fiber {
    const TYPE: ObjectType = ObjectType::Fiber;
}

impl std::fmt::Debug for Fiber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Fiber:\n  Call stack:\n")?;
        for frame in self.call_stack.iter().rev() {
            let chunk = frame.chunk();
            let ip = frame.ip.get();
            write!(f, "    {}: {:#x}\n", chunk.module().as_str(), ip)?;
        }
        f.write_str("  Value stack:\n")?;
        for value in self.value_stack.borrow().iter().rev() {
            // Safe because we have a reference to the fiber, so this value is reachable.
            let value = unsafe { Value::new(*value) };
            write!(f, "    {:?}\n", value)?;
        }
        Ok(())
    }
}

impl<ErrStream> Mold<ErrStream>
where
    ErrStream: std::io::Write,
{
    pub(crate) fn evaluate(&mut self, mut fiber: Root<Fiber>) -> bool {
        let mut context = ExecutionContext {
            fiber: &mut *fiber,
            mold: self,
        };
        context.evaluate();
        true
    }
}

struct ExecutionContext<'a, ErrStream>
where
    ErrStream: std::io::Write,
{
    fiber: &'a mut Fiber,
    mold: &'a mut Mold<ErrStream>,
}

impl<'a, ErrStream> ExecutionContext<'a, ErrStream>
where
    ErrStream: std::io::Write,
{
    fn evaluate(&mut self) {
        let current_frame = self.fiber.current_frame();
        let mut ip = current_frame.ip.get();
        let chunk = current_frame.chunk();
        let code_len = chunk.code_len();
        let module = chunk.module();
        while ip < code_len {
            let op = chunk.get_op(ip);
            ip += 1;
            match op {
                Op::Null => self.fiber.stack_push(Value::null()),
                Op::True => self.fiber.stack_push(Value::t()),
                Op::False => self.fiber.stack_push(Value::f()),
                Op::Const0 => self.fiber.stack_push(chunk.get_constant(0)),
                Op::Const1 => self.fiber.stack_push(chunk.get_constant(1)),
                Op::Const2 => self.fiber.stack_push(chunk.get_constant(2)),
                Op::Const3 => self.fiber.stack_push(chunk.get_constant(3)),
                Op::Const4 => self.fiber.stack_push(chunk.get_constant(4)),
                Op::Const5 => self.fiber.stack_push(chunk.get_constant(5)),
                Op::Const6 => self.fiber.stack_push(chunk.get_constant(6)),
                Op::Const7 => self.fiber.stack_push(chunk.get_constant(7)),
                Op::Const1B => {
                    let value = chunk.get_constant(chunk.get_u8(ip) as usize);
                    ip += 1;
                    self.fiber.stack_push(value);
                }
                Op::Const2B => {
                    let value = chunk.get_constant(chunk.get_u16(ip) as usize);
                    ip += 2;
                    self.fiber.stack_push(value);
                }
                Op::SetGlobal => {
                    let value = self
                        .fiber
                        .stack_pop()
                        .expect("bug: stack underflow: OP_SETGLOBAL: value");
                    let variable = self
                        .fiber
                        .stack_pop()
                        .expect("bug: stack underflow: OP_SETGLOBAL: variable")
                        .as_symbol()
                        .expect("bug: OP_SETGLOBAL: variable is not a value");
                    self.mold.globals.set(module, variable, value);
                    self.fiber.stack_push(Value::symbol(variable));
                }
                Op::GetGlobal => {
                    let variable = self
                        .fiber
                        .stack_pop()
                        .expect("bug: stack underflow: OP_GETGLOBAL: variable")
                        .as_symbol()
                        .expect("bug: OP_GETGLOBAL: variable is not a value");
                    match self.mold.globals.get(module, variable) {
                        Some(value) => self.fiber.stack_push(value),
                        None => {
                            self.report_error(
                                format!("unbound global '{}'", variable.as_str()),
                                None,
                            );
                            self.fiber.stack_push(Value::null());
                        }
                    }
                }
            }
            current_frame.ip.set(ip);
        }
    }

    fn report_error(&self, message: impl AsRef<str>, help: Option<&str>) {
        write!(
            self.mold.errors.borrow_mut(),
            "\x1b[1;31merror\x1b[39m: {}\x1b[0m\n{}\x1b[1mstack backtrace:\x1b[0m\n",
            message.as_ref(),
            help.unwrap_or("")
        )
        .unwrap();
        self.fiber.call_stack.iter().rev().for_each(|frame| {
            let chunk = frame.chunk();
            // FIXME: The way this is used right now, it only works because in the VM loop, we only
            //        update the frame's IP at the end of the massive `match` block.
            let ip = frame.ip.get();
            let line_number = chunk.get_line_at_offset(ip);
            let module = chunk.module();
            let source = self
                .mold
                .module_source
                .get(module)
                .expect("bug: chunk without source code");
            let line_start = if line_number == 1 {
                0
            } else {
                source
                    .char_indices()
                    .filter(|(_, c)| *c == '\n')
                    .nth(line_number - 2)
                    .expect("bug: source doesn't have enough lines")
                    .0
                    + 1
            };
            let line_length = source[line_start..]
                .bytes()
                .position(|b| b == b'\n')
                .unwrap_or(source.len() - line_start);
            write!(
                self.mold.errors.borrow_mut(),
                "  \x1b[2m{}:{}:\x1b[0m {}\n",
                module.as_str(),
                line_number,
                &source[line_start..line_start + line_length],
            )
            .unwrap();
        })
        // FIXME: Print stack trace
    }
}
