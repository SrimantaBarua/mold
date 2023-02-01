use std::borrow::Cow;
use std::cell::Cell;

use crate::lexer::{Token, TokenType};
use crate::{Chunk, Lexer, Mold, Op, Ptr, Value};

struct CompilerError<'a> {
    module: &'a str,
    source: &'a str,
    start_offset: usize,
    end_offset: usize,
    line_number: usize,
    line_offset: usize,
    message: Cow<'static, str>,
    help: Option<&'static str>,
}

impl<'a> CompilerError<'a> {
    fn new(
        module: &'a str,
        source: &'a str,
        start_offset: usize,
        end_offset: usize,
        message: Cow<'static, str>,
        help: Option<&'static str>,
    ) -> CompilerError<'a> {
        let line_number = 1 + source[..start_offset]
            .bytes()
            .filter(|b| *b == b'\n')
            .count();
        let line_offset = source[..start_offset]
            .bytes()
            .rev()
            .position(|b| b == b'\n')
            .unwrap_or(start_offset);
        CompilerError {
            module,
            source,
            start_offset,
            end_offset,
            line_number,
            line_offset,
            message,
            help,
        }
    }

    fn max_line_number_width(&self) -> usize {
        let max_line_number = 2 + self.source[..self.end_offset]
            .bytes()
            .filter(|b| *b == b'\n')
            .count();
        max_line_number.to_string().len()
    }

    fn source_lines(&self) -> String {
        let line_number_width = self.max_line_number_width();
        let mut ret = format!("\x1b[1;34m{:line_number_width$} |\x1b[0m\n", "",);
        let mut cursor = self.start_offset - self.line_offset;
        let mut line_number = self.line_number;
        while cursor < self.end_offset {
            let line_length = self.source[cursor..]
                .bytes()
                .position(|b| b == b'\n')
                .unwrap_or(self.source[cursor..].len());
            ret.push_str(&format!(
                "\x1b[1;34m{:line_number_width$} |\x1b[0m  {}\n",
                line_number,
                &self.source[cursor..cursor + line_length]
            ));
            let error_start = if self.start_offset > cursor {
                self.start_offset - cursor
            } else {
                0
            };
            let error_end = if self.end_offset < cursor + line_length {
                self.end_offset - cursor
            } else {
                line_length
            };
            ret.push_str(&format!(
                "{:line_number_width$} \x1b[1;34m|  {}\x1b[31m{}\x1b[0m\n",
                "",
                " ".repeat(error_start),
                "^".repeat(error_end - error_start)
            ));
            cursor += line_length + 1;
            line_number += 1;
        }
        ret
    }
}

pub enum CompilerResult<'a> {
    Eof,
    HadError,
    Chunk(Ptr<'a, Chunk>),
}

#[derive(Clone)]
struct Local<'a> {
    name: &'a str,
    scope_depth: usize,
    visible: bool,
    initialized: bool,
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
    subexpr_start_stack: Vec<usize>,
    locals: Vec<Local<'a>>,
    scope_depth: usize,
    had_error: Cell<bool>,
    in_panic_mode: Cell<bool>,
}

impl<'a, 'b, ErrStream> Compiler<'a, 'b, ErrStream>
where
    ErrStream: std::io::Write,
    'a: 'b,
{
    fn expression(&mut self) {
        match &self.current.typ {
            TokenType::LeftParen => self.list(),
            TokenType::RightParen => self.error_at_current_token(
                "unbalanced closing parenthesis ')'",
                HELP_UNBALANCED_PARENS,
            ),
            TokenType::True => self.push_constant_op(Value::t(), self.current.line_number),
            TokenType::False => self.push_constant_op(Value::f(), self.current.line_number),
            TokenType::Quote => unimplemented!("quote"),
            TokenType::QuasiQuote => unimplemented!("quasiquote"),
            TokenType::Unquote => unimplemented!("unquote"),
            TokenType::UnquoteSplicing => unimplemented!("unquote-splicing"),
            TokenType::Dot => self.error_at_current_token(
                "incorrent use of dotted pair syntax '.'",
                HELP_DOTTED_PAIRS,
            ),
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
            TokenType::Identifier(i) => self.get_symbol_value(i),
            TokenType::Error(message) => self.error_at_current_token(message.clone(), None),
        }
    }

    fn start_expression(&mut self) {
        self.in_panic_mode.set(false);
        self.subexpr_start_stack.push(self.current.start);
    }

    fn end_expression(&mut self) {
        self.in_panic_mode.set(false);
        self.subexpr_start_stack
            .pop()
            .expect("bug: we weren't parsing an expression");
    }

    fn list(&mut self) {
        self.start_expression();
        self.advance();
        let symbol = match &self.current.typ {
            TokenType::Identifier(symbol) => *symbol,
            t => {
                self.error_at_current_token(
                    format!("expected symbol, found {}", t.to_str()),
                    HELP_NON_SYMBOL_FIRST_FORM,
                );
                "dummy"
            }
        };
        match symbol {
            "define" => self.define(),
            "let" => self.let_(),
            /*
            "let*" => self.let_(),
            "letrec" => self.let_(),
            "letrec*" => self.let_(),
            */
            _ => self.function_or_macro_call(symbol),
        }
        self.end_expression();
    }

    fn declare_variable(
        &mut self,
        name: &'a str,
        visible: bool,
        initialized: bool,
    ) -> Option<usize> {
        if self.scope_depth == 0 {
            // We're declaring a global variable. It's a no-op
            return None;
        }
        // Validate that there is no local variable at the same scope depth with the same name.
        if self
            .locals
            .iter()
            .rev()
            .take_while(|local| local.scope_depth == self.scope_depth)
            .find(|local| local.name == name)
            .is_some()
        {
            self.error_at_current_token(
                format!("cannot rebind '{}' in the same scope", name),
                HELP_LET_BINDING_REBOUND,
            );
        }
        // Push local
        self.locals.push(Local {
            name,
            scope_depth: self.scope_depth,
            visible,
            initialized,
        });
        Some(self.locals.len() - 1)
    }

    // TODO: Define local variable when inside a scope
    fn define(&mut self) {
        let start_line_number = self.current.line_number;
        self.advance();
        let variable = match &self.current.typ {
            TokenType::Identifier(symbol) => *symbol,
            t => {
                self.error_at_current_token(
                    format!("expected symbol, found {}", t.to_str()),
                    HELP_DEFINE_VARIABLE_NOT_SYMBOL,
                );
                "dummy"
            }
        };
        let index = self.declare_variable(variable, true, false);
        if index.is_none() {
            // This is a global variable
            self.push_constant_op(
                Value::symbol(self.mold.heap.new_str(variable)),
                self.current.line_number,
            );
        }
        self.advance();
        if std::matches!(self.current.typ, TokenType::RightParen) {
            self.chunk.push_op(Op::Null, start_line_number);
        } else {
            self.expression();
            self.advance();
            if !std::matches!(self.current.typ, TokenType::RightParen) {
                self.error_at_current_token(
                    "too many arguments to `define`",
                    HELP_DEFINE_EXTRA_ARGS,
                );
            }
        }
        if let Some(index) = index {
            // This is a local variable
            self.locals[index].initialized = true;
            // We need to simulate the effect of "define" which returns the symbol that was bound.
            self.push_constant_op(
                Value::symbol(self.mold.heap.new_str(variable)),
                self.current.line_number,
            );
        } else {
            // Global variable
            self.chunk.push_op(Op::SetGlobal, start_line_number);
        }
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;
        let num_locals_in_scope = self
            .locals
            .iter()
            .rev()
            .take_while(|local| local.scope_depth > self.scope_depth)
            .count();
        if num_locals_in_scope == 0 {
            return;
        }
        // A bit of trickery. Say we have N locals in this scope. We'll actually have N+1 values on
        // the stack for this scope, since the last value is the result of the last expression. We
        // want to retain that last value and pop off the locals before it.
        // So we set the 0th local in the scope to the N+1th value (which pops off the N+1th
        // value). Now we're left with N values on the stack. We want to pop off N-1 of then
        // (because the 0th is the return value).
        let return_index = self.locals.len() - num_locals_in_scope;
        self.set_local(return_index, self.current.line_number);
        self.locals.truncate(return_index);
        for _ in 0..num_locals_in_scope {
            self.chunk.push_op(Op::Pop, self.current.line_number);
        }
    }

    fn let_(&mut self) {
        self.advance();
        self.begin_scope();
        // Bindings
        if std::matches!(self.current.typ, TokenType::LeftParen) {
            self.advance();
        } else {
            self.error_at_current_expression(
                "`let` form missing bindings",
                self.current.end,
                HELP_LET_MISSING_BINDINGS,
            );
        }
        while !std::matches!(self.current.typ, TokenType::RightParen) {
            if std::matches!(self.current.typ, TokenType::LeftParen) {
                self.advance();
            } else {
                self.error_at_current_token(
                    "malformed let binding",
                    HELP_LET_BINDING_DOES_NOT_START_WITH_PAREN,
                );
            }
            if let TokenType::Identifier(variable) = self.current.typ {
                self.declare_variable(variable, false, false);
                self.advance();
            } else {
                self.error_at_current_token(
                    "malformed let binding",
                    HELP_LET_BINDING_FIRST_IS_NOT_IDENTIFIER,
                );
            };
            self.expression();
            self.advance();
            if std::matches!(self.current.typ, TokenType::RightParen) {
                self.in_panic_mode.set(false);
                self.advance();
            } else {
                self.error_at_current_token(
                    "malformed let binding",
                    HELP_LET_BINDING_DOES_NOT_END_WITH_PAREN,
                );
            }
        }
        self.advance();
        // Mark all locals visible and initialized
        self.locals
            .iter_mut()
            .rev()
            .take_while(|local| local.scope_depth == self.scope_depth)
            .for_each(|local| {
                local.visible = true;
                local.initialized = true;
            });
        let mut first = true;
        while !std::matches!(self.current.typ, TokenType::RightParen) {
            if !first {
                self.chunk.push_op(Op::Pop, self.current.line_number);
            }
            self.expression();
            self.advance();
            first = false;
        }
        if first {
            // There were no expressions in the body
            self.chunk.push_op(Op::Null, self.current.line_number);
        }
        self.end_scope();
    }

    fn set_local(&mut self, index: usize, line_number: usize) {
        match index {
            0..=255 => {
                self.chunk.push_op(Op::SetLocal1B, line_number);
                self.chunk.push_u8(index as u8, line_number);
            }
            256..=65791 => {
                self.chunk.push_op(Op::SetLocal2B, line_number);
                self.chunk.push_u16((index - 256) as u16, line_number);
            }
            _ => panic!("local variables >= 65792 unsupported"),
        }
    }

    fn get_local(&mut self, index: usize, line_number: usize) {
        match index {
            0..=255 => {
                self.chunk.push_op(Op::GetLocal1B, line_number);
                self.chunk.push_u8(index as u8, line_number);
            }
            256..=65791 => {
                self.chunk.push_op(Op::GetLocal2B, line_number);
                self.chunk.push_u16((index - 256) as u16, line_number);
            }
            _ => panic!("local variables >= 65792 unsupported"),
        }
    }

    fn get_symbol_value(&mut self, symbol: &str) {
        if let Some(index) = self
            .locals
            .iter()
            .rev()
            .position(|local| local.visible && local.name == symbol)
            .map(|index| self.locals.len() - index - 1)
        {
            self.get_local(index, self.current.line_number);
        } else {
            self.push_constant_op(
                Value::symbol(self.mold.heap.new_str(symbol)),
                self.current.line_number,
            );
            self.chunk.push_op(Op::GetGlobal, self.current.line_number);
        }
    }

    fn function_or_macro_call(&mut self, symbol: &str) {
        unimplemented!("function or macro call")
    }

    fn advance(&mut self) {
        match self.lexer.next() {
            None => self.error_at_current_expression(
                "unterminated expression",
                self.lexer.source().len(),
                HELP_UNTERMINATED_EXPRESSION,
            ),
            Some(token) => self.current = token,
        }
    }

    fn push_constant_op(&mut self, value: Value<'b>, line_number: usize) {
        let index = self.chunk.push_constant(value);
        match index {
            0..=255 => {
                self.chunk.push_op(Op::Const1B, line_number);
                self.chunk.push_u8(index as u8, line_number);
            }
            256..=65791 => {
                self.chunk.push_op(Op::Const2B, line_number);
                self.chunk.push_u16((index - 256) as u16, line_number);
            }
            _ => panic!("constant indices >= 65792 unsupported"),
        }
    }

    fn error_at_current_expression(
        &self,
        message: impl Into<Cow<'static, str>>,
        source_end: usize,
        help: Option<&'static str>,
    ) {
        let last_subexpr_start = self.subexpr_start_stack.last().expect(
            "bug: we should only have called error_at_current_expression \
                    if we're inside a sub-expression",
        );
        self.report_error(CompilerError::new(
            self.module,
            self.lexer.source(),
            *last_subexpr_start,
            source_end,
            message.into(),
            help,
        ))
    }

    fn error_at_current_token(
        &self,
        message: impl Into<Cow<'static, str>>,
        help: Option<&'static str>,
    ) {
        self.report_error(CompilerError::new(
            self.module,
            self.lexer.source(),
            self.current.start,
            self.current.end,
            message.into(),
            help,
        ))
    }

    fn report_error(&self, error: CompilerError<'_>) {
        // If we're in panic recovery mode, then don't print further syntax errors while we
        // "synchronize". This prevents error cascades which will only confuse a user.
        if self.in_panic_mode.get() {
            return;
        }
        self.in_panic_mode.set(true);
        self.had_error.set(true);
        write!(
            self.mold.errors.borrow_mut(),
            "\x1b[1;31merror\x1b[39m: {}\x1b[0m\n  \x1b[1;34m-->\x1b[0m {}:{}:{}\n{}{}\n",
            error.message,
            error.module,
            error.line_number,
            error.line_offset,
            error.source_lines(),
            error.help.unwrap_or(""),
        )
        .unwrap();
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
    let chunk = Chunk::new(mold.heap.new_str(module), &mold.heap);
    let mut compiler = Compiler {
        module,
        current,
        chunk,
        lexer,
        mold,
        locals: Vec::new(),
        scope_depth: 0,
        had_error: Cell::new(false),
        subexpr_start_stack: Vec::new(),
        in_panic_mode: Cell::new(false),
    };
    compiler.expression();
    if compiler.had_error.get() {
        return CompilerResult::HadError;
    }
    CompilerResult::Chunk(compiler.chunk)
}

const HELP_UNBALANCED_PARENS: Option<&str> = Some("help: Try removing the closing parenthesis\n");
const HELP_DOTTED_PAIRS: Option<&str> = Some(
    "\x1b[1mhelp\x1b[0m: Dotted pair syntax is a way to represent the \x1b[1;4mcar\x1b[0m and \x1b[1;4mcdr\x1b[0m of a cons
      explicitly. For instance (1 . 2) denotes a cons cell where the \x1b[1;4mcar\x1b[0m is 1
      and the \x1b[1;4mcdr\x1b[0m is 2. This looks like -
      +-------+
      | 1 | 2 |
      +-------+
      Since lists are made up of cons cells, the list (1 2 3) in dotted pair
      syntax would look like (1 . (2 . (3 . ()))). In box drawing form, this
      would look like -
      +-------+    +-------+    +--------+
      | 1 |  -+--->| 2 |  -+--->| 3 | () |
      +-------+    +-------+    +--------+
      In summary, it's an error to have '.' anywhere but between the two last
      elements in a list with at least two elements.\n",
);
const HELP_UNTERMINATED_EXPRESSION: Option<&str> =
    Some("\x1b[1mhelp\x1b[0m: Maybe you forgot a closing parenthesis? ')'\n");
const HELP_NON_SYMBOL_FIRST_FORM: Option<&str> = Some(
    "\x1b[1mhelp\x1b[0m: The first form in a list form should be a symbol which evaluates to a
      function or macro\n",
);
const HELP_DEFINE_VARIABLE_NOT_SYMBOL: Option<&str> = Some(
    "\x1b[1mhelp\x1b[0m: The first argument to \x1b[1;4mdefine\x1b[0m should be a symbol.
      \x1b[1;4mdefine\x1b[0m is a special form that is used to declare variables. Calling \x1b[1;4mdefine\x1b[0m
      in a top-level expression declares a \x1b[1mglobal\x1b[0m variable. Calling it within a
      scope declares a \x1b[1mlocal\x1b[0m variable.\n",
);
const HELP_DEFINE_EXTRA_ARGS: Option<&str> = Some(
    "\x1b[1mhelp\x1b[0m: \x1b[1;4mdefine\x1b[0m takes 1 or 2 arguments. The first argument should be a symbol.
      The second (optional) argument is an initial value to set for the
      variable. For example -
      (define foo)    \x1b[2m;; foo's value is ()\x1b[0m
      (define bar 1)  \x1b[2m;; bar's value is 1\x1b[0m\n",
);
const HELP_LET_MISSING_BINDINGS: Option<&str> = None; // TODO
const HELP_LET_BINDING_DOES_NOT_START_WITH_PAREN: Option<&str> = None; // TODO
const HELP_LET_BINDING_DOES_NOT_END_WITH_PAREN: Option<&str> = None; // TODO
const HELP_LET_BINDING_FIRST_IS_NOT_IDENTIFIER: Option<&str> = None; // TODO
const HELP_LET_BINDING_REBOUND: Option<&str> = None; // TODO
