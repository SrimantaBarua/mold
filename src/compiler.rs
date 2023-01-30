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
            _ => self.function_or_macro_call(symbol),
        }
        self.end_expression();
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
        self.push_constant_op(
            Value::symbol(self.mold.heap.new_str(variable)),
            self.current.line_number,
        );
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
        self.chunk.push_op(Op::SetGlobal, start_line_number);
    }

    // TODO: Try to resolve local variables first
    fn get_symbol_value(&mut self, symbol: &str) {
        self.push_constant_op(
            Value::symbol(self.mold.heap.new_str(symbol)),
            self.current.line_number,
        );
        self.chunk.push_op(Op::GetGlobal, self.current.line_number);
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
            0 => self.chunk.push_op(Op::Const0, line_number),
            1 => self.chunk.push_op(Op::Const1, line_number),
            2 => self.chunk.push_op(Op::Const2, line_number),
            3 => self.chunk.push_op(Op::Const3, line_number),
            4 => self.chunk.push_op(Op::Const4, line_number),
            5 => self.chunk.push_op(Op::Const5, line_number),
            6 => self.chunk.push_op(Op::Const6, line_number),
            7 => self.chunk.push_op(Op::Const7, line_number),
            8..=263 => {
                self.chunk.push_op(Op::Const1B, line_number);
                self.chunk.push_u8((index - 8) as u8, line_number);
            }
            264..=65799 => {
                self.chunk.push_op(Op::Const2B, line_number);
                self.chunk.push_u16((index - 264) as u16, line_number);
            }
            _ => panic!("constant indices >= 65800 unsupported"),
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
