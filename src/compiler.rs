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
        let mut ret = String::new();
        if self.line_number != 1 {
            let prev_line_end = self.start_offset - self.line_offset - 1;
            let prev_line_length = self.source[..prev_line_end]
                .bytes()
                .rev()
                .position(|b| b == b'\n')
                .unwrap_or(prev_line_end);
            let prev_line_start = prev_line_end - prev_line_length;
            ret.push_str(&format!(
                "{:line_number_width$} |  {}\n",
                self.line_number - 1,
                &self.source[prev_line_start..prev_line_end]
            ));
        }
        let mut cursor = self.start_offset - self.line_offset;
        let mut line_number = self.line_number;
        while cursor < self.end_offset {
            let line_length = self.source[cursor..]
                .bytes()
                .position(|b| b == b'\n')
                .unwrap_or(self.source[cursor..].len());
            ret.push_str(&format!(
                "{:line_number_width$} |  {}\n",
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
                "{:line_number_width$} |  {}{}\n",
                "",
                " ".repeat(error_start),
                "^".repeat(error_end - error_start)
            ));
            cursor += line_length + 1;
            line_number += 1;
        }
        if cursor < self.source.len() {
            let line_length = self.source[cursor..]
                .bytes()
                .position(|b| b == b'\n')
                .unwrap_or(self.source[cursor..].len());
            ret.push_str(&format!(
                "{:line_number_width$} |  {}\n",
                line_number,
                &self.source[cursor..cursor + line_length]
            ));
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
    fn expression(&mut self, is_top_level: bool) {
        match &self.current.typ {
            TokenType::LeftParen => self.list(is_top_level),
            TokenType::RightParen => {
                self.error_at_current("unbalanced closing parenthesis ')'", UNBALANCED_PARENS_HELP)
            }
            TokenType::True => self.push_constant_op(Value::t(), self.current.line_number),
            TokenType::False => self.push_constant_op(Value::f(), self.current.line_number),
            TokenType::Quote => unimplemented!("quote"),
            TokenType::QuasiQuote => unimplemented!("quasiquote"),
            TokenType::Unquote => unimplemented!("unquote"),
            TokenType::UnquoteSplicing => unimplemented!("unquote-splicing"),
            TokenType::Dot => {
                self.error_at_current("incorrent use of dotted pair syntax '.'", DOTTED_PAIRS_HELP)
            }
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
            TokenType::Identifier(i) => unimplemented!("identifier"),
            TokenType::Error(message) => self.error_at_current(message.clone(), None),
        }
    }

    fn list(&mut self, is_top_level: bool) {
        // TODO: Pop this off the stack when we get a closing parenthesis. Also pop when doing
        //       panic recovery.
        self.subexpr_start_stack.push(self.current.start);
        self.advance();
        let symbol = match &self.current.typ {
            TokenType::Identifier(symbol) => *symbol,
            _ => return self.error_at_current("CAR of list form should be a symbol", None),
        };
        match symbol {
            _ => self.function_or_macro_call(symbol),
        }
    }

    fn function_or_macro_call(&mut self, symbol: &str) {
        unimplemented!("function or macro call")
    }

    fn advance(&mut self) {
        let last_subexpr_start = self
            .subexpr_start_stack
            .last()
            .expect("bug: we should only have called advance if we're inside a sub-expression");
        let source = self.lexer.source();
        let end = source.len();
        match self.lexer.next() {
            None => self.report_error(CompilerError::new(
                self.module,
                self.lexer.source(),
                *last_subexpr_start,
                end,
                "unterminated expression".into(),
                None,
            )),
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

    fn error_at_current(&self, message: impl Into<Cow<'static, str>>, help: Option<&'static str>) {
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
        self.in_panic_mode.set(true);
        self.had_error.set(true);
        write!(
            self.mold.errors.borrow_mut(),
            "error: {}\n  --> {}:{}:{}\n{}{}\n",
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
    let chunk = Chunk::new(&mold.heap);
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
    compiler.expression(true);
    if compiler.had_error.get() {
        return CompilerResult::HadError;
    }
    CompilerResult::Chunk(compiler.chunk)
}

const UNBALANCED_PARENS_HELP: Option<&str> = Some("help: Try removing the closing parenthesis\n");
const DOTTED_PAIRS_HELP: Option<&str> = Some(
    r#"help: Dotted pair syntax is a way to represent the CAR and CDR of a cons
      explicitly. For instance (1 . 2) denotes a cons cell where the CAR is 1
      and the CDR is 2. This looks like -
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
      elements in a list with at least two elements.
"#,
);
