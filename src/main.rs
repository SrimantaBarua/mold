use mold::{Heap, Lexer, ReaderResult};

fn main() {
    let heap = Heap::new();
    let source = "\"hello\"";
    let mut lexer = Lexer::new(source);
    let expression = match mold::read(&mut lexer, &heap) {
        ReaderResult::Value(expression) => expression,
        ReaderResult::Error {
            message,
            line_number,
        } => {
            panic!("reader error: {}: {}", line_number, message)
        }
        ReaderResult::Eof => {
            eprintln!("EOF");
            return;
        }
    };
    let chunk = mold::compile("main", 1, expression, &heap).unwrap();
    println!("Chunk: {:?}", *chunk);
}
