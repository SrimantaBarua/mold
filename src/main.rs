fn main() {
    let heap = mold::Heap::new();
    let mut lexer = mold::Lexer::new("(1 2 . 3)");
    loop {
        match mold::read(&mut lexer, &heap) {
            mold::ReaderResult::Eof => break,
            mold::ReaderResult::Error {
                message,
                line_number,
            } => eprintln!("Reader error: {}: {}", line_number, message),
            mold::ReaderResult::Value(value) => {
                eprintln!("Read: {:?}", value)
            }
        }
    }
}
