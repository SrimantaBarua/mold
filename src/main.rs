fn main() {
    let mut lexer = mold::Lexer::new("#t #f '(\"hello\" hello #b10 #o10 #x10 10 0.5)");
    let tokens = lexer.collect::<Vec<_>>();
    println!("{:?}", tokens);
}
