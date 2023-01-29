fn main() {
    let mut mold = mold::Mold::new(std::io::stderr());
    mold.interpret("main", "#t\n (define 1 2)");
}
