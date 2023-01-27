fn main() {
    let mut mold = mold::Mold::new();
    mold.interpret("main", "(+ 1 2.5)");
}
