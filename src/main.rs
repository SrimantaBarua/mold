fn main() {
    let mut mold = mold::Mold::new(std::io::stderr());
    mold.interpret("main", "#f");
}
