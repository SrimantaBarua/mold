fn main() {
    let mut mold = mold::Mold::new(std::io::stderr());
    mold.interpret("main", "(let* ((x 1) (y x)) (define z y))");
}
