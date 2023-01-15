fn main() {
    let mut heap = mold::Heap::new();
    let mut chunk = mold::Chunk::new("main");
    chunk.push_op(mold::Op::Null, 1);
    chunk.push_op(mold::Op::True, 1);
    chunk.push_op(mold::Op::False, 2);
    chunk.push_constant(mold::Value::int(1));
    chunk.push_op(mold::Op::Const0, 3);
    println!("{}", chunk);
}
