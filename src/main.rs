fn main() {
    let mut heap = mold::Heap::new();
    let cons = heap.new_cons(
        mold::Value::cons(heap.new_cons(mold::Value::int(1), mold::Value::int(2))),
        mold::Value::null(),
    );
    println!("{:?}", cons);
}
