use mold::{Heap, Value};

fn main() {
    let heap = Heap::new();
    let cons = heap.new_cons(
        Value::cons(heap.new_cons(
            Value::symbol(heap.new_str("foo")),
            Value::str(heap.new_str("bar")),
        )),
        Value::cons(heap.new_cons(Value::int(5), Value::double(2.5))),
    );
    println!("{:?}", cons);
}
