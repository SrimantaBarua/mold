fn main() {
    let mut heap = mold::Heap::new();
    let foo = heap.new_str("foo");
    let foo1 = heap.new_str("foo");
    let bar = heap.new_str("bar");
    assert!(foo.ptr_eq(foo1));
    assert!(!foo.ptr_eq(bar));
    /*
    let cons = heap.new_cons(
        mold::Value::cons(heap.new_cons(mold::Value::int(1), mold::Value::int(2))),
        mold::Value::null(),
    );
    */
    println!("{:?} | {:?} | {:?}", foo, foo1, bar);
}
