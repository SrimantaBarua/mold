use std::borrow::Borrow;
use std::cell::{Cell, RefCell};
use std::collections::HashSet;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;

#[derive(Clone, Copy, Debug)]
pub struct Value(ValueType);

impl Value {
    pub fn null() -> Value {
        Value(ValueType::Null)
    }

    pub fn t() -> Value {
        Value(ValueType::True)
    }

    pub fn f() -> Value {
        Value(ValueType::False)
    }

    pub fn int(i: i32) -> Value {
        Value(ValueType::Integer(i))
    }

    pub fn double(f: f64) -> Value {
        Value(ValueType::Double(f))
    }

    pub fn cons(ptr: Ptr<'_, Cons>) -> Value {
        Value(ValueType::Cons(ptr.0))
    }

    pub fn is_cons(&self) -> bool {
        std::matches!(self.0, ValueType::Cons(_))
    }
}

#[derive(Clone, Copy, Debug)]
enum ValueType {
    Null,  // '()
    True,  // #t
    False, // #f
    Integer(i32),
    Double(f64),
    Cons(NonNull<Object<Cons>>),
}

pub struct ReachableValue<'a>(Value, &'a PhantomData<()>);

impl<'a> ReachableValue<'a> {
    fn new(value: Value) -> ReachableValue<'a> {
        ReachableValue(value, &PhantomData)
    }

    pub fn as_cons(&self) -> Option<Ptr<'a, Cons>> {
        if self.0.is_cons() {
            Some(unsafe { self.as_cons_unchecked() })
        } else {
            None
        }
    }

    pub unsafe fn as_cons_unchecked(&self) -> Ptr<'a, Cons> {
        if let ValueType::Cons(ptr) = self.0 .0 {
            Ptr(ptr, &PhantomData)
        } else {
            panic!("as_cons_unchecked called for {:?}", self.0);
        }
    }
}

impl<'a> std::fmt::Debug for ReachableValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 .0 {
            ValueType::Cons(_) => {
                // Safe because we've just checked this is a cons
                unsafe { self.as_cons_unchecked().fmt(f) }
            }
            _ => self.0.fmt(f),
        }
    }
}

pub trait MoldObject: std::fmt::Debug {}

pub struct Root<T>(NonNull<Object<T>>);

impl<T> Deref for Root<T> {
    type Target = T;

    fn deref(&self) -> &T {
        // Safe because we know Root<T> hasn't been GC'ed
        unsafe { &self.0.as_ref().data }
    }
}

impl<T> std::fmt::Debug for Root<T>
where
    T: MoldObject,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Root").field(self.deref()).finish()
    }
}

impl<T> Clone for Root<T> {
    fn clone(&self) -> Self {
        Root(self.0)
    }
}

impl<T> Copy for Root<T> {}

pub struct Ptr<'a, T>(NonNull<Object<T>>, &'a PhantomData<()>);

impl<'a, T> Ptr<'a, T> {
    pub fn ptr_eq(&self, other: Self) -> bool {
        std::ptr::eq(self.0.as_ptr(), other.0.as_ptr())
    }
}

impl<'a, T> Deref for Ptr<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        // Safe because we know Ptr<'a, T> hasn't been GC'ed
        unsafe { &self.0.as_ref().data }
    }
}

impl<'a, T> std::fmt::Debug for Ptr<'a, T>
where
    T: MoldObject,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Ptr").field(self.deref()).finish()
    }
}

impl<'a, T> Clone for Ptr<'a, T> {
    fn clone(&self) -> Self {
        Ptr(self.0, &PhantomData)
    }
}

impl<'a, T> Copy for Ptr<'a, T> {}

#[derive(Debug)]
enum ObjectType {
    Cons,
}

struct ObjectHeader {
    next: *mut ObjectHeader,
    typ: ObjectType,
}

#[repr(C)]
struct Object<T> {
    header: ObjectHeader,
    data: T,
}

pub struct Cons {
    car: Value,
    cdr: Value,
}

impl Cons {
    fn car(&self) -> ReachableValue<'_> {
        ReachableValue(self.car, &PhantomData)
    }

    fn cdr(&self) -> ReachableValue<'_> {
        ReachableValue(self.cdr, &PhantomData)
    }
}

impl std::fmt::Debug for Cons {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cons")
            .field("car", &self.car())
            .field("cdr", &self.cdr())
            .finish()
    }
}

impl MoldObject for Cons {}

#[derive(Debug)]
pub struct Str(String);

impl MoldObject for Str {}

#[derive(Debug)]
pub struct Heap {
    objects: Cell<*mut ObjectHeader>,
    interner: RefCell<Interner>,
}

impl Heap {
    pub fn new() -> Heap {
        Heap {
            objects: Cell::new(std::ptr::null_mut()),
            interner: RefCell::new(Interner::new()),
        }
    }

    pub fn new_cons(&self, car: Value, cdr: Value) -> Ptr<'_, Cons> {
        self.new_object(Cons { car, cdr })
    }

    pub fn new_str<S>(&self, string: S) -> Ptr<'_, Str>
    where
        S: AsRef<str> + ToString,
    {
        if let Some(str) = self.interner.borrow().get(&string.as_ref()) {
            return Ptr(str.0, &PhantomData);
        }
        let ptr = self.new_object(Str(string.to_string()));
        self.interner.borrow_mut().insert(WeakStr(ptr.0));
        ptr
    }

    pub fn garbage_collect(&mut self) {
        // 1. Mark roots
        // 2. Free unmarked objects
        // FIXME: Before we free a Str object, we need to notify the interner to remove the Str
        //        from its set of strings.
        unimplemented!()
    }

    fn new_object<T>(&self, data: T) -> Ptr<'_, T> {
        let object = Box::new(Object {
            header: ObjectHeader {
                next: self.objects.get(),
                typ: ObjectType::Cons,
            },
            data,
        });
        let object_ptr = Box::into_raw(object);
        // We can do this because Object<T> is #[repr(C)]
        self.objects.set(object_ptr as *mut ObjectHeader);
        // Safe because we just allocated the pointer
        Ptr(unsafe { NonNull::new_unchecked(object_ptr) }, &PhantomData)
    }
}

#[derive(Clone)]
struct WeakStr(NonNull<Object<Str>>);

impl WeakStr {
    // This is unsafe in isolation - it requires the Interner and the Heap working in tandem to
    // ensure invariants hold. The invariant being - the Heap should notify the interner that it
    // should remove a string from its hashset, _before_ the Heap deletes the object.
    fn as_str(&self) -> &str {
        unsafe { &self.0.as_ref().data.0 }
    }
}

impl Borrow<str> for WeakStr {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl PartialEq for WeakStr {
    fn eq(&self, other: &Self) -> bool {
        self.as_str().eq(other.as_str())
    }
}

impl Eq for WeakStr {}

impl Hash for WeakStr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl std::fmt::Debug for WeakStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("WeakStr").field(&self.as_str()).finish()
    }
}

#[derive(Debug)]
struct Interner {
    strings: HashSet<WeakStr>,
}

impl Interner {
    fn new() -> Interner {
        Interner {
            strings: HashSet::default(),
        }
    }

    fn get(&self, string: &str) -> Option<WeakStr> {
        self.strings.get(string).cloned()
    }

    fn insert(&mut self, string: WeakStr) {
        debug_assert!(self.get(string.as_str()).is_none());
        self.strings.insert(string);
    }
}
