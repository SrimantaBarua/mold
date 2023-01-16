use std::borrow::Borrow;
use std::cell::{Cell, RefCell};
use std::collections::HashSet;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;

#[derive(Clone, Copy, Debug)]
pub enum ValueType {
    Null,
    True,
    False,
    Integer,
    Double,
    Cons,
    Str,
    Symbol,
}

#[derive(Clone, Copy)]
pub struct Value(u64);

impl Value {
    const NAN_VALUE: u64 = 0x7ff8000000000000;
    const NON_FLOAT_MASK: u64 = 0x7ffc000000000000;
    const TYPE_MASK: u64 = 0x8003000000000007;
    // FIXME: We can get one more bit off the top because of sign extension
    const POINTER_MASK: u64 = 0x0000fffffffffff8;
    const I32_MASK: u64 = 0x00000007fffffff8;
    const I32_SHIFT: u64 = 3;
    const TYPE_FLAG_NULL: u64 = 0;
    const TYPE_FLAG_TRUE: u64 = 1;
    const TYPE_FLAG_FALSE: u64 = 2;
    const TYPE_FLAG_INTEGER: u64 = 3;
    const TYPE_FLAG_CONS: u64 = 4;
    const TYPE_FLAG_STR: u64 = 5;
    const TYPE_FLAG_SYMBOL: u64 = 6;
    const TAG_NULL: u64 = Self::NON_FLOAT_MASK | Self::TYPE_FLAG_NULL;
    const TAG_TRUE: u64 = Self::NON_FLOAT_MASK | Self::TYPE_FLAG_TRUE;
    const TAG_FALSE: u64 = Self::NON_FLOAT_MASK | Self::TYPE_FLAG_FALSE;
    const TAG_INTEGER: u64 = Self::NON_FLOAT_MASK | Self::TYPE_FLAG_INTEGER;
    const TAG_CONS: u64 = Self::NON_FLOAT_MASK | Self::TYPE_FLAG_CONS;
    const TAG_STR: u64 = Self::NON_FLOAT_MASK | Self::TYPE_FLAG_STR;
    const TAG_SYMBOL: u64 = Self::NON_FLOAT_MASK | Self::TYPE_FLAG_SYMBOL;

    pub fn null() -> Value {
        Value(Self::TAG_NULL)
    }

    pub fn t() -> Value {
        Value(Self::TAG_TRUE)
    }

    pub fn f() -> Value {
        Value(Self::TAG_FALSE)
    }

    pub fn int(i: i32) -> Value {
        Value((((i as u64) << Self::I32_SHIFT) & Self::I32_MASK) | Self::TAG_INTEGER)
    }

    pub fn double(f: f64) -> Value {
        if f.is_nan() {
            Value(Self::NAN_VALUE)
        } else {
            Value(unsafe { std::mem::transmute(f) })
        }
    }

    pub fn cons(ptr: Ptr<'_, Cons>) -> Value {
        Value::object(ptr, Self::TAG_CONS)
    }

    pub fn str(ptr: Ptr<'_, Str>) -> Value {
        Value::object(ptr, Self::TAG_STR)
    }

    pub fn symbol(ptr: Ptr<'_, Str>) -> Value {
        Value::object(ptr, Self::TAG_SYMBOL)
    }

    fn object<T>(ptr: Ptr<'_, T>, tag: u64) -> Value {
        let raw = ptr.0.as_ptr() as u64;
        debug_assert_eq!((raw & !Self::POINTER_MASK), 0);
        Value(raw | tag)
    }

    pub fn is_null(&self) -> bool {
        self.0 == Self::TAG_NULL
    }

    pub fn is_t(&self) -> bool {
        self.0 == Self::TAG_TRUE
    }

    pub fn is_f(&self) -> bool {
        self.0 == Self::TAG_FALSE
    }

    pub fn is_int(&self) -> bool {
        (self.0 & (Self::NON_FLOAT_MASK | Self::TYPE_MASK)) == Self::TAG_INTEGER
    }

    pub fn is_double(&self) -> bool {
        (self.0 & Self::NON_FLOAT_MASK) != Self::NON_FLOAT_MASK
    }

    pub fn is_cons(&self) -> bool {
        (self.0 & (Self::NON_FLOAT_MASK | Self::TYPE_MASK)) == Self::TAG_CONS
    }

    pub fn is_str(&self) -> bool {
        (self.0 & (Self::NON_FLOAT_MASK | Self::TYPE_MASK)) == Self::TAG_STR
    }

    pub fn is_symbol(&self) -> bool {
        (self.0 & (Self::NON_FLOAT_MASK | Self::TYPE_MASK)) == Self::TAG_SYMBOL
    }

    pub fn as_int(&self) -> Option<i32> {
        if self.is_int() {
            Some(unsafe { self.as_int_unchecked() })
        } else {
            None
        }
    }

    pub unsafe fn as_int_unchecked(&self) -> i32 {
        ((self.0 & Self::I32_MASK) >> Self::I32_SHIFT) as i32
    }

    pub fn as_double(&self) -> Option<f64> {
        if self.is_double() {
            Some(unsafe { self.as_double_unchecked() })
        } else {
            None
        }
    }

    pub unsafe fn as_double_unchecked(&self) -> f64 {
        std::mem::transmute(self.0)
    }

    unsafe fn as_object_unchecked<T>(&self) -> NonNull<Object<T>> {
        NonNull::new_unchecked((self.0 & Value::POINTER_MASK) as *mut Object<T>)
    }

    pub fn typ(&self) -> ValueType {
        match self.0 & (Self::NON_FLOAT_MASK | Self::TYPE_MASK) {
            Self::TAG_NULL => ValueType::Null,
            Self::TAG_TRUE => ValueType::True,
            Self::TAG_FALSE => ValueType::False,
            Self::TAG_INTEGER => ValueType::Integer,
            Self::TAG_CONS => ValueType::Cons,
            Self::TAG_STR => ValueType::Str,
            Self::TAG_SYMBOL => ValueType::Symbol,
            _ => {
                if self.is_double() {
                    ValueType::Double
                } else {
                    panic!("unknown value type: {:?}", self.0)
                }
            }
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut strukt = f.debug_struct("Value");
        strukt.field("raw", &self.0);
        match self.typ() {
            ValueType::Null | ValueType::True | ValueType::False => {
                strukt.field("typ", &self.typ())
            }
            ValueType::Integer => strukt.field("int", unsafe { &self.as_int_unchecked() }),
            ValueType::Double => strukt.field("double", unsafe { &self.as_double_unchecked() }),
            ValueType::Cons => strukt.field("cons", unsafe { &self.as_object_unchecked::<Cons>() }),
            ValueType::Str => strukt.field("str", unsafe { &self.as_object_unchecked::<Str>() }),
            ValueType::Symbol => {
                strukt.field("symbol", unsafe { &self.as_object_unchecked::<Str>() })
            }
        };
        strukt.finish()
    }
}

pub struct ReachableValue<'a>(Value, &'a PhantomData<()>);

impl<'a> ReachableValue<'a> {
    pub(crate) unsafe fn new(value: Value) -> ReachableValue<'a> {
        ReachableValue(value, &PhantomData)
    }

    pub fn into_value(self) -> Value {
        self.0
    }

    pub fn as_cons(&self) -> Option<Ptr<'a, Cons>> {
        if self.0.is_cons() {
            Some(unsafe { self.as_cons_unchecked() })
        } else {
            None
        }
    }

    pub unsafe fn as_cons_unchecked(&self) -> Ptr<'a, Cons> {
        self.as_object_unchecked()
    }

    pub fn as_str(&self) -> Option<Ptr<'a, Str>> {
        if self.0.is_str() {
            Some(unsafe { self.as_str_unchecked() })
        } else {
            None
        }
    }

    pub unsafe fn as_str_unchecked(&self) -> Ptr<'a, Str> {
        self.as_object_unchecked()
    }

    pub fn as_symbol(&self) -> Option<Ptr<'a, Str>> {
        if self.0.is_symbol() {
            Some(unsafe { self.as_symbol_unchecked() })
        } else {
            None
        }
    }

    pub unsafe fn as_symbol_unchecked(&self) -> Ptr<'a, Str> {
        self.as_object_unchecked()
    }

    unsafe fn as_object_unchecked<T>(&self) -> Ptr<'a, T> {
        Ptr(self.0.as_object_unchecked(), &PhantomData)
    }
}

impl<'a> std::fmt::Debug for ReachableValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.typ() {
            ValueType::Cons => {
                // Safe because we've just checked this is a cons
                unsafe {
                    f.debug_tuple("Value::Cons")
                        .field(&self.as_cons_unchecked())
                        .finish()
                }
            }
            ValueType::Str => {
                // Safe because we've just checked this is a str
                unsafe {
                    f.debug_tuple("Value::Str")
                        .field(&self.as_str_unchecked())
                        .finish()
                }
            }
            ValueType::Symbol => {
                // Safe because we've just checked this is a str
                unsafe {
                    f.debug_tuple("Value::Symbol")
                        .field(&self.as_symbol_unchecked())
                        .finish()
                }
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

impl<T> DerefMut for Root<T> {
    fn deref_mut(&mut self) -> &mut T {
        // Safe because we know Root<T> hasn't been GC'ed
        unsafe { &mut self.0.as_mut().data }
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

    pub(crate) unsafe fn extend_lifetime<'b>(&self) -> Ptr<'b, T> {
        Ptr(self.0, &PhantomData)
    }
}

impl<'a, T> Deref for Ptr<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        // Safe because we know Ptr<'a, T> hasn't been GC'ed
        unsafe { &self.0.as_ref().data }
    }
}

impl<'a, T> DerefMut for Ptr<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        // Safe because we know Ptr<'a, T> hasn't been GC'ed
        unsafe { &mut self.0.as_mut().data }
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
    Str,
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
    pub(crate) fn car(&self) -> ReachableValue<'_> {
        ReachableValue(self.car, &PhantomData)
    }

    pub(crate) fn cdr(&self) -> ReachableValue<'_> {
        ReachableValue(self.cdr, &PhantomData)
    }

    // FIXME: Revisit this when we move to a generational GC
    pub(crate) fn set_cdr(&mut self, value: Value) {
        self.cdr = value;
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
        self.new_object(ObjectType::Cons, Cons { car, cdr })
    }

    pub fn new_str<S>(&self, string: S) -> Ptr<'_, Str>
    where
        S: AsRef<str> + ToString,
    {
        if let Some(str) = self.interner.borrow().get(&string.as_ref()) {
            return Ptr(str.0, &PhantomData);
        }
        let ptr = self.new_object(ObjectType::Str, Str(string.to_string()));
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

    fn new_object<T>(&self, typ: ObjectType, data: T) -> Ptr<'_, T> {
        let object = Box::new(Object {
            header: ObjectHeader {
                next: self.objects.get(),
                typ,
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
