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
pub(crate) struct ValueStore(u64);

impl ValueStore {
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
}

#[derive(Clone, Copy)]
pub struct Value<'a>(ValueStore, &'a PhantomData<()>);

impl<'a> Value<'a> {
    pub(crate) unsafe fn new(value: ValueStore) -> Value<'a> {
        Value(value, &PhantomData)
    }

    pub(crate) unsafe fn extend_lifetime<'b>(self) -> Value<'b> {
        Value(self.0, &PhantomData)
    }

    pub(crate) fn into_inner(self) -> ValueStore {
        self.0
    }

    pub fn null() -> Value<'a> {
        Value(ValueStore(ValueStore::TAG_NULL), &PhantomData)
    }

    pub fn t() -> Value<'a> {
        Value(ValueStore(ValueStore::TAG_TRUE), &PhantomData)
    }

    pub fn f() -> Value<'a> {
        Value(ValueStore(ValueStore::TAG_FALSE), &PhantomData)
    }

    pub fn int(i: i32) -> Value<'a> {
        Value(
            ValueStore(
                (((i as u64) << ValueStore::I32_SHIFT) & ValueStore::I32_MASK)
                    | ValueStore::TAG_INTEGER,
            ),
            &PhantomData,
        )
    }

    pub fn double(f: f64) -> Value<'a> {
        if f.is_nan() {
            Value(ValueStore(ValueStore::NAN_VALUE), &PhantomData)
        } else {
            Value(ValueStore(unsafe { std::mem::transmute(f) }), &PhantomData)
        }
    }

    pub fn cons(ptr: Ptr<'a, Cons>) -> Value<'a> {
        Value::object(ptr, ValueStore::TAG_CONS)
    }

    pub fn str(ptr: Ptr<'a, Str>) -> Value<'a> {
        Value::object(ptr, ValueStore::TAG_STR)
    }

    pub fn symbol(ptr: Ptr<'a, Str>) -> Value<'a> {
        Value::object(ptr, ValueStore::TAG_SYMBOL)
    }

    fn object<T>(ptr: Ptr<'a, T>, tag: u64) -> Value<'a> {
        let raw = ptr.0.as_ptr() as u64;
        debug_assert_eq!((raw & !ValueStore::POINTER_MASK), 0);
        Value(ValueStore(raw | tag), &PhantomData)
    }

    pub fn is_null(&self) -> bool {
        self.0 .0 == ValueStore::TAG_NULL
    }

    pub fn is_t(&self) -> bool {
        self.0 .0 == ValueStore::TAG_TRUE
    }

    pub fn is_f(&self) -> bool {
        self.0 .0 == ValueStore::TAG_FALSE
    }

    pub fn is_int(&self) -> bool {
        (self.0 .0 & (ValueStore::NON_FLOAT_MASK | ValueStore::TYPE_MASK))
            == ValueStore::TAG_INTEGER
    }

    pub fn is_double(&self) -> bool {
        (self.0 .0 & ValueStore::NON_FLOAT_MASK) != ValueStore::NON_FLOAT_MASK
    }

    pub fn is_cons(&self) -> bool {
        (self.0 .0 & (ValueStore::NON_FLOAT_MASK | ValueStore::TYPE_MASK)) == ValueStore::TAG_CONS
    }

    pub fn is_str(&self) -> bool {
        (self.0 .0 & (ValueStore::NON_FLOAT_MASK | ValueStore::TYPE_MASK)) == ValueStore::TAG_STR
    }

    pub fn is_symbol(&self) -> bool {
        (self.0 .0 & (ValueStore::NON_FLOAT_MASK | ValueStore::TYPE_MASK)) == ValueStore::TAG_SYMBOL
    }

    pub fn as_int(&self) -> Option<i32> {
        if self.is_int() {
            Some(unsafe { self.as_int_unchecked() })
        } else {
            None
        }
    }

    pub unsafe fn as_int_unchecked(&self) -> i32 {
        ((self.0 .0 & ValueStore::I32_MASK) >> ValueStore::I32_SHIFT) as i32
    }

    pub fn as_double(&self) -> Option<f64> {
        if self.is_double() {
            Some(unsafe { self.as_double_unchecked() })
        } else {
            None
        }
    }

    pub unsafe fn as_double_unchecked(&self) -> f64 {
        std::mem::transmute(self.0 .0)
    }

    pub fn typ(&self) -> ValueType {
        match self.0 .0 & (ValueStore::NON_FLOAT_MASK | ValueStore::TYPE_MASK) {
            ValueStore::TAG_NULL => ValueType::Null,
            ValueStore::TAG_TRUE => ValueType::True,
            ValueStore::TAG_FALSE => ValueType::False,
            ValueStore::TAG_INTEGER => ValueType::Integer,
            ValueStore::TAG_CONS => ValueType::Cons,
            ValueStore::TAG_STR => ValueType::Str,
            ValueStore::TAG_SYMBOL => ValueType::Symbol,
            _ => {
                if self.is_double() {
                    ValueType::Double
                } else {
                    panic!("unknown value type: {:?}", self.0 .0)
                }
            }
        }
    }

    pub fn as_cons(&self) -> Option<Ptr<'a, Cons>> {
        if self.is_cons() {
            Some(unsafe { self.as_cons_unchecked() })
        } else {
            None
        }
    }

    pub unsafe fn as_cons_unchecked(&self) -> Ptr<'a, Cons> {
        self.as_object_unchecked()
    }

    pub fn as_str(&self) -> Option<Ptr<'a, Str>> {
        if self.is_str() {
            Some(unsafe { self.as_str_unchecked() })
        } else {
            None
        }
    }

    pub unsafe fn as_str_unchecked(&self) -> Ptr<'a, Str> {
        self.as_object_unchecked()
    }

    pub fn as_symbol(&self) -> Option<Ptr<'a, Str>> {
        if self.is_symbol() {
            Some(unsafe { self.as_symbol_unchecked() })
        } else {
            None
        }
    }

    pub unsafe fn as_symbol_unchecked(&self) -> Ptr<'a, Str> {
        self.as_object_unchecked()
    }

    unsafe fn as_object_unchecked<T>(&self) -> Ptr<'a, T> {
        Ptr(
            NonNull::new_unchecked((self.0 .0 & ValueStore::POINTER_MASK) as *mut Object<T>),
            &PhantomData,
        )
    }
}

impl<'a> std::fmt::Debug for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut strukt = f.debug_struct("Value");
        strukt.field("raw", &format_args!("{:#x}", self.0 .0));
        match self.typ() {
            ValueType::Null | ValueType::True | ValueType::False => {
                strukt.field("typ", &self.typ())
            }
            ValueType::Integer => strukt.field("int", unsafe { &self.as_int_unchecked() }),
            ValueType::Double => strukt.field("double", unsafe { &self.as_double_unchecked() }),
            ValueType::Cons => strukt.field("cons", unsafe { &self.as_cons_unchecked() }),
            ValueType::Str => strukt.field("str", unsafe { &self.as_str_unchecked() }),
            ValueType::Symbol => strukt.field("symbol", unsafe { &self.as_symbol_unchecked() }),
        };
        strukt.finish()
    }
}

pub trait MoldObject: std::fmt::Debug {
    const TYPE: ObjectType;
}

#[derive(Debug)]
#[repr(C)]
struct RootMarker {
    previous: NonNull<RootMarker>,
    next: NonNull<RootMarker>,
    object: NonNull<ObjectHeader>,
}

impl RootMarker {
    // This is marked unsafe because the caller has to ensure they call `initialize` before using
    // this RootMarker.
    unsafe fn uninitialized(object: NonNull<ObjectHeader>) -> RootMarker {
        RootMarker {
            object,
            previous: NonNull::dangling(),
            next: NonNull::dangling(),
        }
    }

    fn initialize(&mut self) {
        // Safe because we have a mutable reference to the pointer that we're setting
        unsafe {
            self.previous = NonNull::new_unchecked(self);
            self.next = NonNull::new_unchecked(self);
        }
    }

    fn append_to_list(&mut self, head: &mut RootMarker) {
        unsafe {
            self.previous = head.previous;
            self.next = NonNull::new_unchecked(head);
            head.previous.as_mut().next = NonNull::new_unchecked(self);
            head.previous = NonNull::new_unchecked(self);
        }
    }

    fn unlink(&mut self) {
        // Safe provided these pointers are valid - invariant is maintained across this module.
        unsafe {
            self.previous.as_mut().next = self.next;
            self.next.as_mut().previous = self.previous;
        }
        self.initialize();
    }
}

pub struct Root<T>(NonNull<RootMarker>, PhantomData<T>);

// FIXME: This would be UNSAFE if we use a copying garbage collector, since the underlying object
//        pointer would move.
impl<T> Deref for Root<T> {
    type Target = T;

    fn deref(&self) -> &T {
        // Safe because we know Root<T> hasn't been GC'ed, and Object<T> is #[repr(C)]
        unsafe { &self.0.as_ref().object.cast::<Object<T>>().as_ref().data }
    }
}

// FIXME: This would be UNSAFE if we use a copying garbage collector, since the underlying object
//        pointer would move.
impl<T> DerefMut for Root<T> {
    fn deref_mut(&mut self) -> &mut T {
        // Safe because we know Root<T> hasn't been GC'ed, and Object<T> is #[repr(C)]
        unsafe { &mut self.0.as_mut().object.cast::<Object<T>>().as_mut().data }
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

impl<T> Drop for Root<T> {
    fn drop(&mut self) {
        // Safe because we know Root<T> hasn't been GC'ed
        unsafe { self.0.as_mut().unlink() }
    }
}

pub struct Ptr<'a, T>(NonNull<Object<T>>, &'a PhantomData<()>);

impl<'a, T> Ptr<'a, T> {
    pub fn ptr_eq(&self, other: Self) -> bool {
        std::ptr::eq(self.0.as_ptr(), other.0.as_ptr())
    }

    pub(crate) fn downgrade(self) -> Gc<T> {
        Gc(self.0)
    }

    pub(crate) unsafe fn extend_lifetime<'b>(&self) -> Ptr<'b, T> {
        Ptr(self.0, &PhantomData)
    }

    pub(crate) fn root(self, heap: &Heap) -> Root<T> {
        heap.root(self)
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
pub(crate) struct Gc<T>(NonNull<Object<T>>);

impl<T> Gc<T> {
    pub(crate) unsafe fn to_ptr<'a>(&self) -> Ptr<'a, T> {
        Ptr(self.0, &PhantomData)
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc(self.0)
    }
}

impl<T> Copy for Gc<T> {}

// Gc<Str> needs to be a hash key
impl PartialEq for Gc<Str> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0.as_ptr(), other.0.as_ptr())
    }
}

impl Eq for Gc<Str> {}

impl Hash for Gc<Str> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state)
    }
}

#[derive(Debug)]
pub enum ObjectType {
    Cons,
    Str,
    Chunk,
    Fiber,
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
    car: ValueStore,
    cdr: ValueStore,
}

impl Cons {
    pub(crate) fn car(&self) -> Value<'_> {
        Value(self.car, &PhantomData)
    }

    pub(crate) fn cdr(&self) -> Value<'_> {
        Value(self.cdr, &PhantomData)
    }

    // FIXME: Revisit this when we move to a generational GC
    pub(crate) fn set_cdr(&mut self, value: Value<'_>) {
        self.cdr = value.0;
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

impl MoldObject for Cons {
    const TYPE: ObjectType = ObjectType::Cons;
}

#[derive(Debug)]
pub struct Str(String);

impl Str {
    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

impl MoldObject for Str {
    const TYPE: ObjectType = ObjectType::Str;
}

#[derive(Debug)]
pub struct Heap {
    objects: Cell<*mut ObjectHeader>,
    roots: Box<RefCell<RootMarker>>,
    interner: RefCell<Interner>,
}

impl Heap {
    pub fn new() -> Heap {
        let mut heap = Heap {
            objects: Cell::new(std::ptr::null_mut()),
            interner: RefCell::new(Interner::new()),
            // This is a dummy root marker, so having the object pointer be dangling is okay.
            // This is safe because we call `initialize` right after.
            roots: Box::new(RefCell::new(unsafe {
                RootMarker::uninitialized(NonNull::dangling())
            })),
        };
        heap.roots.get_mut().initialize();
        heap
    }

    pub fn new_cons(&self, car: Value<'_>, cdr: Value<'_>) -> Ptr<'_, Cons> {
        self.new_object(Cons {
            car: car.0,
            cdr: cdr.0,
        })
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

    pub(crate) fn new_object<T>(&self, data: T) -> Ptr<'_, T>
    where
        T: MoldObject,
    {
        let object = Box::new(Object {
            header: ObjectHeader {
                next: self.objects.get(),
                typ: T::TYPE,
            },
            data,
        });
        let object_ptr = Box::into_raw(object);
        // We can do this because Object<T> is #[repr(C)]
        self.objects.set(object_ptr as *mut ObjectHeader);
        // Safe because we just allocated the pointer
        Ptr(unsafe { NonNull::new_unchecked(object_ptr) }, &PhantomData)
    }

    fn root<T>(&self, ptr: Ptr<'_, T>) -> Root<T> {
        // Safe because we call initialize right after. The `cast` only works because Object<T> is
        // #[repr(C)].
        let mut root_marker = Box::new(unsafe { RootMarker::uninitialized(ptr.0.cast()) });
        root_marker.initialize();
        root_marker.append_to_list(&mut *self.roots.borrow_mut());
        // Safe because we just allocated the pointer
        Root(
            unsafe { NonNull::new_unchecked(Box::into_raw(root_marker)) },
            PhantomData,
        )
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
