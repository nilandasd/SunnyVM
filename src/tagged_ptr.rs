use std::fmt;
use std::ptr::NonNull;

use zapalloc::{AllocRaw, RawPtr};

use crate::array::{ArrayU8, ArrayU16, ArrayU32, ArrayU64};
use crate::dict::Dict;
use crate::function::{Function, Closure};
use crate::list::List;
use crate::memory::{HeapStorage, SymbolId};
use crate::number::NumberObject;
use crate::ptr_ops::{get_tag, ScopedRef, Tagged, TAG_NUMBER, TAG_OBJECT, TAG_SYMBOL};
use crate::printer::Print;
use crate::safe_ptr::{MutatorScope, ScopedPtr};
use crate::text::Text;
use crate::upvalue::Upvalue;

#[derive(Copy, Clone)]
pub enum Value<'guard> {
    ArrayU8(ScopedPtr<'guard, ArrayU8>),
    ArrayU16(ScopedPtr<'guard, ArrayU16>),
    ArrayU32(ScopedPtr<'guard, ArrayU32>),
    ArrayU64(ScopedPtr<'guard, ArrayU64>),
    Dict(ScopedPtr<'guard, Dict>),
    Function(ScopedPtr<'guard, Function>),
    Closure(ScopedPtr<'guard, Closure>),
    List(ScopedPtr<'guard, List>),
    Nil,
    Number(isize),
    Symbol(SymbolId),
    NumberObject(ScopedPtr<'guard, NumberObject>),
    Text(ScopedPtr<'guard, Text>),
    Upvalue(ScopedPtr<'guard, Upvalue>),
}

impl<'guard> fmt::Display for Value<'guard> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Number(n) => write!(f, "{}", *n),
            Value::Symbol(n) => write!(f, "SymbolId: {}", *n),
            Value::Text(t) => t.print(self, f),
            Value::List(a) => a.print(self, f),
            Value::ArrayU8(a) => a.print(self, f),
            Value::ArrayU16(a) => a.print(self, f),
            Value::ArrayU32(a) => a.print(self, f),
            Value::ArrayU64(a) => a.print(self, f),
            Value::Dict(d) => d.print(self, f),
            Value::Function(n) => n.print(self, f),
            Value::Closure(n) => n.print(self, f),
            Value::Upvalue(_) => write!(f, "Upvalue"),
            _ => write!(f, "<unidentified-object-type>"),
        }
    }
}

impl<'guard> fmt::Debug for Value<'guard> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::ArrayU8(a) => a.debug(self, f),
            Value::ArrayU16(a) => a.debug(self, f),
            Value::ArrayU32(a) => a.debug(self, f),
            Value::ArrayU64(a) => a.debug(self, f),
            Value::Dict(d) => d.debug(self, f),
            Value::Function(n) => n.debug(self, f),
            Value::List(a) => a.debug(self, f),
            Value::Nil => write!(f, "nil"),
            Value::Number(n) => write!(f, "{}", *n),
            Value::Symbol(n) => write!(f, "SymbolId: {}", *n),
            Value::Text(t) => t.debug(self, f),
            Value::Upvalue(_) => write!(f, "Upvalue"),
            _ => write!(f, "<unidentified-object-type>"),
        }
    }
}

impl<'guard> MutatorScope for Value<'guard> {}

#[derive(Copy, Clone)]
pub enum FatPtr {
    ArrayU8(RawPtr<ArrayU8>),
    ArrayU16(RawPtr<ArrayU16>),
    ArrayU32(RawPtr<ArrayU32>),
    ArrayU64(RawPtr<ArrayU64>),
    Dict(RawPtr<Dict>),
    Function(RawPtr<Function>),
    Closure(RawPtr<Closure>),
    List(RawPtr<List>),
    Nil,
    Number(isize),
    Symbol(SymbolId),
    NumberObject(RawPtr<NumberObject>),
    Text(RawPtr<Text>),
    Upvalue(RawPtr<Upvalue>),
}

impl FatPtr {
    pub fn as_value<'guard>(&self, guard: &'guard dyn MutatorScope) -> Value<'guard> {
        match self {
            FatPtr::ArrayU8(raw_ptr) => {
                Value::ArrayU8(ScopedPtr::new(guard, raw_ptr.scoped_ref(guard)))
            }
            FatPtr::ArrayU16(raw_ptr) => {
                Value::ArrayU16(ScopedPtr::new(guard, raw_ptr.scoped_ref(guard)))
            }
            FatPtr::ArrayU32(raw_ptr) => {
                Value::ArrayU32(ScopedPtr::new(guard, raw_ptr.scoped_ref(guard)))
            }
            FatPtr::ArrayU64(raw_ptr) => {
                Value::ArrayU64(ScopedPtr::new(guard, raw_ptr.scoped_ref(guard)))
            }
            FatPtr::Dict(raw_ptr) => Value::Dict(ScopedPtr::new(guard, raw_ptr.scoped_ref(guard))),
            FatPtr::Function(raw_ptr) => {
                Value::Function(ScopedPtr::new(guard, raw_ptr.scoped_ref(guard)))
            },
            FatPtr::Closure(raw_ptr) => {
                Value::Closure(ScopedPtr::new(guard, raw_ptr.scoped_ref(guard)))
            },
            FatPtr::List(raw_ptr) => Value::List(ScopedPtr::new(guard, raw_ptr.scoped_ref(guard))),
            FatPtr::Nil => Value::Nil,
            FatPtr::Number(num) => Value::Number(*num),
            FatPtr::Symbol(symbol_id) => Value::Symbol(*symbol_id),
            FatPtr::NumberObject(raw_ptr) => {
                Value::NumberObject(ScopedPtr::new(guard, raw_ptr.scoped_ref(guard)))
            }
            FatPtr::Text(raw_ptr) => Value::Text(ScopedPtr::new(guard, raw_ptr.scoped_ref(guard))),
            FatPtr::Upvalue(raw_ptr) => {
                Value::Upvalue(ScopedPtr::new(guard, raw_ptr.scoped_ref(guard)))
            }
        }
    }
}

/// Implement `From<RawPtr<T>> for FatPtr` for the given FatPtr discriminant and the given `T`
macro_rules! fatptr_from_rawptr {
    ($F:tt, $T:ty) => {
        impl From<RawPtr<$T>> for FatPtr {
            fn from(ptr: RawPtr<$T>) -> FatPtr {
                FatPtr::$F(ptr)
            }
        }
    };
}

fatptr_from_rawptr!(ArrayU8, ArrayU8);
fatptr_from_rawptr!(ArrayU16, ArrayU16);
fatptr_from_rawptr!(ArrayU32, ArrayU32);
fatptr_from_rawptr!(ArrayU64, ArrayU64);
fatptr_from_rawptr!(Dict, Dict);
fatptr_from_rawptr!(Function, Function);
fatptr_from_rawptr!(List, List);
fatptr_from_rawptr!(NumberObject, NumberObject);
fatptr_from_rawptr!(Text, Text);
fatptr_from_rawptr!(Upvalue, Upvalue);
fatptr_from_rawptr!(Closure, Closure);

impl From<isize> for FatPtr {
    fn from(num: isize) -> FatPtr {
        // TODO big numbers
        FatPtr::Number(num)
    }
}

impl From<TaggedPtr> for FatPtr {
    fn from(ptr: TaggedPtr) -> FatPtr {
        ptr.into_fat_ptr()
    }
}

impl PartialEq for FatPtr {
    fn eq(&self, other: &FatPtr) -> bool {
        use self::FatPtr::*;

        match (*self, *other) {
            (Nil, Nil) => true,
            (Number(i), Number(j)) => i == j,
            (NumberObject(p), NumberObject(q)) => p == q,
            _ => false,
        }
    }
}

#[derive(Copy, Clone)]
pub union TaggedPtr {
    tag: usize,
    number: isize,
    symbol: SymbolId,
    object: NonNull<()>,
}

impl TaggedPtr {
    pub fn nil() -> TaggedPtr {
        TaggedPtr { tag: 0 }
    }

    pub fn is_nil(&self) -> bool {
        unsafe { self.tag == 0 }
    }

    pub fn object<T>(ptr: RawPtr<T>) -> TaggedPtr {
        TaggedPtr {
            object: ptr.tag(TAG_OBJECT).cast::<()>(),
        }
    }

    pub fn symbol(value: SymbolId) -> TaggedPtr {
        TaggedPtr {
            symbol: (((value as usize) << 2) | TAG_SYMBOL) as SymbolId,
        }
    }

    pub fn literal_symbol(value: u16) -> TaggedPtr {
        TaggedPtr {
            symbol: (((value as usize) << 2) | TAG_SYMBOL) as SymbolId,
        }
    }

    pub fn number(value: isize) -> TaggedPtr {
        TaggedPtr {
            number: (((value as usize) << 2) | TAG_NUMBER) as isize,
        }
    }

    pub fn literal_integer(value: i16) -> TaggedPtr {
        TaggedPtr {
            number: (((value as usize) << 2) | TAG_NUMBER) as isize,
        }
    }

    fn into_fat_ptr(&self) -> FatPtr {
        unsafe {
            if self.tag == 0 {
                FatPtr::Nil
            } else {
                match get_tag(self.tag) {
                    TAG_NUMBER => FatPtr::Number(self.number >> 2),
                    TAG_SYMBOL => FatPtr::Symbol(self.symbol >> 2),
                    TAG_OBJECT => {
                        let untyped_object_ptr = RawPtr::untag(self.object).as_untyped();
                        let header_ptr = HeapStorage::get_header(untyped_object_ptr);

                        header_ptr.as_ref().get_object_fatptr()
                    }

                    _ => panic!("Invalid TaggedPtr type tag!"),
                }
            }
        }
    }
}

impl From<FatPtr> for TaggedPtr {
    fn from(ptr: FatPtr) -> TaggedPtr {
        match ptr {
            FatPtr::ArrayU8(raw) => TaggedPtr::object(raw),
            FatPtr::ArrayU16(raw) => TaggedPtr::object(raw),
            FatPtr::ArrayU32(raw) => TaggedPtr::object(raw),
            FatPtr::ArrayU64(raw) => TaggedPtr::object(raw),
            FatPtr::Dict(raw) => TaggedPtr::object(raw),
            FatPtr::Function(raw) => TaggedPtr::object(raw),
            FatPtr::Closure(raw) => TaggedPtr::object(raw),
            FatPtr::List(raw) => TaggedPtr::object(raw),
            FatPtr::Nil => TaggedPtr::nil(),
            FatPtr::Number(value) => TaggedPtr::number(value),
            FatPtr::Symbol(value) => TaggedPtr::symbol(value),
            FatPtr::NumberObject(raw) => TaggedPtr::object(raw),
            FatPtr::Text(raw) => TaggedPtr::object(raw),
            FatPtr::Upvalue(raw) => TaggedPtr::object(raw),
        }
    }
}

impl PartialEq for TaggedPtr {
    fn eq(&self, other: &TaggedPtr) -> bool {
        unsafe { self.tag == other.tag }
    }
}
