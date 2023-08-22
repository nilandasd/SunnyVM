use std::cell::Cell;
use std::fmt;
use std::ops::Deref;

use zapalloc::{AllocObject, RawPtr};

use crate::header::TypeList;
use crate::printer::Print;
use crate::ptr_ops::ScopedRef;
use crate::tagged_ptr::{FatPtr, TaggedPtr, Value};

pub trait MutatorScope {}

pub struct ScopedPtr<'guard, T: Sized> {
    value: &'guard T,
}

impl<'guard, T: Sized> ScopedPtr<'guard, T> {
    pub fn new(_guard: &'guard dyn MutatorScope, value: &'guard T) -> ScopedPtr<'guard, T> {
        ScopedPtr { value }
    }

    pub fn as_tagged(&self, guard: &'guard dyn MutatorScope) -> TaggedScopedPtr<'guard>
    where
        FatPtr: From<RawPtr<T>>,
        T: AllocObject<TypeList>,
    {
        TaggedScopedPtr::new(
            guard,
            TaggedPtr::from(FatPtr::from(RawPtr::new(self.value))),
        )
    }
}

impl<'scope, T: Sized> MutatorScope for ScopedPtr<'scope, T> {}

impl<'guard, T: Sized> Clone for ScopedPtr<'guard, T> {
    fn clone(&self) -> ScopedPtr<'guard, T> {
        ScopedPtr { value: self.value }
    }
}

impl<'guard, T: Sized> Copy for ScopedPtr<'guard, T> {}

impl<'guard, T: Sized> Deref for ScopedPtr<'guard, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.value
    }
}

impl<'guard, T: Sized + Print> fmt::Display for ScopedPtr<'guard, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.print(self, f)
    }
}

impl<'guard, T: Sized + Print> fmt::Debug for ScopedPtr<'guard, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.print(self, f)
    }
}

impl<'guard, T: Sized + PartialEq> PartialEq for ScopedPtr<'guard, T> {
    fn eq(&self, rhs: &ScopedPtr<'guard, T>) -> bool {
        self.value == rhs.value
    }
}

#[derive(Clone)]
pub struct CellPtr<T: Sized> {
    inner: Cell<RawPtr<T>>,
}

impl<T: Sized> CellPtr<T> {
    pub fn new_with(source: ScopedPtr<T>) -> CellPtr<T> {
        CellPtr {
            inner: Cell::new(RawPtr::new(source.value)),
        }
    }

    pub fn get<'guard>(&self, guard: &'guard dyn MutatorScope) -> ScopedPtr<'guard, T> {
        ScopedPtr::new(guard, self.inner.get().scoped_ref(guard))
    }

    pub fn set(&self, source: ScopedPtr<T>) {
        self.inner.set(RawPtr::new(source.value))
    }
}

impl<T: Sized> From<ScopedPtr<'_, T>> for CellPtr<T> {
    fn from(ptr: ScopedPtr<T>) -> CellPtr<T> {
        CellPtr::new_with(ptr)
    }
}

#[derive(Copy, Clone)]
pub struct TaggedScopedPtr<'guard> {
    ptr: TaggedPtr,
    value: Value<'guard>,
}

impl<'guard> TaggedScopedPtr<'guard> {
    pub fn new(guard: &'guard dyn MutatorScope, ptr: TaggedPtr) -> TaggedScopedPtr<'guard> {
        TaggedScopedPtr {
            ptr,
            value: FatPtr::from(ptr).as_value(guard),
        }
    }

    pub fn value(&self) -> Value<'guard> {
        self.value
    }

    pub fn get_ptr(&self) -> TaggedPtr {
        self.ptr
    }
}

impl<'scope> MutatorScope for TaggedScopedPtr<'scope> {}

impl<'guard> Deref for TaggedScopedPtr<'guard> {
    type Target = Value<'guard>;

    fn deref(&self) -> &Value<'guard> {
        &self.value
    }
}

impl<'guard> fmt::Display for TaggedScopedPtr<'guard> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<'guard> fmt::Debug for TaggedScopedPtr<'guard> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<'guard> PartialEq for TaggedScopedPtr<'guard> {
    fn eq(&self, rhs: &TaggedScopedPtr<'guard>) -> bool {
        self.ptr == rhs.ptr
    }
}

#[derive(Clone)]
pub struct TaggedCellPtr {
    inner: Cell<TaggedPtr>,
}

impl TaggedCellPtr {
    pub fn new_nil() -> TaggedCellPtr {
        TaggedCellPtr {
            inner: Cell::new(TaggedPtr::nil()),
        }
    }

    pub fn new_with(source: TaggedScopedPtr) -> TaggedCellPtr {
        TaggedCellPtr {
            inner: Cell::new(TaggedPtr::from(source.ptr)),
        }
    }

    pub fn new_ptr(source: TaggedPtr) -> TaggedCellPtr {
        TaggedCellPtr {
            inner: Cell::new(source),
        }
    }

    pub fn get<'guard>(&self, guard: &'guard dyn MutatorScope) -> TaggedScopedPtr<'guard> {
        TaggedScopedPtr::new(guard, self.inner.get())
    }

    pub fn set(&self, source: TaggedScopedPtr) {
        self.inner.set(TaggedPtr::from(source.ptr))
    }

    pub fn copy_from(&self, other: &TaggedCellPtr) {
        self.inner.set(other.inner.get());
    }

    pub fn is_nil(&self) -> bool {
        self.inner.get().is_nil()
    }

    pub fn set_to_nil(&self) {
        self.inner.set(TaggedPtr::nil())
    }

    pub fn set_to_ptr(&self, ptr: TaggedPtr) {
        self.inner.set(ptr)
    }

    pub fn get_ptr(&self) -> TaggedPtr {
        self.inner.get()
    }
}

impl From<TaggedScopedPtr<'_>> for TaggedCellPtr {
    fn from(ptr: TaggedScopedPtr) -> TaggedCellPtr {
        TaggedCellPtr::new_with(ptr)
    }
}
