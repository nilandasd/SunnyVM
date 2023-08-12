use std::ptr::NonNull;

use zapalloc::RawPtr;

use crate::safe_ptr::MutatorScope;

pub trait AsNonNull {
    fn non_null_ptr(&self) -> NonNull<Self> {
        unsafe { NonNull::new_unchecked(self as *const Self as *mut Self) }
    }
}

const TAG_MASK: usize = 0x3;
pub const TAG_SYMBOL: usize = 0x1;
pub const TAG_OBJECT: usize = 0x2;
pub const TAG_NUMBER: usize = 0x3;
const PTR_MASK: usize = !0x3;

pub fn get_tag(tagged_word: usize) -> usize {
    tagged_word & TAG_MASK
}

pub trait Tagged<T> {
    fn tag(self, tag: usize) -> NonNull<T>;
    fn untag(from: NonNull<T>) -> RawPtr<T>;
}

impl<T> Tagged<T> for RawPtr<T> {
    fn tag(self, tag: usize) -> NonNull<T> {
        unsafe { NonNull::new_unchecked((self.as_word() | tag) as *mut T) }
    }

    fn untag(from: NonNull<T>) -> RawPtr<T> {
        RawPtr::new((from.as_ptr() as usize & PTR_MASK) as *const T)
    }
}

pub trait ScopedRef<T> {
    fn scoped_ref<'scope>(&self, guard: &'scope dyn MutatorScope) -> &'scope T;
}

impl<T> ScopedRef<T> for RawPtr<T> {
    fn scoped_ref<'scope>(&self, _guard: &'scope dyn MutatorScope) -> &'scope T {
        unsafe { &*self.as_ptr() }
    }
}
