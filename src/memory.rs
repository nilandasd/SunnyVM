use zapalloc::{ZapHeap, RawPtr, AllocObject, AllocRaw, ArraySize};

use crate::error::RuntimeError;
use crate::header::{ObjectHeader, TypeList};
use crate::ptr_ops::ScopedRef;
use crate::safe_ptr::{MutatorScope, ScopedPtr, TaggedScopedPtr};
use crate::symbol_map::SymbolMap;
use crate::tagged_ptr::{FatPtr, TaggedPtr};

pub struct MutatorView<'memory> {
    heap: &'memory Heap,
}

impl<'memory> MutatorView<'memory> {
    fn new(mem: &'memory Memory) -> MutatorView<'memory> {
        MutatorView { heap: &mem.heap }
    }

    pub fn lookup_sym(&self, name: &str) -> TaggedScopedPtr<'_> {
        TaggedScopedPtr::new(self, self.heap.lookup_sym(name))
    }

    pub fn alloc<T>(&self, object: T) -> Result<ScopedPtr<'_, T>, RuntimeError>
    where
        T: AllocObject<TypeList>,
    {
        Ok(ScopedPtr::new(
            self,
            self.heap.alloc(object)?.scoped_ref(self),
        ))
    }

    pub fn alloc_tagged<T>(&self, object: T) -> Result<TaggedScopedPtr<'_>, RuntimeError>
    where
        FatPtr: From<RawPtr<T>>,
        T: AllocObject<TypeList>,
    {
        Ok(TaggedScopedPtr::new(self, self.heap.alloc_tagged(object)?))
    }

    pub fn alloc_array(&self, capacity: ArraySize) -> Result<RawPtr<u8>, RuntimeError> {
        self.heap.alloc_array(capacity)
    }

    pub fn nil(&self) -> TaggedScopedPtr<'_> {
        TaggedScopedPtr::new(self, TaggedPtr::nil())
    }
}

impl<'memory> MutatorScope for MutatorView<'memory> {}

pub type HeapStorage = ZapHeap<ObjectHeader>;

struct Heap {
    heap: HeapStorage,
    syms: SymbolMap,
}

impl Heap {
    fn new() -> Heap {
        Heap {
            heap: HeapStorage::new(),
            syms: SymbolMap::new(),
        }
    }

    fn lookup_sym(&self, name: &str) -> TaggedPtr {
        TaggedPtr::symbol(self.syms.lookup(name))
    }

    fn alloc<T>(&self, object: T) -> Result<RawPtr<T>, RuntimeError>
    where
        T: AllocObject<TypeList>,
    {
        Ok(self.heap.alloc(object)?)
    }

    fn alloc_tagged<T>(&self, object: T) -> Result<TaggedPtr, RuntimeError>
    where
        FatPtr: From<RawPtr<T>>,
        T: AllocObject<TypeList>,
    {
        Ok(TaggedPtr::from(FatPtr::from(self.heap.alloc(object)?)))
    }

    fn alloc_array(&self, capacity: ArraySize) -> Result<RawPtr<u8>, RuntimeError> {
        Ok(self.heap.alloc_array(capacity)?)
    }
}

pub struct Memory {
    heap: Heap,
}

impl Memory {
    pub fn new() -> Memory {
        Memory { heap: Heap::new() }
    }

    pub fn mutate<M: Mutator>(&self, m: &M, input: M::Input) -> Result<M::Output, RuntimeError> {
        let mut guard = MutatorView::new(self);
        m.run(&mut guard, input)
    }
}

pub trait Mutator: Sized {
    type Input;
    type Output;

    fn run(&self, mem: &MutatorView, input: Self::Input) -> Result<Self::Output, RuntimeError>;

    // TODO
    // function to return iterator that iterates over roots
}
// ANCHOR_END: DefMutator
