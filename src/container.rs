// TODO iterators/views
use zapalloc::ArraySize;

use crate::error::RuntimeError;
use crate::memory::MutatorView;
use crate::safe_ptr::{MutatorScope, ScopedPtr, TaggedCellPtr, TaggedScopedPtr};

// Base container-type trait. All container types are subtypes of `Container`.
//
// All container operations _must_ follow interior mutability only rules.
// Because there are no compile-time mutable aliasing guarantees, there can be no references
// into arrays at all, unless there can be a guarantee that the array memory will not be
// reallocated.
//
// `T` cannot be restricted to `Copy` because of the use of `Cell` for interior mutability.
pub trait Container<T: Sized + Clone>: Sized {
    fn new() -> Self;

    // TODO: this may not make sense for tree types
    fn with_capacity<'guard>(
        mem: &'guard MutatorView,
        capacity: ArraySize,
    ) -> Result<Self, RuntimeError>;

    fn clear<'guard>(&self, mem: &'guard MutatorView) -> Result<(), RuntimeError>;

    fn length(&self) -> ArraySize;
}

pub trait FillContainer<T: Sized + Clone>: Container<T> {
    fn fill<'guard>(
        &self,
        mem: &'guard MutatorView,
        size: ArraySize,
        item: T,
    ) -> Result<(), RuntimeError>;
}

pub trait FillAnyContainer: FillContainer<TaggedCellPtr> {
    fn fill<'guard>(
        &self,
        mem: &'guard MutatorView,
        size: ArraySize,
        item: TaggedScopedPtr<'guard>,
    ) -> Result<(), RuntimeError>;
}

pub trait StackContainer<T: Sized + Clone>: Container<T> {
    fn push<'guard>(&self, mem: &'guard MutatorView, item: T) -> Result<(), RuntimeError>;
    fn pop<'guard>(&self, _guard: &'guard dyn MutatorScope) -> Result<T, RuntimeError>;
    fn top<'guard>(&self, _guard: &'guard dyn MutatorScope) -> Result<T, RuntimeError>;
}

pub trait StackAnyContainer: StackContainer<TaggedCellPtr> {
    fn push<'guard>(
        &self,
        mem: &'guard MutatorView,
        item: TaggedScopedPtr<'guard>,
    ) -> Result<(), RuntimeError>;

    fn pop<'guard>(
        &self,
        _guard: &'guard dyn MutatorScope,
    ) -> Result<TaggedScopedPtr<'guard>, RuntimeError>;

    fn top<'guard>(
        &self,
        _guard: &'guard dyn MutatorScope,
    ) -> Result<TaggedScopedPtr<'guard>, RuntimeError>;
}

pub trait IndexedContainer<T: Sized + Clone>: Container<T> {
    fn get<'guard>(
        &self,
        _guard: &'guard dyn MutatorScope,
        index: ArraySize,
    ) -> Result<T, RuntimeError>;

    fn set<'guard>(
        &self,
        _guard: &'guard dyn MutatorScope,
        index: ArraySize,
        item: T,
    ) -> Result<(), RuntimeError>;
}

pub trait SliceableContainer<T: Sized + Clone>: IndexedContainer<T> {
    // This function allows access to the interior of a container as a slice by way of a
    // function, permitting direct access to the memory locations of objects in the container
    // for the lifetime of the closure call.
    //
    // It is important to understand that the 'guard lifetime is not the same safe duration
    // as the slice lifetime - the slice may be invalidated during the 'guard lifetime
    // by operations on the container that cause reallocation.
    //
    // To prevent the function from modifying the container outside of the slice reference,
    // the implementing container must maintain a RefCell-style flag to catch runtime
    // container modifications that would render the slice invalid or cause undefined
    // behavior.
    fn access_slice<'guard, F, R>(&self, _guard: &'guard dyn MutatorScope, f: F) -> R
    where
        F: FnOnce(&mut [T]) -> R;
}

pub trait IndexedAnyContainer: IndexedContainer<TaggedCellPtr> {
    fn get<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        index: ArraySize,
    ) -> Result<TaggedScopedPtr<'guard>, RuntimeError>;

    fn set<'guard>(
        &self,
        _guard: &'guard dyn MutatorScope,
        index: ArraySize,
        item: TaggedScopedPtr<'guard>,
    ) -> Result<(), RuntimeError>;
}

// Hashable-indexed interface. Objects used as keys must implement Hashable.
pub trait HashIndexedAnyContainer {
    fn lookup<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        key: TaggedScopedPtr,
    ) -> Result<TaggedScopedPtr<'guard>, RuntimeError>;

    fn assoc<'guard>(
        &self,
        mem: &'guard MutatorView,
        key: TaggedScopedPtr<'guard>,
        value: TaggedScopedPtr<'guard>,
    ) -> Result<(), RuntimeError>;

    fn dissoc<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        key: TaggedScopedPtr,
    ) -> Result<TaggedScopedPtr<'guard>, RuntimeError>;

    fn exists<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        key: TaggedScopedPtr,
    ) -> Result<bool, RuntimeError>;
}

// Replace the contents of a container with the values in the slice
pub trait ContainerFromSlice<T: Sized + Clone>: Container<T> {
    fn from_slice<'guard>(
        mem: &'guard MutatorView,
        data: &[T],
    ) -> Result<ScopedPtr<'guard, Self>, RuntimeError>;
}

// Replace the contents of a container with the values in the slice
pub trait AnyContainerFromSlice: Container<TaggedCellPtr> {
    fn from_slice<'guard>(
        mem: &'guard MutatorView,
        data: &[TaggedScopedPtr<'guard>],
    ) -> Result<ScopedPtr<'guard, Self>, RuntimeError>;
}

// The implementor represents mutable changes via an internal version count
// such that the use of any references to an older version return an error
pub trait VersionedContainer<T: Sized + Clone>: Container<T> {}

pub trait ImmutableContainer<T: Sized + Clone>: Container<T> {}
