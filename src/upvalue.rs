use std::cell::Cell;

use crate::array::ArraySize;
use crate::safe_ptr::{TaggedCellPtr, ScopedPtr, MutatorScope, TaggedScopedPtr};
use crate::memory::MutatorView;
use crate::error::RuntimeError;
use crate::list::List;
use crate::container::{IndexedContainer, IndexedAnyContainer};
use crate::tagged_ptr::{TaggedPtr, Value};

// A closure upvalue as generally described by Lua 5.1 implementation.
// There is one main difference - in the Lua (and Crafting Interpreters) documentation, an upvalue
// is closed by pointing the `location` pointer at the `closed` pointer directly in the struct.
// This isn't a good idea _here_ because a stack location may be invalidated by the stack List
// object being reallocated. This VM doesn't support pointers into objects.
#[derive(Clone)]
pub struct Upvalue {
    // Upvalue location can't be a pointer because it would be a pointer into the dynamically
    // alloocated stack List - the pointer would be invalidated if the stack gets reallocated.
    value: TaggedCellPtr,
    closed: Cell<bool>,
    location: ArraySize,
}

impl Upvalue {
    // Allocate a new Upvalue on the heap. The absolute stack index of the object must be
    // provided.
    pub fn alloc<'guard>(
        mem: &'guard MutatorView,
        location: ArraySize,
    ) -> Result<ScopedPtr<'guard, Upvalue>, RuntimeError> {
        mem.alloc(Upvalue {
            value: TaggedCellPtr::new_nil(),
            closed: Cell::new(false),
            location,
        })
    }

    pub fn get<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        stack: ScopedPtr<'guard, List>,
    ) -> Result<TaggedPtr, RuntimeError> {
        match self.closed.get() {
            true => Ok(self.value.get_ptr()),
            false => Ok(IndexedContainer::get(&*stack, guard, self.location)?.get_ptr()),
        }
    }

    pub fn set<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        stack: ScopedPtr<'guard, List>,
        ptr: TaggedPtr,
    ) -> Result<(), RuntimeError> {
        match self.closed.get() {
            true => self.value.set_to_ptr(ptr),
            false => {
                IndexedContainer::set(&*stack, guard, self.location, TaggedCellPtr::new_ptr(ptr))?
            }
        };
        Ok(())
    }

    pub fn close<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        stack: ScopedPtr<'guard, List>,
    ) -> Result<(), RuntimeError> {
        let ptr = IndexedContainer::get(&*stack, guard, self.location)?.get_ptr();
        self.value.set_to_ptr(ptr);
        self.closed.set(true);
        Ok(())
    }
}

pub fn env_upvalue_lookup<'guard>(
    guard: &'guard dyn MutatorScope,
    closure_env: TaggedScopedPtr<'guard>,
    upvalue_id: u8,
) -> Result<ScopedPtr<'guard, Upvalue>, RuntimeError> {
    match *closure_env {
        Value::List(env) => {
            let upvalue_ptr = IndexedAnyContainer::get(&*env, guard, upvalue_id as ArraySize)?;

            match *upvalue_ptr {
                Value::Upvalue(upvalue) => Ok(upvalue),
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}
