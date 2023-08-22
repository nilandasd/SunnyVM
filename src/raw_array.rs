use std::mem::size_of;
use std::ptr::NonNull;
use std::slice::from_raw_parts_mut;

use zapalloc::ArraySize;

use crate::error::{ErrorKind, RuntimeError};
use crate::memory::MutatorView;

pub const DEFAULT_ARRAY_SIZE: ArraySize = 8;

pub fn default_array_growth(capacity: ArraySize) -> Result<ArraySize, RuntimeError> {
    if capacity == 0 {
        Ok(DEFAULT_ARRAY_SIZE)
    } else {
        capacity
            .checked_add(capacity / 2)
            .ok_or(RuntimeError::new(ErrorKind::BadAllocationRequest))
    }
}

pub struct RawArray<T: Sized> {
    capacity: ArraySize,
    ptr: Option<NonNull<T>>,
}

impl<T: Sized> Clone for RawArray<T> {
    fn clone(&self) -> Self {
        RawArray {
            capacity: self.capacity,
            ptr: self.ptr,
        }
    }
}

impl<T: Sized> Copy for RawArray<T> {}

impl<T: Sized> RawArray<T> {
    pub fn new() -> RawArray<T> {
        RawArray {
            capacity: 0,
            ptr: None,
        }
    }

    pub fn with_capacity<'scope>(
        mem: &'scope MutatorView,
        capacity: u32,
    ) -> Result<RawArray<T>, RuntimeError> {
        let capacity_bytes = capacity
            .checked_mul(size_of::<T>() as ArraySize)
            .ok_or(RuntimeError::new(ErrorKind::BadAllocationRequest))?;

        Ok(RawArray {
            capacity,
            ptr: NonNull::new(mem.alloc_array(capacity_bytes)?.as_ptr() as *mut T),
        })
    }

    // TODO the inner implementation of this should live in the allocator API to make
    pub fn resize<'scope>(
        &mut self,
        mem: &'scope MutatorView,
        new_capacity: u32,
    ) -> Result<(), RuntimeError> {
        if new_capacity == 0 {
            self.capacity = 0;
            self.ptr = None;
            return Ok(());
        }

        match self.ptr {
            Some(old_ptr) => {
                let old_capacity_bytes = size_of::<T>() as ArraySize * self.capacity;
                let old_ptr = old_ptr.as_ptr();

                let new_capacity_bytes = new_capacity
                    .checked_mul(size_of::<T>() as ArraySize)
                    .ok_or(RuntimeError::new(ErrorKind::BadAllocationRequest))?;

                let new_ptr = mem.alloc_array(new_capacity_bytes)?.as_ptr() as *mut T;

                let (old_slice, new_slice) = unsafe {
                    (
                        from_raw_parts_mut(old_ptr as *mut u8, old_capacity_bytes as usize),
                        from_raw_parts_mut(new_ptr as *mut u8, new_capacity_bytes as usize),
                    )
                };

                for (src, dest) in old_slice.iter().zip(new_slice) {
                    *dest = *src;
                }

                self.ptr = NonNull::new(new_ptr);
                self.capacity = new_capacity;

                Ok(())
            }

            None => {
                *self = Self::with_capacity(mem, new_capacity)?;
                Ok(())
            }
        }
    }

    pub fn capacity(&self) -> ArraySize {
        self.capacity
    }

    pub fn as_ptr(&self) -> Option<*const T> {
        match self.ptr {
            Some(ptr) => Some(ptr.as_ptr()),
            None => None,
        }
    }
}
