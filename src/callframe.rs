use std::cell::Cell;

use crate::container::Container;
use crate::list::List;
use crate::safe_ptr::CellPtr;
use crate::function::Function;
use crate::array::{Array, ArraySize};
use crate::safe_ptr::{ScopedPtr, MutatorScope};
use crate::error::RuntimeError;
use crate::memory::MutatorView;

#[derive(Clone)]
pub struct CallFrame {
    pub function: CellPtr<Function>,
    pub ip: Cell<ArraySize>,
    pub base: ArraySize,
    pub overflow: Option<CellPtr<List>>,
}

impl CallFrame {
    pub fn new_main<'guard>(main_fn: ScopedPtr<'guard, Function>, mem: &'guard MutatorView) -> Result<CallFrame, RuntimeError> {
        let overflow_capacity = (*main_fn).overflow_capacity();
        let overflow = if overflow_capacity == 0 {
            None
        } else {
            let overflow_list = List::alloc_with_capacity(mem, overflow_capacity as u32)?;
            Some(CellPtr::new_with(overflow_list))
        };


        Ok(CallFrame {
            function: CellPtr::new_with(main_fn),
            ip: Cell::new(0),
            base: 0,
            overflow,
        })
    }

    pub fn new<'guard>(
        function: ScopedPtr<'guard, Function>,
        ip: ArraySize,
        base: ArraySize,
        mem: &'guard MutatorView,
    ) -> Result<CallFrame, RuntimeError> {
        let overflow_capacity = (*function).overflow_capacity();
        let overflow = if overflow_capacity == 0 {
            None
        } else {
            Some(CellPtr::new_with(List::alloc_with_capacity(mem, overflow_capacity as u32)?))
        };

        Ok(CallFrame {
            function: CellPtr::new_with(function),
            ip: Cell::new(ip),
            base,
            overflow,
        })
    }

    pub fn overflow<'guard>(&self, guard: &'guard dyn MutatorScope) -> Option<ScopedPtr<'guard, List>> {
        match &self.overflow {
            None => None,
            Some(cell_ptr) => Some(cell_ptr.get(guard)),
        }
    }

    pub fn as_string<'guard>(&self, guard: &'guard dyn MutatorScope) -> String {
        let function = self.function.get(guard);
        format!("in {}", function)
    }
}

pub type CallFrameList = Array<CallFrame>;
