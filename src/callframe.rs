use std::cell::Cell;

use crate::safe_ptr::CellPtr;
use crate::function::Function;
use crate::array::{Array, ArraySize};
use crate::safe_ptr::{ScopedPtr, MutatorScope};

#[derive(Clone)]
pub struct CallFrame {
    pub function: CellPtr<Function>,
    pub ip: Cell<ArraySize>,
    pub base: ArraySize,
}

impl CallFrame {
    pub fn new_main<'guard>(main_fn: ScopedPtr<'guard, Function>) -> CallFrame {
        CallFrame {
            function: CellPtr::new_with(main_fn),
            ip: Cell::new(0),
            base: 0,
        }
    }

    pub fn new<'guard>(
        function: ScopedPtr<'guard, Function>,
        ip: ArraySize,
        base: ArraySize,
    ) -> CallFrame {
        CallFrame {
            function: CellPtr::new_with(function),
            ip: Cell::new(ip),
            base,
        }
    }

    pub fn as_string<'guard>(&self, guard: &'guard dyn MutatorScope) -> String {
        let function = self.function.get(guard);
        format!("in {}", function)
    }
}

pub type CallFrameList = Array<CallFrame>;
