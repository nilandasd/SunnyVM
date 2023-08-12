use std::convert::TryFrom;
use std::fmt;
use std::ops;

use crate::memory::MutatorView;
use crate::safe_ptr::ScopedPtr;
use crate::array::{Array, ArrayU64};
use crate::printer::Print;
use crate::safe_ptr::{MutatorScope, CellPtr};
use crate::error::RuntimeError;
use crate::container::{StackContainer, Container};

pub const TAG_NUM_MAX: isize = isize::MAX / 4;
pub const TAG_NUM_MIN: isize = isize::MIN / 4;

pub struct NumberObject {
    data: CellPtr<ArrayU64>,
    negative: bool,
}

impl NumberObject {
    pub fn alloc<'guard>(
        integer: isize,
        mem: &'guard MutatorView,
    ) -> Result<ScopedPtr<'guard, NumberObject>, RuntimeError> {
        let data = Array::alloc_with_capacity(mem, 1)?;
        let uint = if integer < 0 {
            u64::try_from(integer * -1)
        } else {
            u64::try_from(integer)
        }.unwrap();

        data.push(mem, uint)?;

        mem.alloc(
            NumberObject {
                data: CellPtr::new_with(data),
                negative: integer < 0,
            }
        )
    }

    pub fn clone<'guard>(
        num_obj: &NumberObject,
        mem: &'guard MutatorView,
    ) -> Result<ScopedPtr<'guard, NumberObject>, RuntimeError> {
        let data = Array::alloc_clone(mem, num_obj.data.get(mem))?;

        mem.alloc(
            NumberObject {
                data: CellPtr::new_with(data),
                negative: num_obj.negative,
            }
        )
    }

    fn add<'guard>(&self, rhs: NumberObject, mem: &'guard MutatorView) -> Result<ScopedPtr<'guard, NumberObject>, RuntimeError> {
        let result = NumberObject::clone(self, mem)?;
        let index = 0;

        todo!()
    }
}

impl Print for NumberObject {
    fn print<'guard>(
        &self,
        _guard: &'guard dyn MutatorScope,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        write!(f, "NumberObject(nan)")
    }
}


