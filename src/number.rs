use std::convert::TryFrom;
use std::cell::Cell;
use std::fmt;

use crate::memory::MutatorView;
use crate::safe_ptr::ScopedPtr;
use crate::array::{Array, ArrayU64};
use crate::printer::Print;
use crate::safe_ptr::{MutatorScope, CellPtr};
use crate::error::RuntimeError;
use crate::container::{IndexedContainer, StackContainer, Container};

pub const TAG_NUM_MAX: isize = isize::MAX / 4;
pub const TAG_NUM_MIN: isize = isize::MIN / 4;

pub struct NumberObject {
    data: CellPtr<ArrayU64>,
    negative: Cell<bool>,
}

impl NumberObject {
    pub fn alloc_from_isize<'guard>(
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
                data: CellPtr::from(data),
                negative: Cell::from(integer < 0),
            }
        )
    }

    pub fn sub<'guard>(
        &self,
        rhs: &NumberObject,
        mem: &'guard MutatorView
    ) -> Result<ScopedPtr<'guard, NumberObject>, RuntimeError> {
        if rhs.negative.get() == self.negative.get() {
            self.logical_sub(rhs, mem)
        } else {
            self.logical_add(rhs, mem)
        }
    }

    pub fn add<'guard>(
        &self,
        rhs: &NumberObject,
        mem: &'guard MutatorView
    ) -> Result<ScopedPtr<'guard, NumberObject>, RuntimeError> {
        if rhs.negative.get() == self.negative.get() {
            self.logical_add(rhs, mem)
        } else {
            self.logical_sub(rhs, mem)
        }
    }

    fn logical_add<'guard>(
        &self,
        rhs: &NumberObject,
        mem: &'guard MutatorView
    ) -> Result<ScopedPtr<'guard, NumberObject>, RuntimeError> {
        let result = self.unsigned_add(rhs, mem)?;
        result.negative.set(rhs.negative.get());
        Ok(result)
    }

    fn logical_sub<'guard>(
        &self,
        rhs: &NumberObject,
        mem: &'guard MutatorView
    ) -> Result<ScopedPtr<'guard, NumberObject>, RuntimeError> {
        if self.unsigned_gt(rhs, mem)? {
            let result = self.unsigned_sub(rhs, mem)?;
            result.negative.set(self.negative.get());
            Ok(result)
        } else if self.unsigned_eq(rhs, mem)? {
            Ok(NumberObject::alloc_from_isize(0, mem)?)
        } else {
            let result = rhs.unsigned_sub(self, mem)?;
            result.negative.set(rhs.negative.get());
            Ok(result)
        }
    }

    fn unsigned_gte<'guard>(
        &self,
        rhs: &NumberObject,
        mem: &'guard MutatorView
    ) -> Result<bool, RuntimeError> {
        let lhs_data = self.data.get(mem);
        let rhs_data = rhs.data.get(mem);
        let lhs_len = lhs_data.length();
        let rhs_len = rhs_data.length();

        if lhs_len != rhs_len {
            return Ok(lhs_len < rhs_len);
        }

        if lhs_len > rhs_len {
            return Ok(true);
        }

        Ok(
            self.unsigned_gt(rhs, mem)? ||
            self.unsigned_eq(rhs, mem)?
        )
    }

    // returns true if self is greater than rhs, not accounting the sign
    fn unsigned_gt<'guard>(
        &self,
        rhs: &NumberObject,
        mem: &'guard MutatorView
    ) -> Result<bool, RuntimeError> {
        let lhs_data = self.data.get(mem);
        let rhs_data = rhs.data.get(mem);
        let lhs_len = lhs_data.length();
        let rhs_len = rhs_data.length();

        if lhs_len < rhs_len {
            return Ok(false);
        }

        if lhs_len > rhs_len {
            return Ok(true);
        }

        for index in (0..lhs_len).rev() {
            let lhs_val = (*lhs_data).get(mem, index)?;
            let rhs_val = (*rhs_data).get(mem, index)?;

            if lhs_val < rhs_val {
                return Ok(false);
            }
            if lhs_val > rhs_val {
                return Ok(true);
            }
        }

        // they are equal
        return Ok(false);
    }

    fn unsigned_eq<'guard>(
        &self,
        rhs: &NumberObject,
        mem: &'guard MutatorView
    ) -> Result<bool, RuntimeError> {
        let lhs_data = self.data.get(mem);
        let rhs_data = rhs.data.get(mem);
        let lhs_len = lhs_data.length();
        let rhs_len = rhs_data.length();

        if lhs_len != rhs_len {
            return Ok(false);
        }

        for index in 0..lhs_len {
            let lhs_val = (*lhs_data).get(mem, index)?;
            let rhs_val = (*rhs_data).get(mem, index)?;

            if lhs_val != rhs_val {
                return Ok(false);
            }
        }

        return Ok(true);
    }

    // lhs is guaranteed to be greater than rhs, otherwise undefined
    fn unsigned_sub<'guard>(
        &self,
        rhs: &NumberObject,
        mem: &'guard MutatorView
    ) -> Result<ScopedPtr<'guard, NumberObject>, RuntimeError> {
        let result_obj = NumberObject::alloc_from_isize(0, mem)?;
        let result_data = result_obj.data.get(mem);
        let lhs_data = self.data.get(mem);
        let rhs_data = rhs.data.get(mem);
        let lhs_len = lhs_data.length();
        let rhs_len = rhs_data.length();
        let mut carry_flag = false;
        let mut index = 0;

        while index < lhs_len {
            let lhs_val = (*lhs_data).get(mem, index)?;
            let rhs_val = if index < rhs_len {
                (*rhs_data).get(mem, index)?
            } else {
                0
            };

            let mut temp: i128 = (lhs_val - rhs_val) as i128;

            if carry_flag {
                temp -= 1;
            }

            match u64::try_from(temp) {
                Ok(n) => {
                    if !((n == 0) && (index == (lhs_len - 1))) {
                        result_data.push(mem, n)?;
                        carry_flag = false;
                    }
                }
                Err(_) => {
                    let n = u64::try_from(
                        temp + (u64::MAX as i128)
                    ).unwrap();
                    result_data.push(mem, n)?;
                    carry_flag = true;
                } 
            }

            index += 1;
        }

        Ok(result_obj)
    }

    fn unsigned_add<'guard>(
        &self,
        rhs: &NumberObject,
        mem: &'guard MutatorView
    ) -> Result<ScopedPtr<'guard, NumberObject>, RuntimeError> {
        let result_obj = NumberObject::alloc_from_isize(0, mem)?;
        let result_data = result_obj.data.get(mem);
        let lhs_data = self.data.get(mem);
        let rhs_data = rhs.data.get(mem);
        let lhs_len = lhs_data.length();
        let rhs_len = rhs_data.length();
        let mut carry_flag = false;
        let mut index = 0;

        while index < rhs_len || index < lhs_len || carry_flag {
            let lhs_val = if index < lhs_len {
                (*lhs_data).get(mem, index)?
            } else {
                0
            };

            let rhs_val = if index < rhs_len {
                (*rhs_data).get(mem, index)?
            } else {
                0
            };

            let mut temp: u128 = (rhs_val + lhs_val) as u128;

            if carry_flag {
                temp += 1;
            }

            match u64::try_from(temp) {
                Ok(n) => {
                    result_data.push(mem, n)?;
                    carry_flag = false;
                }
                Err(_) => {
                    // TODO: maybe a bit mask might be faster
                    let n = ((temp as u64) - u64::MAX) as u64;
                    result_data.push(mem, n)?;
                    carry_flag = true;
                } 
            }

            index += 1;
        }

        Ok(result_obj)
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
