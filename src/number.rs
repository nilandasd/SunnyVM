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

#[derive(PartialEq)]
enum NumObjRelation {
    Greater,
    Equal,
    Less,
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

    pub fn cmp<'guard>(
        &self,
        rhs: &NumberObject,
        mem: &'guard MutatorView
    ) -> Result<NumObjRelation, RuntimeError> {
        if self.negative.get() && !rhs.negative.get() {
            Ok(NumObjRelation::Less)
        } else if !self.negative.get() && rhs.negative.get() {
            Ok(NumObjRelation::Greater)
        } else if !self.negative.get() {
            Ok(self.unsigned_cmp(rhs, mem)?)
        } else {
            match self.unsigned_cmp(rhs, mem)? {
                NumObjRelation::Less =>
                    Ok(NumObjRelation::Greater),
                NumObjRelation::Greater =>
                    Ok(NumObjRelation::Greater),
                NumObjRelation::Equal =>
                    Ok(NumObjRelation::Equal),
            }
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
        match self.unsigned_cmp(rhs, mem)? {
            NumObjRelation::Greater => {
                let result = self.unsigned_sub(rhs, mem)?;
                result.negative.set(self.negative.get());
                Ok(result)
            }
            NumObjRelation::Equal => {
                Ok(NumberObject::alloc_from_isize(0, mem)?)
            }
            NumObjRelation::Less => {
                let result = rhs.unsigned_sub(self, mem)?;
                result.negative.set(!rhs.negative.get());
                Ok(result)
            }
        }
    }

    fn unsigned_cmp<'guard>(
        &self,
        rhs: &NumberObject,
        mem: &'guard MutatorView
    ) -> Result<NumObjRelation, RuntimeError> {
        let lhs_data = self.data.get(mem);
        let rhs_data = rhs.data.get(mem);
        let lhs_len = lhs_data.length();
        let rhs_len = rhs_data.length();

        if lhs_len < rhs_len {
            return Ok(NumObjRelation::Less);
        }

        if lhs_len > rhs_len {
            return Ok(NumObjRelation::Greater);
        }

        for index in (0..lhs_len).rev() {
            let lhs_val = (*lhs_data).get(mem, index)?;
            let rhs_val = (*rhs_data).get(mem, index)?;

            if lhs_val < rhs_val {
                return Ok(NumObjRelation::Less);
            }
            if lhs_val > rhs_val {
                return Ok(NumObjRelation::Greater);
            }
        }

        return Ok(NumObjRelation::Equal);
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

        result_data.clear(mem)?;

        while index < lhs_len {
            let lhs_val = (*lhs_data).get(mem, index)?;
            let rhs_val = if index < rhs_len {
                (*rhs_data).get(mem, index)?
            } else {
                0
            };

            let mut temp: i128 = (lhs_val as i128) - (rhs_val as i128);

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

        result_data.clear(mem)?;

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

            let mut temp: u128 = (rhs_val as u128) + (lhs_val as u128);

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
                    let n = (temp - (u64::MAX as u128)) as u64;
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
        guard: &'guard dyn MutatorScope,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        write!(f, "NumberObject: ");

        unsafe {
            for n in self.data.get(guard).as_slice(guard).iter().rev() {
                write!(f, "{}\t", n);

            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::error::RuntimeError;
    use crate::memory::{Memory, Mutator, MutatorView};

    #[test]
    fn equality_comparison() {
        let mem = Memory::new();

        struct Test {}
        impl Mutator for Test {
            type Input = ();
            type Output = ();

            fn run(
                &self,
                mem: &MutatorView,
                _input: Self::Input,
            ) -> Result<Self::Output, RuntimeError> {
                let x = NumberObject::alloc_from_isize(69, mem)?;
                let y = NumberObject::alloc_from_isize(69, mem)?;
                let z = NumberObject::alloc_from_isize(420, mem)?;

                assert!(NumObjRelation::Equal == z.cmp(&z, mem)?);
                assert!(NumObjRelation::Equal == x.cmp(&y, mem)?);
                assert!(NumObjRelation::Equal != x.cmp(&z, mem)?);

                Ok(())
            }
        }

        let test = Test {};
        
        mem.mutate(&test, ()).unwrap();
    }

    #[test]
    fn addition_and_subtraction() {
        let mem = Memory::new();

        struct Test {}
        impl Mutator for Test {
            type Input = ();
            type Output = ();

            fn run(
                &self,
                mem: &MutatorView,
                _input: Self::Input,
            ) -> Result<Self::Output, RuntimeError> {
                let zero = NumberObject::alloc_from_isize(0, mem)?;
                let one = NumberObject::alloc_from_isize(isize::MAX, mem)?;
                let two = one.add(&one, mem)?; // max * 2
                let three = two.add(&one, mem)?; // max * 3
                let four = three.add(&one, mem)?; // max * 4
                let minus_two = two.sub(&four, mem)?; // max * 2 * -1
                let zero2 = two.add(&minus_two, mem)?; // 0
                                         
                assert!(NumObjRelation::Less == minus_two.cmp(&one, mem)?);
                assert!(NumObjRelation::Less == zero.cmp(&one, mem)?);
                assert!(NumObjRelation::Less == one.cmp(&two, mem)?);
                assert!(NumObjRelation::Less == two.cmp(&three, mem)?);
                assert!(NumObjRelation::Less == three.cmp(&four, mem)?);
                assert!(NumObjRelation::Equal == zero.cmp(&zero2, mem)?);
                assert!(NumObjRelation::Greater == zero.cmp(&minus_two, mem)?);
                assert!(NumObjRelation::Greater == one.cmp(&zero, mem)?);
                assert!(NumObjRelation::Greater == two.cmp(&one, mem)?);
                assert!(NumObjRelation::Greater == three.cmp(&two, mem)?);
                assert!(NumObjRelation::Greater == four.cmp(&three, mem)?);

                assert!(minus_two.negative.get());

                Ok(())
            }
        }

        let test = Test {};
        mem.mutate(&test, ()).unwrap();
    }
}
