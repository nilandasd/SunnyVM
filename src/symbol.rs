use std::hash::{Hash, Hasher};
use std::str;

use crate::array::{ArrayU8, Array};
use crate::hashable::Hashable;
use crate::safe_ptr::MutatorScope;
use crate::memory::MutatorView;
use crate::error::RuntimeError;
use crate::container::{Container, IndexedContainer};

pub type Symbol = Array<u8>;

impl Symbol {
    pub fn from_str<'guard>(mem: &'guard MutatorView, name: &str) -> Result<Symbol, RuntimeError> {
        let symbol = ArrayU8::with_capacity(mem, name.len() as u32)?;

        for index in 0..symbol.length() {
            symbol.set(mem, index, name.as_bytes()[index as usize])?;
        }

        Ok(symbol)
    }

    pub fn as_str<'guard>(&self, mem: &'guard dyn MutatorScope) -> &str {
        unsafe {
            str::from_utf8(self.as_slice(mem)).unwrap()
        }
    }

    fn print_str<'guard>(&self, mem: &'guard MutatorView) {
        unsafe {
            println!("{:?}", str::from_utf8(self.as_slice(mem)));
        }
    }
}

impl Hashable for Symbol {
    fn hash<'guard, H: Hasher>(&self, guard: &'guard dyn MutatorScope, h: &mut H) {
        unsafe {
            self.as_str(guard).hash(h)
        }
    }
}
