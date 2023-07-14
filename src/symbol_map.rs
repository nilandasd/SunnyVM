use std::cell::RefCell;
use std::collections::HashMap;

use zapalloc::{AllocRaw, RawPtr};

use crate::arena::Arena;
use crate::symbol::Symbol;
use crate::error::RuntimeError;
use crate::memory::MutatorView;

pub struct SymbolMap {
    map: RefCell<HashMap<String, RawPtr<Symbol>>>,
    arena: Arena,
}

impl SymbolMap {
    pub fn new() -> SymbolMap {
        SymbolMap {
            map: RefCell::new(HashMap::new()),
            arena: Arena::new(),
        }
    }

    pub fn lookup<'guard>(&self, mem: &'guard MutatorView, name: &str) -> Result<RawPtr<Symbol>, RuntimeError> {
        if let Some(ptr) = self.map.borrow().get(name) {
            return Ok(*ptr);
        }

        let name = String::from(name);
        let ptr = self.arena.alloc(Symbol::from_str(mem, &name)?).unwrap();
        self.map.borrow_mut().insert(name, ptr);
        Ok(ptr)
    }
}
