use std::cell::RefCell;
use std::collections::HashMap;

use zapalloc::{AllocRaw, RawPtr};

use crate::arena::Arena;
use crate::symbol::Symbol;

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

    pub fn lookup(&self, name: &str) -> RawPtr<Symbol> {
        {
            if let Some(ptr) = self.map.borrow().get(name) {
                return *ptr;
            }
        }

        let name = String::from(name);
        let ptr = self.arena.alloc(Symbol::new(&name)).unwrap();
        self.map.borrow_mut().insert(name, ptr);
        ptr
    }
}
