use crate::safe_ptr::CellPtr;
use crate::symbol_map::SymbolMap;
use crate::error::RuntimeError;
use crate::bytecode::ByteCode;
use crate::bytecode::Register;
use crate::memory::MutatorView;

struct Scope {
    // symbols: Vec<String>,
    // symbol bindings
    // function being declared
    // function name
}

//pub type ScopeList = Array<Scope>;

pub struct Generator {
    // scopes: CellPtr<ScopeList>,
    bytecode: Option<CellPtr<ByteCode>>
}

impl Generator {
    pub fn alloc<'guard>(view: &'guard MutatorView) -> Result<Generator, RuntimeError> {
        Ok(
            Generator { 
                // scopes: vec![],
                bytecode: None
            }
        )
    }

    pub fn lookup_sym(&self, sym_map: &SymbolMap, name: &str) -> Register {
        todo!()
    }
}
