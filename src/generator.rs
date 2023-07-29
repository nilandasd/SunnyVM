use std::collections::HashMap;
use std::marker::PhantomData;
use std::cell::Cell;

use zapalloc::ArraySize;

use crate::tagged_ptr::{TaggedPtr, Value};
use crate::safe_ptr::{CellPtr, TaggedScopedPtr, TaggedCellPtr};
use crate::bytecode::{ByteCode, LiteralId, JumpOffset};
use crate::bytecode::{Register, Opcode};
use crate::error::{RuntimeError, GeneratorError};
use crate::memory::{Mutator, MutatorView, SymbolId};
use crate::thread::Thread;
use crate::list::List;
use crate::function::Function;

pub trait Compiler {
    fn compile<'guard>(self, generator: &mut Generator) -> Result<(), ()>;
}

pub type Offset = u32;

pub struct MetaGenerator<T> {
    _compiler_type: PhantomData<T>,
}

impl<T> MetaGenerator<T> {
    pub fn new() -> MetaGenerator<T> {
        MetaGenerator {
            _compiler_type: PhantomData,
        }
    }
}

impl<T: Compiler> Mutator for MetaGenerator<T> {
    type Input = T;
    type Output = CellPtr<Thread>;

    fn run(
        &self,
        view: &MutatorView,
        compiler: Self::Input,
    ) -> Result<Self::Output, RuntimeError> {
        let mut generator = Generator::alloc(view, None)?;
        let thread = CellPtr::from(Thread::alloc(view)?);

        compiler.compile(&mut generator);

        // set the thread to point generator main func

        Ok(thread)
    }
}

type TempId = usize; // TODO: allow for more then 256 temps

#[derive(Eq, Hash, PartialEq, Copy, Clone)]
enum Var {
    Symbol(SymbolId),
    Temp(TempId),
}

pub struct Binding {
    location: BindSite,
    initialized: bool,
    kind: VarKind,
}

enum BindSite {
    Register(Register),
    Overflow(usize),
}

enum VarKind {
    Upvalue {
        upvalue_id: u8,
        frame_offset: u8,
        frame_register: u8,
    },
    Global,
    Local,
    Temp,
}

pub struct Generator<'guard, 'parent> {
    view: &'guard MutatorView<'guard>,
    parent: Option<&'parent Generator<'guard, 'parent>>,
    function: CellPtr<Function>,
    bindings: HashMap<Var, Binding>,
    registers: [Option<Var>; 256],
}

impl<'guard, 'parent> Generator<'guard, 'parent> {
    fn alloc(
        view: &'guard MutatorView<'guard>,
        parent: Option<&'parent Generator<'guard, 'parent>>,
    ) -> Result<Generator<'guard, 'parent>, RuntimeError> {
        //let args = CellPtr::from(List::alloc(view)?);
        let function = CellPtr::from(Function::default_alloc(view)?);
        let bindings = HashMap::new();
        let registers = [None; 256];

        Ok(Generator { view, parent, bindings, function, registers })
    }

    pub fn decl_var(&mut self, name: String) -> Var {
        if let Value::Symbol(sym_id) = self.view.lookup_sym(&name).value() {
            let var_kind = match self.parent {
                None => VarKind::Global,
                Some(_) => VarKind::Local,
            };
            let var = Var::Symbol(sym_id);

            self.set_binding(var_kind);

            var
        } else {
            unreachable!("lookup_sym always returns sym_id")
        }
    }

    fn set_binding(&mut self, var_kind: VarKind) -> Binding {
        todo!()
    }

    pub fn return_var(&mut self, dest: Var) {
        todo!()
    }

    pub fn decl_func(&mut self) -> Result<Generator<'guard, 'parent>, RuntimeError> {
        // alloc a func with this func as the parent
        todo!()
    }

    pub fn load_num(&mut self, var: &mut Var, num: isize) {
        let binding = self.bindings.get(var).unwrap();
        let var_id = match var {
            Var::Temp(id) => id,
            Var::Symbol(id) => id,
        };

        if num < (i16::MIN as isize) || (i16::MAX as isize) < num {
            // TODO: push a literal
            // num might still fit into a tagged ptr
            // otherwize we need a num object
            todo!()
        } else {
            match binding.kind {
                VarKind::Temp | VarKind::Local => {
                    let code = Opcode::LoadInteger {
                        dest: binding.register,
                        integer: num as i16
                    };
                    self.push_code(code);
                }
                VarKind::Global => {
                    let num_reg = self.bind_temp().register;
                    let sym_reg = self.bind_temp().register;

                    let load_int = Opcode::LoadInteger {
                        dest: num_reg,
                        integer: num as i16
                    };

                    // TODO: symbolId may be larger than u16 MAX 
                    // in this case a load literal instruction would be needed
                    // to load a taggedsymbolpointer
                    let load_sym = Opcode::LoadSymbol {
                        dest: sym_reg,
                        symbol: *var_id as u16
                    };

                    let store_global = Opcode::StoreGlobal {
                        src: num_reg,
                        name: sym_reg,
                    };


                    self.push_code(load_int);
                    self.push_code(load_sym);
                    self.push_code(store_global);
                }
                VarKind::Upvalue {
                    upvalue_id, frame_offset, frame_register
                } => {
                    todo!()
                }
            }
        }
    }

    pub fn add(&mut self, dest: &mut Var, var1: &mut Var, var2: &mut Var) { 
        match dest.kind {
            VarKind::Temp | VarKind::Local { .. } => {
                self.load(var1);
                self.load(var2);

                let code = Opcode::Add {
                    dest: dest.register,
                    reg1: var1.register,
                    reg2: var2.register,
                };

                self.push_code(code);
            }
            _ => { todo!() }
        }
    }

    fn load(&mut self, var: &mut Var) {
        // should handle the code for loading in an upvalue or Global
        // if they have yet to be loaded, and have not been 
        todo!()
    }

    fn push_code(&self, code: Opcode) -> Result<Offset, RuntimeError> {
        let bytecode = self.function.get(self.view).code(self.view);

        bytecode.push(self.view, code)?;

        Ok(bytecode.last_instruction())
    }

    pub fn get_temp(&mut self) -> Var {
        todo!()
    }
}
