use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::marker::PhantomData;
use std::cell::Cell;

use zapalloc::ArraySize;

use crate::tagged_ptr::{TaggedPtr, Value};
use crate::safe_ptr::{ScopedPtr, CellPtr, TaggedScopedPtr, TaggedCellPtr};
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
        let global_func = Function::default_alloc(view)?;
        let mut generator = Generator::new(view, None, global_func);
        let thread = CellPtr::from(Thread::alloc(view)?);
        let func_ptr = generator.get_func_ptr();

        compiler.compile(&mut generator);

        thread.get(view).set_func(view, func_ptr)?;

        Ok(thread)
    }
}

type TempId = usize; // TODO: allow for more then 256 temps

#[derive(Eq, Hash, PartialEq, Copy, Clone)]
pub enum VarId {
    Symbol(SymbolId),
    Temp(TempId),
}

#[derive(Copy, Clone)]
struct Var {
    kind: VarKind,
    bind_index: Option<u16>, // this means a function can hold u16::MAX values at once
}

impl Var {
    fn reg(&self) -> u8 {
        match self.bind_index {
            None => panic!("not bound"),
            Some(idx) => {
                if 255 < idx {
                    panic!("not bound")
                }

                u8::try_from(idx).unwrap()
            }
        }
    }
}

#[derive(Eq, PartialEq, Clone, Copy)]
enum VarKind {
    Upvalue {
        upvalue_id: u8,
        frame_offset: u16,
        frame_register: u16, // if greater than 255 then it is spilled, not a register
    },
    Global,
    Local,
    Temp,
}

#[derive(Copy, Clone)]
enum Binding {
    Freed,
    Used(VarId),
    Active(VarId),
}

pub struct Generator<'guard, 'parent> {
    mem: &'guard MutatorView<'guard>,
    parent: Option<&'parent mut Generator<'guard, 'parent>>,
    function: CellPtr<Function>,
    vars: HashMap<VarId, Var>,
    bindings: Vec<Binding>,
    temp_counter: usize
}

impl<'guard, 'parent> Generator<'guard, 'parent> {
    fn new(
        mem: &'guard MutatorView<'guard>,
        parent: Option<&'parent mut Generator<'guard, 'parent>>,
        function: ScopedPtr<'guard, Function>,
    ) -> Generator<'guard, 'parent> {
        //let args = CellPtr::from(List::alloc(view)?);
        let function = CellPtr::from(function);
        let vars = HashMap::new();
        let bindings = vec![];
        let temp_counter = 0;

        Generator { mem, parent, function, bindings, vars, temp_counter }
    }

    pub fn load_func(&mut self, var_id: VarId, generator: Generator<'guard, 'parent>) -> Result<(), RuntimeError> {
        let func_ptr = generator.get_func_ptr().as_tagged(self.mem);
        let literal_id = self.push_lit(func_ptr)?;
        let temp = self.get_temp();
        let function = self.bind(temp)?;
        let dest = self.bind(var_id)?;

        self.push_code(Opcode::LoadLiteral { dest: function, literal_id })?;
        self.push_code(Opcode::MakeClosure { dest, function })?;
        
        self.free(temp);

        Ok(())
    }

    pub fn decl_func(&mut self, args: Vec<String>) -> Result<Generator<'guard, 'parent>, RuntimeError> {
        let new_func = Function::default_alloc(self.mem)?; // TODO: set function args
        let generator = Generator::new(self.mem, None, new_func);
        
        Ok(generator)
    }

    pub fn decl_var(&mut self, name: String) -> VarId {
        if let Value::Symbol(sym_id) = self.mem.lookup_sym(&name).value() {
            let new_var_kind = if self.is_global_scope() {
                VarKind::Global
            } else {
                VarKind::Local
            };

            let var_id = VarId::Symbol(sym_id);

            if let Some(existing_var) = self.vars.get(&var_id) {
                match existing_var.kind {
                    VarKind::Local => {
                        match new_var_kind {
                            VarKind::Global => unreachable!("local var was declared in the global scope"),
                            VarKind::Local => return var_id,
                            VarKind::Temp | VarKind::Upvalue { .. } => unreachable!("decl kind is always local or global"),
                        }
                    }
                    VarKind::Global => {
                        match new_var_kind {
                            VarKind::Global => return var_id,
                            VarKind::Local => {
                                if let Some(bind_idx) = existing_var.bind_index {
                                    self.bindings[bind_idx as usize] = Binding::Freed;
                                }
                            },
                            VarKind::Temp | VarKind::Upvalue { .. } => unreachable!("decl kind is always local or global"),
                        }
                    }
                    VarKind::Upvalue { .. } => {
                        match new_var_kind {
                            VarKind::Global => unreachable!("cannot have an upvalue in the global scope"),
                            VarKind::Local => {
                                if let Some(bind_idx) = existing_var.bind_index {
                                    self.bindings[bind_idx as usize] = Binding::Freed;
                                }
                            }
                            VarKind::Temp | VarKind::Upvalue { .. } => unreachable!("decl kind is always local or global"),
                        }
                    }
                    VarKind::Temp => unreachable!("temp variable found with symbol id"),
                }
            }

            let new_var = Var {
                kind: new_var_kind,
                bind_index: None,
            };

            self.vars.insert(var_id, new_var);

            var_id
        } else {
            unreachable!("lookup_sym always returns sym_id")
        }
    }

    pub fn add(&mut self, dest: VarId, op1: VarId, op2: VarId) -> Result<(), RuntimeError> {
        let dest = self.bind(dest)?;
        self.activate(dest);
        let reg1 = self.bind(op1)?;
        self.activate(reg1);
        let reg2 = self.bind(op2)?;
        
        self.push_code(Opcode::Add { dest, reg1, reg2 })?;

        self.deactivate(dest);
        self.deactivate(reg1);

        Ok(())
    }

    pub fn load_num(&mut self, var_id: VarId, num: isize) -> Result<(), RuntimeError> {
        let dest = self.bind(var_id)?;

        if num < (i16::MIN as isize) || (i16::MAX as isize) < num {
            // either make a taggedptr or load a literal num object
            todo!()
        } else {
            self.push_code(Opcode::LoadInteger { dest, integer: num as i16 })?;
        }

        Ok(())
    }

    pub fn get_temp(&mut self) -> VarId {
        let var_id = VarId::Temp(self.temp_counter);
        let temp = Var {
            kind: VarKind::Temp,
            bind_index: None,
        };

        self.temp_counter += 1;
        self.vars.insert(var_id, temp);

        return var_id;
    }

    pub fn gen_return(&mut self, var_id: VarId) -> Result<(), RuntimeError> {
        let reg = self.bind(var_id)?;

        self.push_code(Opcode::Return { reg })?;

        Ok(())
    }

    // ===================== PRIVATE FUNCS ====================================
    // ===================== PRIVATE FUNCS ====================================
    // ===================== PRIVATE FUNCS ====================================
    // ===================== VVVVVVVVVVVVV ====================================

    fn is_global_scope(&self) -> bool {
        self.parent.is_none()
    }

    fn find_or_close_var(&mut self, var_id: VarId, frame_offset: u16) -> Var {
        if let Some(var) = self.vars.get(&var_id) {
            if frame_offset == 0 {
                return *var;
            } else {
                match var.kind {
                    VarKind::Global => {
                        let new_var = Var {
                            kind: VarKind::Global,
                            bind_index: None,
                        };

                        return new_var;
                    }
                    VarKind::Local => {
                        match var.bind_index {
                            None => {
                                // In this case we found a var to close, but it
                                // is not yet initialized so we dont need to
                                // create an upvalue and instead can just
                                // declare a local value.
                                let new_var = Var {
                                    kind: VarKind::Local,
                                    bind_index: None,
                                };

                                return new_var;
                            }
                            Some(idx) => {
                                let new_var = Var {
                                    kind: VarKind::Upvalue {
                                        upvalue_id: 0, // don't set upvalue id until
                                        frame_offset,
                                        frame_register: idx,
                                    },
                                    bind_index: None,
                                };

                                return new_var;
                            }
                        }
                    }
                    VarKind::Upvalue {
                        frame_offset: nested_offset,
                        frame_register,
                        .. 
                    } => {
                        // we can use the upvalue in this nested func to create 
                        // a new one for the calling func, 
                        let new_var = Var {
                            kind: VarKind::Upvalue {
                                upvalue_id: 0,
                                frame_offset: nested_offset + frame_offset,
                                frame_register,
                            },
                            bind_index: None,
                        };

                        return new_var;
                    }
                    VarKind::Temp => unreachable!("cannot close temps"),
                }
            }
        }

        // if var is a temp then error b/c it was not found in the current function
        if let VarId::Temp(_) = var_id {
            panic!("Generator Error: Temporary value not found")
        }

        if self.is_global_scope() {
            let new_var = Var {
                kind: VarKind::Global,
                bind_index: None,
            };

            self.vars.insert(var_id, new_var);
        }

        self.parent.as_mut().unwrap().find_or_close_var(var_id, frame_offset + 1)
    }

    // return reg this way we can write let reg self.active(self.bind(var_id));
    fn activate(&mut self, reg: u8) {
        match self.bindings[reg as usize] {
            Binding::Freed => panic!("cannot active free binding"),
            Binding::Used(var_id) => {
                self.bindings[reg as usize] = Binding::Active(var_id);
            }
            Binding::Active(_) => {}, //already active, do nothing
        }
    }

    fn deactivate(&mut self, reg: u8) {
        match self.bindings[reg as usize] {
            Binding::Freed => panic!("cannot deactive free binding"),
            Binding::Used(_) => {} // already deactivated
            Binding::Active(var_id) => {
                self.bindings[reg as usize] = Binding::Used(var_id);
            },
        }
    }

    // binds a variable to a register
    // CASES
    //  1. the variable is already bound
    //      - do nothing
    //  2. a variable is not bound
    //      - find / evict a reg
    //  3. the variable is bound to an overflow
    //      - insert a load_overflow instruction
    //      - find / evict a reg
    // 1 there is an available free register
    // may need to create spill or zambone instructions to do so
    fn bind(&mut self, var_id: VarId) -> Result<u8, RuntimeError> {
        let mut var = self.vars.get(&var_id).unwrap().clone();

        match var.bind_index {
            None => {
                let new_bind_index = self.get_free_reg(var_id)?;
                var.bind_index = Some(u16::try_from(new_bind_index).unwrap());
            }
            Some(bind_index) => {
                if bind_index > 255 {
                    let new_bind_index = self.get_free_reg(var_id)?;
                    let overflow_id = u16::try_from(bind_index - 256).unwrap();
                    var.bind_index = Some(u16::try_from(new_bind_index).unwrap());
                    self.push_code(
                        Opcode::LoadOverflow {
                            dest: new_bind_index,
                            overflow_id
                    })?;
                } else {
                    // nothing to do we already got a good binding :)
                    return Ok(var.bind_index.unwrap() as u8)
                }
            }
        };

        self.vars.insert(var_id, var);

        Ok(var.bind_index.unwrap() as u8)
    }

    // sets a register to be Binding::Userd(var_id) and returns the index
    fn get_free_reg(&mut self, var_id: VarId) -> Result<u8, RuntimeError> {
        let mut index = 0;
        while index < self.bindings.len() && index < 256 {
            let binding = self.bindings[index];

            match binding {
                Binding::Freed => {
                    self.bindings[index] = Binding::Used(var_id);

                    return Ok(u8::try_from(index).unwrap());
                }
                _ => {}
            }

            index += 1;
        }

        if index == self.bindings.len() {
            self.bindings.push(Binding::Used(var_id));
            return Ok(u8::try_from(index).unwrap());
        }

        // we didn't find a free register :(
        // we must spill a register into memory
 
        index = 0;
        while index < 256 {
            let binding = self.bindings[index];

            if index < 256 {
                match binding {
                    Binding::Active(_) => {
                        continue;
                    }
                    Binding::Used(old_var_id) => {
                        self.bindings[index] = Binding::Used(var_id);
                        self.evict(old_var_id)?;
                        self.function.get(self.mem);

                        return Ok(u8::try_from(index).unwrap());
                    }
                    Binding::Freed => unreachable!("there are no free registers"),
                }
            }

            index += 1;
        }

        unreachable!("every register was active!")
    }

    // finds an overflow index to evict the var to
    // updates the vars bind index
    // inserts a store_overflow instruction
    fn evict(&mut self, var_id: VarId) -> Result<(), RuntimeError> {
        let mut var = self.vars.get(&var_id).unwrap().clone();
        let mut index = 256;
        let mut overflow_id = 0;
        let src = u8::try_from(var.bind_index.unwrap()).unwrap();

        while index < self.bindings.len() {
            let binding = self.bindings[index];

            match binding {
                Binding::Freed => {
                    // this is a free overflow index
                    self.bindings[index] = Binding::Used(var_id);
                    overflow_id = index - 256;
                    break;
                }
                Binding::Used(_) => {} // do nothing
                Binding::Active(_) => unreachable!("an overflow binding cannot be active"),
            }

            index += 1;
        }

        if index == self.bindings.len() {
            // we didn't find a free overflow binding, we need to create a new one
            self.bindings.push(Binding::Used(var_id));
            overflow_id = index - 256;
        }

        // update the var's bind index
        var.bind_index = Some(u16::try_from(index).unwrap());

        self.vars.insert(var_id, var);

        self.push_code(Opcode::StoreOverflow {
            overflow_id: u16::try_from(overflow_id).unwrap(),
            src
        })?;

        Ok(())
    }

    fn push_code(&self, code: Opcode) -> Result<Offset, RuntimeError> {
        let bytecode = self.function.get(self.mem).code(self.mem);

        bytecode.push(self.mem, code)?;

        Ok(bytecode.last_instruction())
    }

    fn push_lit(&self, literal: TaggedScopedPtr<'guard>) -> Result<LiteralId, RuntimeError> {
        let bytecode = self.function.get(self.mem).code(self.mem);

        bytecode.push_lit(self.mem, literal)
    }

    fn free(&mut self, var_id: VarId) {
        let var = self.vars.get(&var_id).unwrap();

        match var.bind_index {
            None => {},
            Some(idx) => self.bindings[idx as usize] = Binding::Freed,
        }

        self.vars.remove(&var_id);
    }

    fn get_func_ptr(&self) -> ScopedPtr<'guard, Function> {
        self.function.get(self.mem)
    }
}
