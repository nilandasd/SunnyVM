use std::collections::HashMap;
use std::convert::TryFrom;
use std::marker::PhantomData;

use zapalloc::ArraySize;

use crate::bytecode::{LiteralId, JumpOffset, Opcode, Register};
use crate::error::RuntimeError;
use crate::function::Function;
use crate::memory::{Mutator, MutatorView, SymbolId};
use crate::safe_ptr::{CellPtr, ScopedPtr, TaggedScopedPtr};
use crate::tagged_ptr::Value;
use crate::thread::Thread;

type TempId = usize;

pub trait Compiler {
    fn compile<'guard>(self, generator: &mut Generator) -> Result<(), RuntimeError>;
}

pub struct MetaGenerator<T> {
    _compiler_type: PhantomData<T>,
}

#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone)]
pub enum VarId {
    Symbol(SymbolId),
    Temp(TempId),
}

#[derive(Debug, Copy, Clone)]
struct Var {
    kind: VarKind,
    bind_index: Option<u16>, // this means a function can hold u16::MAX values at once
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
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

#[derive(Copy, Clone, Debug, PartialEq)]
enum Binding {
    Freed,
    Used(VarId),
    Active(VarId),
}

pub struct Generator<'guard> {
    mem: &'guard MutatorView<'guard>,
    function_stack: Vec<FunctionGenerator<'guard>>,
}

struct FunctionGenerator<'guard> {
    mem: &'guard MutatorView<'guard>,
    function: Function,
    vars: HashMap<VarId, Var>,
    bindings: Vec<Binding>,
    temp_counter: usize,
    is_closure: bool,
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

    fn run(&self, view: &MutatorView, compiler: Self::Input) -> Result<Self::Output, RuntimeError> {
        let mut generator = Generator::new(view)?;
        let thread = CellPtr::from(Thread::alloc(view)?);
        compiler.compile(&mut generator)?;
        let func_ptr = generator.function_stack.pop().unwrap().alloc_func()?;
        thread.get(view).set_func(view, func_ptr)?;
        Ok(thread)
    }
}

impl Var {
    fn reg(&self) -> Register {
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

impl<'guard> Generator<'guard> {
    pub fn new(mem: &'guard MutatorView<'guard>) -> Result<Generator<'guard>, RuntimeError> {
        let function = Function::new_default(mem)?;
        Ok(Generator {
            mem,
            function_stack: vec![FunctionGenerator::new(function, mem)],
        })
    }

    pub fn nop(&mut self) -> Result<ArraySize, RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].nop()
    }

    pub fn backpatch(&mut self, index: ArraySize, new_offset: JumpOffset) -> Result<(), RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].backpatch(index, new_offset)
    }

    pub fn jump(&mut self, offset: JumpOffset) -> Result<ArraySize, RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].jump(offset)
    }

    pub fn jump_if_true(
        &mut self,
        test: VarId,
        offset: JumpOffset
    ) -> Result<ArraySize, RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].jump_if_true(test, offset)
    }

    pub fn jump_if_not_true(
        &mut self,
        test: VarId,
        offset: JumpOffset
    ) -> Result<ArraySize, RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].jump_if_not_true(test, offset)
    }

    pub fn new_dict(&mut self, var_id: VarId) -> Result<(), RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].new_dict(var_id)
    }

    pub fn get_dict(&mut self, dict: VarId, sym: VarId, dest: VarId) -> Result<(), RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].get_dict(dict, sym, dest)
    }

    pub fn set_dict(&mut self, dict: VarId, sym: VarId, src: VarId) -> Result<(), RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].set_dict(dict, sym, src)
    }

    pub fn remove_dict(&mut self, dict: VarId, sym: VarId, dest: VarId) -> Result<(), RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].remove_dict(dict, sym, dest)
    }

    pub fn new_list(&mut self, var_id: VarId) -> Result<(), RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].new_list(var_id)
    }

    pub fn get_list(&mut self, list: VarId, index: VarId, dest: VarId) -> Result<(), RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].get_list(list, index, dest)
    }

    pub fn set_list(&mut self, list: VarId, index: VarId, src: VarId) -> Result<(), RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].set_list(list, index, src)
    }

    pub fn push_list(&mut self, list: VarId, src: VarId) -> Result<(), RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].push_list(list, src)
    }

    pub fn pop_list(&mut self, list: VarId, dest: VarId) -> Result<(), RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].pop_list(list, dest)
    }

    pub fn load_num(&mut self, var_id: VarId, num: isize) -> Result<ArraySize, RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].load_num(var_id, num)
    }

    pub fn copy(&mut self, dest: VarId, src: VarId) -> Result<(), RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].copy(dest, src)
    }

    pub fn call(&mut self, func: VarId, dest: VarId) -> Result<(), RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].call(func, dest)
    }

    pub fn get_temp(&mut self) -> VarId {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].get_temp()
    }

    pub fn gen_return(&mut self, dest: VarId) -> Result<(), RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].gen_return(dest)
    }

    pub fn add(&mut self, dest: VarId, op1: VarId, op2: VarId) -> Result<(), RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].add(dest, op1, op2)
    }

    pub fn sub(&mut self, dest: VarId, op1: VarId, op2: VarId) -> Result<(), RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].sub(dest, op1, op2)
    }

    pub fn print(&mut self, var: VarId) -> Result<ArraySize, RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].print(var)
    }

    pub fn load_sym(&mut self, dest: VarId, name: String) -> Result<(), RuntimeError> {
        if let Value::Symbol(sym_id) = self.mem.lookup_sym(&name).value() {
            let top_idx = self.function_stack.len() - 1;
            self.function_stack[top_idx].load_sym(dest, sym_id);
            return Ok(());
        }
        unreachable!("lookup sym always returns sym id")
    }

    pub fn decl_var(&mut self, name: String) -> VarId {
        if let Value::Symbol(sym_id) = self.mem.lookup_sym(&name).value() {
            let var_kind = if self.is_global_scope() {
                VarKind::Global
            } else {
                VarKind::Local
            };

            let var_id = VarId::Symbol(sym_id);
            let top_idx = self.function_stack.len() - 1;

            self.function_stack[top_idx].decl_var(var_kind, var_id);

            return var_id;
        } else {
            unreachable!("lookup sym did always returns sym type");
        }
    }

    pub fn find_and_close_var(&mut self, name: String) -> Result<VarId, RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        if let Value::Symbol(sym_id) = self.mem.lookup_sym(&name).value() {
            let var_id = VarId::Symbol(sym_id);

            for (frame_offset, func_gen) in self.function_stack.iter_mut().rev().enumerate() {
                if func_gen.is_declared(var_id) {
                    let reg_offset = func_gen.bind(var_id)?;

                    self.function_stack[top_idx].close_var(
                        u8::try_from(frame_offset).unwrap(),
                        reg_offset,
                        var_id,
                    );
                    return Ok(var_id);
                }
            }

            self.function_stack[top_idx].decl_var(VarKind::Global, var_id);
            return Ok(var_id);
        } else {
            unreachable!("lookup sym did always returns sym type");
        }
    }

    fn is_global_scope(&self) -> bool {
        self.function_stack.len() == 1
    }

    // pushes a function onto the stack, generated code always
    // targets the top function
    pub fn push_func(&mut self, _args: Vec<String>) -> Result<(), RuntimeError> {
        // TODO use the args!!!

        self.function_stack.push(FunctionGenerator::new(
            Function::new_default(self.mem)?,
            self.mem,
        ));

        Ok(())
    }

    pub fn pop_func(&mut self, var_id: VarId) -> Result<(), RuntimeError> {
        let func_ptr = self.function_stack.pop().unwrap().alloc_func()?;
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].load_and_close_func(var_id, func_ptr)?;
        Ok(())
    }
}

impl<'guard> FunctionGenerator<'guard> {
    pub fn new(function: Function, mem: &'guard MutatorView<'guard>) -> FunctionGenerator<'guard> {
        let vars = HashMap::new();
        let bindings = vec![];
        let temp_counter = 0;
        let is_closure = false;

        FunctionGenerator {
            function,
            bindings,
            vars,
            temp_counter,
            mem,
            is_closure,
        }
    }

    pub fn decl_var(&mut self, new_var_kind: VarKind, var_id: VarId) -> VarId {
        if let Some(existing_var) = self.vars.get(&var_id) {
            match existing_var.kind {
                VarKind::Local => panic!("VarId was already declared in this scope!"),
                VarKind::Global => match new_var_kind {
                    VarKind::Global => return var_id,
                    VarKind::Local => {
                        if let Some(bind_idx) = existing_var.bind_index {
                            self.bindings[bind_idx as usize] = Binding::Freed;
                        }
                    }
                    VarKind::Temp | VarKind::Upvalue { .. } => {
                        unreachable!("decl kind is always local or global")
                    }
                },
                VarKind::Upvalue { .. } => match new_var_kind {
                    VarKind::Global => unreachable!("cannot have an upvalue in the global scope"),
                    VarKind::Local => {
                        if let Some(bind_idx) = existing_var.bind_index {
                            self.bindings[bind_idx as usize] = Binding::Freed;
                        }
                    }
                    VarKind::Temp | VarKind::Upvalue { .. } => {
                        unreachable!("decl kind is always local or global")
                    }
                },
                VarKind::Temp => unreachable!("temp variable found with symbol id"),
            }
        }

        let new_var = Var {
            kind: new_var_kind,
            bind_index: None,
        };

        self.vars.insert(var_id, new_var);

        var_id
    }

    pub fn load_and_close_func(
        &mut self,
        var_id: VarId,
        func_ptr: ScopedPtr<'guard, Function>,
    ) -> Result<(), RuntimeError> {
        let literal_id = self.push_lit(func_ptr.as_tagged(self.mem))?;
        let dest = self.bind(var_id)?;

        self.push_code(Opcode::LoadLiteral { dest, literal_id })?;

        if self.is_closure {
            // self.push_code(Opcode::MakeClosure { dest, function })?;
            todo!()
        }

        Ok(())
    }

    pub fn backpatch(&mut self, instruction: ArraySize, offset: JumpOffset) -> Result<(), RuntimeError> {
        self.function.code(self.mem).update_jump_offset(self.mem, instruction, offset)
    }

    pub fn jump(&mut self, offset: JumpOffset) -> Result<ArraySize, RuntimeError> {
        self.push_code(Opcode::Jump { offset })
    }

    pub fn jump_if_true(&mut self, test_var: VarId, offset: JumpOffset) -> Result<ArraySize, RuntimeError> {
        let test = self.bind(test_var)?;
        self.push_code(Opcode::JumpIfTrue { test, offset })
    }

    pub fn jump_if_not_true(&mut self, test_var: VarId, offset: JumpOffset) -> Result<ArraySize, RuntimeError> {
        let test = self.bind(test_var)?;
        self.push_code(Opcode::JumpIfNotTrue { test, offset })
    }

    pub fn nop(&mut self) -> Result<ArraySize, RuntimeError> {
        self.push_code(Opcode::NoOp)
    }

    pub fn copy(&mut self, dest_var: VarId, src_var: VarId) -> Result<(), RuntimeError> {
        let dest = self.bind(dest_var)?;
        self.activate(dest);
        let src = self.bind(src_var)?;
        self.push_code(Opcode::CopyRegister { dest, src })?;
        self.store_destination_if_nonlocal(dest_var)?;
        self.deactivate(dest);
        Ok(())
    }

    pub fn call(&mut self, function: VarId, dest: VarId) -> Result<(), RuntimeError> {
        let function = self.bind(function)?;
        if function == 255 {
            todo!("functions cannot be called from the last register!")
        }
        self.activate(function);

        let mut call_site: Register = 0;
        for (reg, binding) in self.bindings.iter().enumerate() {
            if 255 < reg {
                break;
            }

            match binding {
                Binding::Freed => {}
                Binding::Used(_) | Binding::Active(_) => {
                    if reg == 255 {
                        call_site = 255 as Register;
                    } else {
                        call_site = (reg + 1) as Register;
                    }
                }
            }
        }

        if (call_site as usize) == self.bindings.len() {
            self.bindings.push(Binding::Freed);
        }

        if (call_site == 255) && (self.bindings[255] != Binding::Freed) {
            self.evict(255)?;
        }

        self.bind_to(call_site, dest)?;
        self.push_code(Opcode::Call {
            function,
            dest: call_site,
        })?;
        self.deactivate(function);

        Ok(())
    }

    pub fn new_dict(&mut self, dest_var: VarId) -> Result<(), RuntimeError> {
        let dest = self.bind(dest_var)?;
        self.push_code(Opcode::NewDict { dest })?;
        self.store_destination_if_nonlocal(dest_var)?;
        Ok(())
    }

    pub fn set_dict(
        &mut self,
        dict_var: VarId,
        sym_var: VarId,
        src_var: VarId
    ) -> Result<(), RuntimeError> {
        let (dict, symbol, src) = self.activate_and_bind3(dict_var, sym_var, src_var)?;
        self.push_code(Opcode::SetDict { dict, symbol, src })?;
        self.deactivate(dict);
        self.deactivate(symbol);
        self.deactivate(src);
        Ok(())
    }

    pub fn get_dict(
        &mut self,
        dict_var: VarId,
        sym_var: VarId,
        dest_var: VarId
    ) -> Result<(), RuntimeError> {
        let (dict, symbol, dest) = self.activate_and_bind3(dict_var, sym_var, dest_var)?;
        self.push_code(Opcode::GetDict { dict, symbol, dest })?;
        self.deactivate(dict);
        self.deactivate(symbol);
        self.deactivate(dest);
        Ok(())
    }

    pub fn remove_dict(
        &mut self,
        dict_var: VarId,
        sym_var: VarId,
        dest_var: VarId
    ) -> Result<(), RuntimeError> {
        let (dict, symbol, dest) = self.activate_and_bind3(dict_var, sym_var, dest_var)?;
        self.push_code(Opcode::RemoveDict { dict, symbol, dest })?;
        self.deactivate(dict);
        self.deactivate(symbol);
        self.deactivate(dest);
        Ok(())
    }

    pub fn new_list(&mut self, dest_var: VarId) -> Result<(), RuntimeError> {
        let dest = self.bind(dest_var)?;
        self.push_code(Opcode::NewList { dest })?;
        self.store_destination_if_nonlocal(dest_var)?;
        Ok(())
    }

    pub fn push_list(&mut self, list_var: VarId, src_var: VarId) -> Result<(), RuntimeError> {
        let list = self.bind(list_var)?;
        self.activate(list);
        let src = self.bind(src_var)?;

        self.push_code(Opcode::PushList { list, src })?;

        self.deactivate(list);

        Ok(())
    }

    pub fn pop_list(&mut self, list_var: VarId, dest_var: VarId) -> Result<(), RuntimeError> {
        let list = self.bind(list_var)?;
        self.activate(list);
        let dest = self.bind(dest_var)?;

        self.push_code(Opcode::PopList { list, dest })?;

        self.deactivate(list);

        Ok(())
    }

    pub fn set_list(&mut self, list_var: VarId, index_var: VarId, src_var: VarId) -> Result<(), RuntimeError> {
        let (list, index, src) = self.activate_and_bind3(list_var, index_var, src_var)?;

        self.push_code(Opcode::SetList { list, index, src })?;

        self.deactivate(list);
        self.deactivate(index);
        self.deactivate(src);

        Ok(())
    }

    pub fn get_list(&mut self, list_var: VarId, index_var: VarId, dest_var: VarId) -> Result<(), RuntimeError> {
        let (list, index, dest) = self.activate_and_bind3(list_var, index_var, dest_var)?;

        self.push_code(Opcode::GetList { list, index, dest })?;

        self.deactivate(list);
        self.deactivate(index);
        self.deactivate(dest);

        Ok(())
    }

    pub fn add(&mut self, dest_var: VarId, op1: VarId, op2: VarId) -> Result<(), RuntimeError> {
        let (dest, reg1, reg2) = self.activate_and_bind3(dest_var, op1, op2)?;

        self.push_code(Opcode::Add { dest, reg1, reg2 })?;
        self.store_destination_if_nonlocal(dest_var)?;
        self.deactivate(dest);
        self.deactivate(reg1);
        self.deactivate(reg2);

        Ok(())
    }

    pub fn sub(&mut self, dest_var: VarId, op1: VarId, op2: VarId) -> Result<(), RuntimeError> {
        let (dest, reg1, reg2) = self.activate_and_bind3(dest_var, op1, op2)?;

        self.push_code(Opcode::Subtract {
            dest,
            left: reg1,
            right: reg2,
        })?;
        self.store_destination_if_nonlocal(dest_var)?;
        self.deactivate(dest);
        self.deactivate(reg1);
        self.deactivate(reg2);

        Ok(())
    }

    pub fn load_num(&mut self, var_id: VarId, num: isize) -> Result<ArraySize, RuntimeError> {
        let dest = self.bind(var_id)?;

        // if num < TAG_NUM_MIN || TAG_NUM_MAX < num {
        //  create a number object
        //  load the number into the literals
        //  load the literal id
        // }
        if num < (i16::MIN as isize) || (i16::MAX as isize) < num {
            // push a tagged num literal
            // load the literal
            todo!("load num as")
        } else {
            self.push_code(Opcode::LoadInteger {
                dest,
                integer: i16::try_from(num).unwrap(),
            })
        }
    }

    pub fn load_sym(&mut self, var_id: VarId, sym_id: usize) -> Result<ArraySize, RuntimeError> {
        let dest = self.bind(var_id)?;
        // TODO allow for more symbols
        if sym_id < (u16::MIN as usize) || (u16::MAX as usize) < sym_id {
            todo!("create symbol literal")
        } else {
            self.push_code(Opcode::LoadSymbol {
                dest,
                symbol: u16::try_from(sym_id).unwrap(),
            })
        }
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

    pub fn print(&mut self, var_id: VarId) -> Result<ArraySize, RuntimeError> {
        let dest = self.bind(var_id)?;
        self.push_code(Opcode::Print { dest })
    }

    // ===================== PRIVATE FUNCS ====================================
    // ===================== PRIVATE FUNCS ====================================
    // ===================== PRIVATE FUNCS ====================================

    fn activate_and_bind3(
        &mut self,
        var1: VarId,
        var2: VarId,
        var3: VarId,
    ) -> Result<(Register, Register, Register), RuntimeError> {
        self.activate_if_bound(var1);
        self.activate_if_bound(var2);
        self.activate_if_bound(var3);

        let reg1 = self.bind(var1)?;
        self.activate(reg1);

        let reg2 = self.bind(var2)?;
        self.activate(reg2);

        let reg3 = self.bind(var3)?;
        self.activate(reg3);

        Ok((reg1, reg2, reg3))
    }

    fn is_declared(&mut self, var_id: VarId) -> bool {
        self.vars.get(&var_id).is_some()
    }

    fn close_var(&mut self, _frame_offset: u8, _frame_register: u8, _var_id: VarId) {
        todo!()
        /*
        let upvalue = Var {
            kind: VarKind::Upvalue { upvalue_id: self.next_upvalue_id(), frame_offset, frame_register },
            bind_index: None,
        }
        */
    }

    fn activate_if_bound(&mut self, var: VarId) {
        if let Some(n) = self.vars.get(&var).unwrap().bind_index {
            if n < 256 {
                self.activate(n as Register);
            }
        }
    }

    fn store_destination_if_nonlocal(&mut self, dest: VarId) -> Result<(), RuntimeError> {
        let dest_var = self.vars.get(&dest).unwrap().clone();

        match dest_var.kind {
            VarKind::Global => {
                match dest {
                    VarId::Symbol(symbol) => {
                        let temp_id = self.get_temp();
                        let temp = self.bind(temp_id)?;
                        let dest_reg = Register::try_from(dest_var.bind_index.unwrap()).unwrap();

                        // TODO: if symbol is too large create a load literal instruction!
                        // this try from u16 will fail if we have more than u16::max symbols in the program
                        self.push_code(Opcode::LoadSymbol {
                            dest: temp,
                            symbol: u16::try_from(symbol).unwrap(),
                        })?;
                        self.push_code(Opcode::StoreGlobal {
                            src: dest_reg,
                            name: temp,
                        })?;

                        self.free(dest); // destination can be freed since the value was stored in the global
                        self.free(temp_id);
                    }
                    VarId::Temp(_) => {
                        unreachable!("global cannot be temp")
                    }
                }
            }
            VarKind::Upvalue {
                ..
            } => {
                unimplemented!("have yet to implement storing upvalues")
            }
            _ => {}
        }

        Ok(())
    }

    // return reg this way we can write let reg self.active(self.bind(var_id));
    fn activate(&mut self, reg: Register) {
        match self.bindings[reg as usize] {
            Binding::Freed => panic!("cannot activate free binding"),
            Binding::Used(var_id) => {
                self.bindings[reg as usize] = Binding::Active(var_id);
            }
            Binding::Active(_) => {} //already active, do nothing
        }
    }

    fn deactivate(&mut self, reg: Register) {
        match self.bindings[reg as usize] {
            Binding::Active(var_id) => {
                self.bindings[reg as usize] = Binding::Used(var_id);
            }
            _ => {}
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
    fn bind(&mut self, var_id: VarId) -> Result<u8, RuntimeError> {
        let mut var = self.vars.get(&var_id).unwrap().clone();

        match var.bind_index {
            None => {
                let new_bind_index = self.bind_reg(var_id)?;
                var.bind_index = Some(u16::try_from(new_bind_index).unwrap());
            }
            Some(bind_index) => {
                if 255 < bind_index {
                    let new_bind_index = self.bind_reg(var_id)?;
                    let overflow_id = u16::try_from(bind_index - 256).unwrap();
                    var.bind_index = Some(u16::try_from(new_bind_index).unwrap());
                    self.bindings[bind_index as usize] = Binding::Freed;
                    self.push_code(Opcode::LoadOverflow {
                        dest: new_bind_index,
                        overflow_id,
                    })?;
                } else {
                    // nothing to do we already got a good binding :)
                    return Ok(var.bind_index.unwrap() as u8);
                }
            }
        };

        self.vars.insert(var_id, var);

        let new_bind_index = var.bind_index.unwrap();

        self.activate(new_bind_index as u8);
        self.load_if_nonlocal(var, var_id)?;
        self.deactivate(new_bind_index as u8);

        Ok(new_bind_index as u8)
    }

    fn bind_to(&mut self, reg: Register, var_id: VarId) -> Result<(), RuntimeError> {
        match self.bindings[reg as usize] {
            Binding::Freed => {}
            Binding::Used(_) => self.evict(reg)?,
            Binding::Active(_) => {
                panic!("Attempted to bind to an active register!");
            }
        }

        self.bindings[reg as usize] = Binding::Used(var_id);
        let mut var = self.vars.get(&var_id).unwrap().clone();
        var.bind_index = Some(reg as u16);
        self.vars.insert(var_id, var);

        Ok(())
    }

    fn load_if_nonlocal(&mut self, var: Var, var_id: VarId) -> Result<(), RuntimeError> {
        match var.kind {
            VarKind::Global => {
                // if the variable is a global create a load global instr
                match var_id {
                    VarId::Symbol(symbol) => {
                        let temp_id = self.get_temp();
                        let temp = self.bind(temp_id)?;
                        let dest_reg = u8::try_from(var.bind_index.unwrap()).unwrap();

                        // TODO: if symbol is too large create a load literal instruction!
                        // this try from u16 will fail if we have more than u16::max symbols in the program
                        self.push_code(Opcode::LoadSymbol {
                            dest: temp,
                            symbol: u16::try_from(symbol).unwrap(),
                        })?;
                        self.push_code(Opcode::LoadGlobal {
                            dest: dest_reg,
                            name: temp,
                        })?;

                        self.free(temp_id);
                    }
                    VarId::Temp(_) => {
                        unreachable!("global cannot be temp")
                    }
                }
            }
            VarKind::Upvalue {
                ..
            } => {
                // if the variable is an upvalue create a load upvalue instr
                unimplemented!("cannot bind upvalues yet")
            }
            _ => {}
        }

        Ok(())
    }

    fn find_free_reg(&mut self) -> Option<Register> {
        let mut index = 0;

        while index < self.bindings.len() && index < 256 {
            let binding = self.bindings[index];

            match binding {
                Binding::Freed => {
                    return Some(u8::try_from(index).unwrap());
                }
                _ => {
                    index += 1;
                }
            }
        }

        if index == self.bindings.len() && self.bindings.len() < 256 {
            self.bindings.push(Binding::Freed);
            return Some(u8::try_from(index).unwrap());
        }

        None
    }

    fn find_used_reg(&mut self) -> Option<Register> {
        let mut index = 0;

        while index < self.bindings.len() && index < 256 {
            let binding = self.bindings[index];

            match binding {
                Binding::Used(_) => {
                    return Some(u8::try_from(index).unwrap());
                }
                _ => {
                    index += 1;
                }
            }
        }

        None
    }

    fn bind_reg(&mut self, var_id: VarId) -> Result<u8, RuntimeError> {
        if let Some(free_reg) = self.find_free_reg() {
            self.bindings[free_reg as usize] = Binding::Used(var_id);
            println!("FREE_REG: {}", free_reg);
            return Ok(u8::try_from(free_reg).unwrap());
        }

        if let Some(evict_reg) = self.find_used_reg() {
            self.evict(evict_reg)?;
            self.bindings[evict_reg as usize] = Binding::Used(var_id);
            println!("EVICT_REG: {}", evict_reg);
            return Ok(u8::try_from(evict_reg).unwrap());
        }

        unreachable!("")
    }

    fn evict(&mut self, evict_reg: Register) -> Result<(), RuntimeError> {
        let var_id = match self.bindings[evict_reg as usize] {
            Binding::Active(_) => panic!("tried to evict active register"),
            Binding::Used(var_id) => var_id,
            Binding::Freed => return Ok(()),
        };
        let mut var = self.vars.get(&var_id).unwrap().clone();
        let mut overflow_id = 0;
        println!("REG: {}", evict_reg);
        println!("VAR_ID: {:?}", var_id);
        println!("VAR: {:?}", var);
        let bind_index = var.bind_index.unwrap();
        let src = u8::try_from(bind_index).unwrap();

        if let Some(free_reg) = self.find_free_reg() {
            var.bind_index = Some(free_reg as u16);
            self.vars.insert(var_id, var);
            self.bindings.push(Binding::Used(var_id));
            self.push_code(Opcode::CopyRegister {
                dest: free_reg,
                src,
            })?;
            return Ok(());
        }

        let mut index = 256;
        while index < self.bindings.len() {
            let binding = self.bindings[index];

            match binding {
                Binding::Freed => {
                    self.bindings[index] = Binding::Used(var_id);
                    overflow_id = index - 256;
                    break;
                }
                Binding::Used(_) => index += 1,
                Binding::Active(_) => unreachable!("an overflow binding cannot be active"),
            }
        }

        if index == self.bindings.len() {
            self.bindings.push(Binding::Used(var_id));
            overflow_id = index - 256;
        }

        var.bind_index = Some(u16::try_from(index).unwrap());

        self.vars.insert(var_id, var);

        self.push_code(Opcode::StoreOverflow {
            overflow_id: u16::try_from(overflow_id).unwrap(),
            src,
        })?;

        Ok(())
    }

    fn push_code(&self, code: Opcode) -> Result<ArraySize, RuntimeError> {
        let bytecode = self.function.code(self.mem);
        bytecode.push(self.mem, code)?;
        Ok(bytecode.last_instruction())
    }

    fn push_lit(&self, literal: TaggedScopedPtr<'guard>) -> Result<LiteralId, RuntimeError> {
        let bytecode = self.function.code(self.mem);

        bytecode.push_lit(self.mem, literal)
    }

    fn free(&mut self, var_id: VarId) {
        let var: &Var = self.vars.get(&var_id).unwrap();

        match var.bind_index {
            None => {}
            Some(idx) => {
                self.bindings[idx as usize] = Binding::Freed;
            }
        }

        self.vars.insert(
            var_id,
            Var {
                kind: var.kind,
                bind_index: None,
            },
        );
    }

    // called when the function generator is popped off the function stack
    fn alloc_func(mut self) -> Result<ScopedPtr<'guard, Function>, RuntimeError> {
        println!("CODE: {}", self.function.code(self.mem));
        if 256 < self.bindings.len() {
            let overflow_capacity = u16::try_from(self.bindings.len() - 256).unwrap();
            self.function.set_overflow_capacity(overflow_capacity);
        }
        self.mem.alloc(self.function)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::vm::SVM;

    struct OverflowTestCompiler {}

    impl Compiler for OverflowTestCompiler {
        fn compile<'guard>(self, gen: &mut Generator) -> Result<(), RuntimeError> {
            let result = gen.decl_var("result".to_string());
            let one = gen.get_temp();
            gen.load_num(one, 1)?;
            gen.load_num(result, 1)?;
            // only 256 variables can be bound at once
            // this should cause some evict instructions to be generated
            // and for some variables to be evicted
            for _ in 0..300 {
                let temp = gen.get_temp();
                gen.copy(temp, one)?;
                gen.add(result, result, temp)?;
            }

            gen.gen_return(result)?;

            Ok(())
        }
    }

    #[test]
    fn test_overflow() {
        let mut vm = SVM::new();
        let test_compiler = OverflowTestCompiler {};

        vm.compile(test_compiler).expect("compiles");
        vm.execute().expect("no errors");
    }
}
