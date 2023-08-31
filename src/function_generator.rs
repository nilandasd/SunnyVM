use crate::number::{TAG_NUM_MAX, TAG_NUM_MIN};
use std::collections::HashMap;
use std::convert::TryFrom;

use zapalloc::ArraySize;

use crate::bytecode::{LiteralId, JumpOffset, Opcode, Register};
use crate::error::RuntimeError;
use crate::function::Function;
use crate::memory::{MutatorView, SymbolId};
use crate::safe_ptr::{ScopedPtr, TaggedScopedPtr};
use crate::tagged_ptr::TaggedPtr;

macro_rules! gen3 {
    ($fn_name:ident,
     $opcode:ident,
     $is_store_instruction:expr
     ) => {
        impl<'guard> FunctionGenerator<'guard> {
            pub fn $fn_name(
                &mut self,
                var1: VarId,
                var2: VarId,
                var3: VarId
            ) -> Result<ArraySize, RuntimeError> {
                let (reg1, reg2, reg3) = self.activate_and_bind3(var1, var2, var3)?;
                let offset = self.push_code(Opcode::$opcode(reg1, reg2, reg3))?;

                if $is_store_instruction {
                    self.store_destination_if_nonlocal(var1)?;
                }

                self.deactivate(reg1);
                self.deactivate(reg2);
                self.deactivate(reg3);
                Ok(offset)
            }
        }
    };
}

type TempId = usize;

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
pub enum VarKind {
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

pub struct FunctionGenerator<'guard> {
    mem: &'guard MutatorView<'guard>,
    function: Function,
    vars: HashMap<VarId, Var>,
    bindings: Vec<Binding>,
    temp_counter: usize,
    is_closure: bool,
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

// store instructions
gen3!(get_dict, GetDict, true);
gen3!(get_list, GetList, true);
gen3!(remove_dict, RemoveDict, true);
gen3!(add, Add, true);
gen3!(sub, Subtract, true);
gen3!(equal, Equal, true);
gen3!(not_equal, NotEqual, true);
gen3!(gt, Gt, true);
gen3!(gte, Gte, true);
gen3!(lt, Lt, true);
gen3!(lte, Lte, true);

// non store instructions
gen3!(set_dict, SetDict, false);
gen3!(set_list, SetList, false);

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

    pub fn copy(&mut self, dest_var: VarId, src_var: VarId) -> Result<ArraySize, RuntimeError> {
        let dest = self.bind(dest_var)?;
        self.activate(dest);
        let src = self.bind(src_var)?;
        let offset = self.push_code(Opcode::CopyRegister { dest, src })?;
        self.store_destination_if_nonlocal(dest_var)?;
        self.deactivate(dest);
        Ok(offset)
    }

    pub fn call(&mut self, function: VarId, dest: VarId, args: Vec<VarId>) -> Result<ArraySize, RuntimeError> {
        if 253 < args.len() {
            unimplemented!("functions with more than 253 arguments are not supported")
        }

        self.free(dest);

        let func_reg = self.bind(function)?;
        self.activate(func_reg);

        for arg in args.iter() {
            let reg = self.bind(*arg)?;
            self.activate(reg);
        }

        self.deactivate_all();

        let free_regs = self.compact();

        let required_free_regs = if args.len() == 0 {
            args.len()
        } else {
            1
        };

        let call_site = if required_free_regs > (free_regs as usize) {
            todo!("we need to evict some registers")
        } else {
            (256 - (free_regs as usize)) as u8
        };

        let mut next_arg = call_site;

        for arg in args.iter() {
            if let Some(src) = self.vars.get(&arg).unwrap().bind_index {
                self.push_code(Opcode::CopyRegister {
                    dest: next_arg,
                    src: src as u8,
                })?;
            }
            next_arg += 1;
        }


        let function = self.bind(function)?;
        let offset = self.push_code(Opcode::Call {
            function,
            dest: call_site,
        })?;

        self.free(dest);

        self.bind_to(call_site, dest)?;

        Ok(offset)
    }

    pub fn new_dict(&mut self, dest_var: VarId) -> Result<ArraySize, RuntimeError> {
        let dest = self.bind(dest_var)?;
        let offset = self.push_code(Opcode::NewDict { dest })?;
        self.store_destination_if_nonlocal(dest_var)?;
        Ok(offset)
    }

    pub fn new_list(&mut self, dest_var: VarId) -> Result<ArraySize, RuntimeError> {
        let dest = self.bind(dest_var)?;
        let offset = self.push_code(Opcode::NewList { dest })?;
        self.store_destination_if_nonlocal(dest_var)?;
        Ok(offset)
    }

    pub fn push_list(&mut self, list_var: VarId, src_var: VarId) -> Result<ArraySize, RuntimeError> {
        let list = self.bind(list_var)?;
        self.activate(list);
        let src = self.bind(src_var)?;
        let offset = self.push_code(Opcode::PushList { list, src })?;
        self.deactivate(list);
        Ok(offset)
    }

    pub fn pop_list(
        &mut self,
        list_var: VarId,
        dest_var: VarId
    ) -> Result<ArraySize, RuntimeError> {
        let list = self.bind(list_var)?;
        self.activate(list);
        let dest = self.bind(dest_var)?;
        let offset = self.push_code(Opcode::PopList { list, dest })?;
        self.deactivate(list);
        Ok(offset)
    }

    pub fn load_num(&mut self, var_id: VarId, num: isize) -> Result<ArraySize, RuntimeError> {
        let dest = self.bind(var_id)?;

        if num < TAG_NUM_MIN || TAG_NUM_MAX < num {
            //  create a number object
            //  load the number into the literals
            //  load the literal id
            todo!("create num obj literal")
        } else if num < (i16::MIN as isize) || (i16::MAX as isize) < num {
            let num_ptr = TaggedPtr::number(num as isize);
            let literal_id = self.push_lit(TaggedScopedPtr::new(self.mem, num_ptr))?;
            self.push_code(Opcode::LoadLiteral {
                dest,
                literal_id,
            })
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

    pub fn gen_return(&mut self, var_id: VarId) -> Result<ArraySize, RuntimeError> {
        let reg = self.bind(var_id)?;
        self.push_code(Opcode::Return { reg })
    }

    pub fn print(&mut self, var_id: VarId) -> Result<ArraySize, RuntimeError> {
        let dest = self.bind(var_id)?;
        self.push_code(Opcode::Print { dest })
    }

    // ===================== PRIVATE FUNCS ====================================
    // ===================== PRIVATE FUNCS ====================================
    // ===================== PRIVATE FUNCS ====================================

    fn compact(&mut self) -> u8 {
        let mut free_regs: u8 = 0;
        for index in 0..256 {
            if index == self.bindings.len() {
                if self.bindings.len() < 256 {
                    free_regs += (255 - (self.bindings.len() as u8)) + 1
                }

                break;
            }
            match self.bindings[index] {
                Binding::Freed => free_regs += 1,
                Binding::Used(_) => {
                    if 0 < free_regs {
                        self.evict(index as u8).unwrap();
                    }
                }
                Binding::Active(_) => unreachable!("no registers should be active at this point")
            }
        }

        free_regs
    }

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

    pub fn is_declared(&mut self, var_id: VarId) -> bool {
        self.vars.get(&var_id).is_some()
    }

    pub fn close_var(&mut self, _frame_offset: u8, _frame_register: u8, _var_id: VarId) {
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

    fn deactivate_all(&mut self) {
        for i in 0..256 {
            if i == self.bindings.len() { break }
            self.deactivate(i as u8)
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
    pub fn bind(&mut self, var_id: VarId) -> Result<u8, RuntimeError> {
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
        // TODO load if nonlocal
        let mut var = self.vars.get(&var_id).unwrap().clone();
        if let Some(bind_index) = var.bind_index {
            self.bindings[bind_index as usize] = Binding::Freed;

            if bind_index <= 255 {
                self.push_code(Opcode::CopyRegister {
                    dest: reg,
                    src: bind_index as u8,
                })?;
            } else {
                self.push_code(Opcode::LoadOverflow {
                    dest: reg,
                    overflow_id: bind_index - 256
                })?;
            }
        }

        if reg as usize >= self.bindings.len() {
            for _ in 0..=(reg as usize - self.bindings.len()) {
                self.bindings.push(Binding::Freed);
            }
        }

        match self.bindings[reg as usize] {
            Binding::Freed => {}
            Binding::Used(_) => self.evict(reg)?,
            Binding::Active(_) => {
                panic!("Attempted to bind to an active register!");
            }
        }

        self.bindings[reg as usize] = Binding::Used(var_id);
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
            return Ok(u8::try_from(free_reg).unwrap());
        }

        if let Some(evict_reg) = self.find_used_reg() {
            self.evict(evict_reg)?;
            self.bindings[evict_reg as usize] = Binding::Used(var_id);
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
        let bind_index = var.bind_index.unwrap();
        let src = u8::try_from(bind_index).unwrap();

        if let Some(free_reg) = self.find_free_reg() {
            var.bind_index = Some(free_reg as u16);
            self.vars.insert(var_id, var);
            self.bindings[free_reg as usize] = Binding::Used(var_id);
            self.bindings[evict_reg as usize] = Binding::Freed;
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
    pub fn alloc_func(mut self) -> Result<ScopedPtr<'guard, Function>, RuntimeError> {
        println!("CODE: {}", self.function.code(self.mem));
        if 256 < self.bindings.len() {
            let overflow_capacity = u16::try_from(self.bindings.len() - 256).unwrap();
            self.function.set_overflow_capacity(overflow_capacity);
        }
        self.mem.alloc(self.function)
    }
}
