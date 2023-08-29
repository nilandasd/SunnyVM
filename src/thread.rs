use std::cell::{Cell, RefCell, RefMut};
use std::io::{self, Write};

use crate::array::ArraySize;
use crate::bytecode::{ByteCode, InstructionStream, Opcode};
use crate::callframe::{CallFrame, CallFrameList};
use crate::container::{
    Container, FillAnyContainer, HashIndexedAnyContainer, IndexedContainer, SliceableContainer,
    StackAnyContainer, StackContainer,
};
use crate::dict::Dict;
use crate::error::{err_eval, RuntimeError};
use crate::function::{Closure, Function};
use crate::list::List;
use crate::memory::MutatorView;
use crate::number::{NumberObject, TAG_NUM_MAX, TAG_NUM_MIN};
use crate::safe_ptr::{CellPtr, MutatorScope, ScopedPtr, TaggedCellPtr, TaggedScopedPtr};
use crate::tagged_ptr::{TaggedPtr, Value};
use crate::upvalue::{env_upvalue_lookup, Upvalue};

pub const RETURN_REG: usize = 0;
pub const ENV_REG: usize = 1;
pub const FIRST_ARG_REG: usize = 2;

#[derive(PartialEq)]
pub enum EvalStatus<'guard> {
    Pending,
    Return(TaggedScopedPtr<'guard>),
}

pub struct Thread {
    frames: CellPtr<CallFrameList>,
    stack: CellPtr<List>,
    stack_base: Cell<ArraySize>,
    upvalues: CellPtr<Dict>,
    globals: CellPtr<Dict>,
    instr: CellPtr<InstructionStream>,
}

impl Thread {
    pub fn alloc<'guard>(
        mem: &'guard MutatorView,
    ) -> Result<ScopedPtr<'guard, Thread>, RuntimeError> {
        let frames = CallFrameList::alloc_with_capacity(mem, 16)?;
        let stack = List::alloc_with_capacity(mem, 256)?;
        let upvalues = Dict::alloc(mem)?;
        let globals = Dict::alloc(mem)?;
        let blank_code = ByteCode::alloc(mem)?;
        let instr = InstructionStream::alloc(mem, blank_code)?;

        stack.fill(mem, 256, mem.nil())?;

        mem.alloc(Thread {
            frames: CellPtr::new_with(frames),
            stack: CellPtr::new_with(stack),
            stack_base: Cell::new(0),
            upvalues: CellPtr::new_with(upvalues),
            globals: CellPtr::new_with(globals),
            instr: CellPtr::new_with(instr),
        })
    }

    fn upvalue_lookup<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        location: ArraySize,
    ) -> Result<(TaggedScopedPtr<'guard>, ScopedPtr<'guard, Upvalue>), RuntimeError> {
        let upvalues = self.upvalues.get(guard);

        // Convert the location integer to a TaggedScopedPtr for passing
        // into the Thread's upvalues Dict
        let location_ptr = TaggedScopedPtr::new(guard, TaggedPtr::number(location as isize));

        // Lookup upvalue in upvalues dict
        match upvalues.lookup(guard, location_ptr) {
            Ok(upvalue_ptr) => {
                // Return it and the tagged-pointer version of the location number
                match *upvalue_ptr {
                    Value::Upvalue(upvalue) => Ok((location_ptr, upvalue)),
                    _ => unreachable!(),
                }
            }
            Err(e) => Err(e),
        }
    }

    fn upvalue_lookup_or_alloc<'guard>(
        &self,
        mem: &'guard MutatorView,
        location: ArraySize,
    ) -> Result<(TaggedScopedPtr<'guard>, ScopedPtr<'guard, Upvalue>), RuntimeError> {
        match self.upvalue_lookup(mem, location) {
            Ok(v) => Ok(v),
            Err(_) => {
                let upvalues = self.upvalues.get(mem);
                let upvalue = Upvalue::alloc(mem, location)?;
                let location_ptr = TaggedScopedPtr::new(mem, TaggedPtr::number(location as isize));
                upvalues.assoc(mem, location_ptr, upvalue.as_tagged(mem))?;

                Ok((location_ptr, upvalue))
            }
        }
    }

    fn eval_next_instr<'guard>(
        &self,
        mem: &'guard MutatorView,
        output_stream: &mut Box<dyn Write>,
    ) -> Result<EvalStatus<'guard>, RuntimeError> {
        // TODO not all these locals are required in every opcode - optimize and get them only
        // where needed
        let frames = self.frames.get(mem);
        let stack = self.stack.get(mem);
        let globals = self.globals.get(mem);
        let instr = self.instr.get(mem);

        stack.access_slice(mem, |full_stack| {
            let stack_base = self.stack_base.get() as usize;
            let window = &mut full_stack[stack_base..stack_base + 256];
            let opcode = instr.get_next_opcode(mem)?;

            match opcode {
                Opcode::NoOp => return Ok(EvalStatus::Pending),

                Opcode::Return { reg } => {
                    let result = window[reg as usize].get_ptr();
                    window[RETURN_REG].set_to_ptr(result);

                    frames.pop(mem)?;

                    if frames.length() == 0 {
                        return Ok(EvalStatus::Return(window[RETURN_REG].get(mem)));
                    } else {
                        let frame = frames.top(mem)?;
                        self.stack_base.set(frame.base);
                        instr.switch_frame(frame.function.get(mem).code(mem), frame.ip.get());
                    }
                }

                Opcode::LoadLiteral { dest, literal_id } => {
                    let literal_ptr = instr.get_literal(mem, literal_id)?;
                    window[dest as usize].set_to_ptr(literal_ptr);
                }

                // Evaluate whether the `test` register contains `nil` - if so, set the `dest`
                // register to the symbol "true", otherwise set it to `nil`
                Opcode::IsNil { dest, test } => {
                    todo!()
                    /*
                    let test_val = window[test as usize].get(mem);

                    match *test_val {
                        Value::Nil => window[dest as usize].set(mem.lookup_sym("true")),
                        _ => window[dest as usize].set_to_nil(),
                    }
                    */
                }

                // Evaluate whether the `test` register contains an atomic value - i.e. a
                // non-container type. Set the `dest` register to "true" or `nil`.
                Opcode::IsAtom { dest, test } => {
                    todo!()
                    /*
                    let test_val = window[test as usize].get(mem);

                    match *test_val {
                        Value::Nil => window[dest as usize].set_to_nil(),
                        // TODO what other types?
                        _ => window[dest as usize].set(mem.lookup_sym("true")),
                    }
                    */
                }

                Opcode::IsIdentical { dest, test1, test2 } => {
                    todo!()
                    /*
                    let test1_val = window[test1 as usize].get_ptr();
                    let test2_val = window[test2 as usize].get_ptr();

                    if test1_val == test2_val {
                        window[dest as usize].set(mem.lookup_sym("true"));
                    } else {
                        window[dest as usize].set(mem.nil());
                    }
                    */
                }

                Opcode::Jump { offset } => {
                    instr.jump(offset);
                }

                Opcode::JumpIfTrue { test, offset } => {
                    let test_val = window[test as usize].get(mem);
                    let true_sym = mem.lookup_sym("true"); // TODO preload keyword syms

                    if test_val == true_sym {
                        instr.jump(offset)
                    }
                }

                Opcode::JumpIfNotTrue { test, offset } => {
                    let test_val = window[test as usize].get(mem);
                    let true_sym = mem.lookup_sym("true");

                    if test_val != true_sym {
                        instr.jump(offset)
                    }
                }

                Opcode::LoadNil { dest } => {
                    window[dest as usize].set_to_nil();
                }

                Opcode::LoadInteger { dest, integer } => {
                    let tagged_ptr = TaggedPtr::literal_integer(integer);
                    window[dest as usize].set_to_ptr(tagged_ptr);
                }

                Opcode::LoadSymbol { dest, symbol } => {
                    let tagged_ptr = TaggedPtr::literal_symbol(symbol);
                    window[dest as usize].set_to_ptr(tagged_ptr);
                }

                Opcode::LoadGlobal { dest, name } => {
                    let name_val = window[name as usize].get(mem);

                    if let Value::Symbol(symbol) = *name_val {
                        let lookup_result = globals.lookup(mem, name_val);

                        match lookup_result {
                            Ok(binding) => window[dest as usize].set(binding),
                            Err(_) => {
                                window[dest as usize]
                                    .set(TaggedScopedPtr::new(mem, TaggedPtr::nil()));
                            }
                        }
                    } else {
                        return Err(err_eval("Cannot lookup global for non-symbol type"));
                    }
                }

                Opcode::StoreGlobal { src, name } => {
                    let name_val = window[name as usize].get(mem);
                    if let Value::Symbol(_) = *name_val {
                        let src_val = window[src as usize].get(mem);
                        globals.assoc(mem, name_val, src_val)?;
                    } else {
                        return Err(err_eval("Cannot bind global to non-symbol type"));
                    }
                }

                Opcode::Call { function, dest } => {
                    let binding = window[function as usize].get(mem);
                    let new_call_frame = |function| -> Result<(), RuntimeError> {
                        let current_frame_ip = instr.get_next_ip();
                        frames.access_slice(mem, |f| {
                            f.last()
                                .expect("No CallFrames in slice!")
                                .ip
                                .set(current_frame_ip)
                        });

                        // Create a new call frame, pushing it to the frame stack
                        let new_stack_base = self.stack_base.get() + dest as ArraySize;
                        let frame = CallFrame::new(function, 0, new_stack_base, mem)?;
                        frames.push(mem, frame)?;

                        // Update the instruction stream to point to the new function
                        let code = function.code(mem);
                        self.stack_base.set(new_stack_base);
                        instr.switch_frame(code, 0);

                        // Ensure the stack has 256 registers allocated
                        // TODO reset to nil to avoid accidental leakage of previous call values
                        // TODO Ruh-roh we shouldn't be able to modify the stack size from
                        // within an access_slice() call :grimace:
                        stack.fill(mem, new_stack_base + 256, mem.nil())?;

                        Ok(())
                    };

                    println!("{}", *binding);

                    match *binding {
                        Value::Function(function) => {
                            let arity = function.arity();

                            new_call_frame(function)?;
                        }

                        _ => return Err(err_eval("Type is not callable")),
                    }
                }

                // This operation should be generated by the compiler after a function definition
                // inside another function but only if the nested function refers to nonlocal
                // variables.
                // The result of this operation is a Partial with a closure environment
                Opcode::MakeClosure { dest, function } => {
                    // 1. iter over function nonlocals
                    //   - calculate absolute stack offset for each
                    //   - find existing or create new Upvalue for each
                    //   - create closure environment with list of Upvalues
                    // 2. create new Partial with environment
                    // 3. set dest to Partial
                    let function_ptr = window[function as usize].get(mem);
                    if let Value::Function(f) = *function_ptr {
                        let nonlocals = f.nonlocals(mem);
                        // Create an environment array for upvalues
                        let env = List::alloc_with_capacity(mem, nonlocals.length())?;

                        // Iter over function nonlocals, calculating absolute stack offset for each
                        nonlocals.access_slice(mem, |nonlocals| -> Result<(), RuntimeError> {
                            for compound in nonlocals {
                                // extract 8 bit register and call frame values from 16 bit nonlocal
                                // descriptors
                                let frame_offset = (*compound >> 8) as ArraySize;
                                let window_offset = (*compound & 0xff) as ArraySize;

                                // look back frame_offset frames and add the register number to
                                // calculate the absolute stack position of the value
                                let frame = frames.get(mem, frames.length() - frame_offset)?;
                                let location = frame.base + window_offset;

                                // look up, or create, the Upvalue for the location, and add it to
                                // the environment
                                let (_, upvalue) = self.upvalue_lookup_or_alloc(mem, location)?;
                                StackAnyContainer::push(&*env, mem, upvalue.as_tagged(mem))?;
                            }

                            Ok(())
                        })?;

                        // Instantiate a Partial function application from the closure environment
                        // and set the destination register
                        let partial = Closure::alloc(mem, f, Some(env))?;
                        window[dest as usize].set(partial.as_tagged(mem));
                    } else {
                        return Err(err_eval("Cannot make a closure from a non-Function type"));
                    }
                }

                Opcode::CopyRegister { dest, src } => {
                    window[dest as usize] = window[src as usize].clone();
                }

                Opcode::Add(dest, reg1, reg2) => {
                    let val1 = window[reg1 as usize].get(mem).value();
                    let val2 = window[reg2 as usize].get(mem).value();

                    match (val1, val2) {
                        (Value::NumberObject(num_obj1), Value::NumberObject(num_obj2)) => {
                            let result = num_obj1.add(&num_obj2, mem)?;
                            let tagged_result = result.as_tagged(mem).get_ptr();

                            window[dest as usize].set_to_ptr(tagged_result);
                        }
                        (Value::Number(num), Value::NumberObject(num_obj))
                        | (Value::NumberObject(num_obj), Value::Number(num)) => {
                            // add a method for add isize to a num object
                            todo!()
                        }
                        (Value::Number(num1), Value::Number(num2)) => {
                            let result = num1 + num2;

                            let tagged_result = if result < TAG_NUM_MIN || TAG_NUM_MAX < result {
                                let num_obj = NumberObject::alloc_from_isize(result, mem)?;

                                num_obj.as_tagged(mem).get_ptr()
                            } else {
                                TaggedPtr::number(result)
                            };

                            window[dest as usize].set_to_ptr(tagged_result);
                        }
                        _ => {
                            unimplemented!("have yet to implement add for non number types")
                        }
                    }
                }

                Opcode::Subtract(dest, left, right) => {
                    let val1 = window[left as usize].get(mem).value();
                    let val2 = window[right as usize].get(mem).value();

                    match (val1, val2) {
                        (Value::NumberObject(num_obj1), Value::NumberObject(num_obj2)) => {
                            let result = num_obj1.sub(&num_obj2, mem)?;
                            let tagged_result = result.as_tagged(mem).get_ptr();

                            window[dest as usize].set_to_ptr(tagged_result);
                        }
                        (Value::Number(num), Value::NumberObject(num_obj))
                        | (Value::NumberObject(num_obj), Value::Number(num)) => {
                            // add a method for add isize to a num object
                            todo!()
                        }
                        (Value::Number(num1), Value::Number(num2)) => {
                            let result = num1 - num2;

                            let tagged_result = if result < TAG_NUM_MIN || TAG_NUM_MAX < result {
                                let num_obj = NumberObject::alloc_from_isize(result, mem)?;

                                num_obj.as_tagged(mem).get_ptr()
                            } else {
                                TaggedPtr::number(result)
                            };

                            window[dest as usize].set_to_ptr(tagged_result);
                        }
                        _ => {
                            unimplemented!("have yet to implement add for non number types")
                        }
                    }
                }

                // TODO
                Opcode::Multiply { dest, reg1, reg2 } => unimplemented!(),

                // TODO
                Opcode::DivideInteger { dest, num, denom } => unimplemented!(),

                Opcode::LoadUpvalue { dest, src } => {
                    let closure_env = window[ENV_REG].get(mem);
                    let upvalue = env_upvalue_lookup(mem, closure_env, src)?;
                    window[dest as usize].set_to_ptr(upvalue.get(mem, stack)?);
                }

                Opcode::StoreUpvalue { dest, src } => {
                    let closure_env = window[ENV_REG].get(mem);
                    let upvalue = env_upvalue_lookup(mem, closure_env, dest)?;
                    upvalue.set(mem, stack, window[src as usize].get_ptr())?;
                }

                Opcode::CloseUpvalues { reg1, reg2, reg3 } => {
                    for reg in &[reg1, reg2, reg3] {
                        // Registers 0 and 1 cannot be closed over TODO: is this logic right?
                        if *reg >= FIRST_ARG_REG as u8 {
                            // calculate absolute stack offset of reg
                            let location = stack_base as ArraySize + *reg as ArraySize;
                            // find the Upvalue object by location
                            let (location_ptr, upvalue) = self.upvalue_lookup(mem, location)?;
                            // close it and unanchor from the Thread
                            upvalue.close(mem, stack)?;
                            self.upvalues.get(mem).dissoc(mem, location_ptr)?;
                        }
                    }
                }

                Opcode::LoadOverflow { dest, overflow_id } => {
                    let frame = frames.top(mem)?;
                    let overflow = frame.overflow(mem).unwrap();
                    let value = overflow.get(mem, overflow_id as u32)?;

                    window[dest as usize].set_to_ptr(value.get_ptr());
                }

                Opcode::StoreOverflow { overflow_id, src } => {
                    let frame = frames.top(mem)?;
                    let overflow = frame.overflow(mem).unwrap();
                    let value = window[src as usize].get(mem);
                    if (overflow_id as u32) < overflow.length() {
                        overflow.set(mem, overflow_id as u32, TaggedCellPtr::new_with(value))?;
                    } else {
                        StackAnyContainer::push(&*overflow, mem, value)?;
                    }
                }

                Opcode::Print { dest } => {
                    write!(output_stream, "{}\n", window[dest as usize].get(mem))?;
                }

                Opcode::NewList { dest } => {
                    let new_list = List::alloc(mem)?;
                    window[dest as usize].set_to_ptr(new_list.as_tagged(mem).get_ptr());
                }


                Opcode::SetList(list, index, src) => {
                    let list_ptr = window[list as usize].get(mem);
                    let src_ptr = window[src as usize].get(mem);
                    let index = window[index as usize].get(mem).value();
                    if let Value::List(list) = *list_ptr {
                        match index {
                            Value::Number(num) => {
                                list.set(mem, num as u32, TaggedCellPtr::from(src_ptr))?;
                            }
                            _ => return Err(err_eval("Cannot index array with non-number type")),
                        }
                    } else {
                        return Err(err_eval("Called SetList on non list type"));
                    }
                }

                Opcode::GetList(dest, list, index) => {
                    let list_ptr = window[list as usize].get(mem);
                    let index = window[index as usize].get(mem).value();
                    if let Value::List(list) = *list_ptr {
                        match index {
                            Value::Number(num) => {
                                let list_val = list.get(mem, num as u32)?;

                                window[dest as usize].set_to_ptr(list_val.get_ptr());
                            }
                            _ => return Err(err_eval("Cannot index array with non-number type")),
                        }
                    } else {
                        return Err(err_eval("Called GetList on non list type"));
                    }
                }

                Opcode::PushList { list, src } => {
                    let list_ptr = window[list as usize].get(mem);
                    let src_ptr = window[src as usize].get(mem);

                    if let Value::List(list) = *list_ptr {
                        StackAnyContainer::push(&*list, mem, src_ptr)?;
                    } else {
                        return Err(err_eval("Called PushList on non list type"));
                    }
                }

                Opcode::PopList { list, dest } => {
                    let list_ptr = window[list as usize].get(mem);

                    if let Value::List(list) = *list_ptr {
                        let value = StackAnyContainer::pop(&*list, mem)?;
                        window[dest as usize].set_to_ptr(value.get_ptr());
                    } else {
                        return Err(err_eval("Called PopList on non list type"));
                    }
                }

                Opcode::NewDict { dest } => {
                    let new_dict = Dict::alloc(mem)?;
                    window[dest as usize].set_to_ptr(new_dict.as_tagged(mem).get_ptr());
                }

                Opcode::SetDict(dict, symbol, src) => {
                    let dict_ptr = window[dict as usize].get(mem);
                    let src_ptr = window[src as usize].get(mem);
                    let symbol = window[symbol as usize].get(mem);
                    if let Value::Dict(dict) = *dict_ptr {
                        dict.assoc(mem, symbol, src_ptr)?;
                    } else {
                        return Err(err_eval("Called SetDict on non dict type"));
                    }
                }

                Opcode::GetDict(dest, dict, symbol) => {
                    let dict_ptr = window[dict as usize].get(mem);
                    let symbol = window[symbol as usize].get(mem);
                    if let Value::Dict(dict) = *dict_ptr {
                        let val = dict.lookup(mem, symbol)?;
                        window[dest as usize].set_to_ptr(val.get_ptr());
                    } else {
                        return Err(err_eval("Called GetDict on non dict type"));
                    }
                }

                Opcode::RemoveDict(dest, dict, symbol) => {
                    let dict_ptr = window[dict as usize].get(mem);
                    let symbol = window[symbol as usize].get(mem);

                    if let Value::Dict(dict) = *dict_ptr {
                        let val = dict.dissoc(mem, symbol)?;
                        window[dest as usize].set_to_ptr(val.get_ptr());
                    } else {
                        return Err(err_eval("Called GetDict on non dict type"));
                    }
                }
            }

            Ok(EvalStatus::Pending)
        })
    }

    pub fn eval_stream<'guard>(
        &self,
        mem: &'guard MutatorView,
        max_instr: ArraySize,
        output_stream: &mut Box<dyn Write>,
    ) -> Result<EvalStatus<'guard>, RuntimeError> {
        for _ in 0..max_instr {
            match self.eval_next_instr(mem, output_stream) {
                // Evaluation paused or completed without error
                Ok(exit_cond) => match exit_cond {
                    EvalStatus::Return(value) => return Ok(EvalStatus::Return(value)),
                    _ => (),
                },

                // Evaluation hit an error
                Err(rt_error) => {
                    // unwind the stack, printing a trace
                    let frames = self.frames.get(mem);

                    // Print a stack trace if the error is multiple call frames deep
                    frames.access_slice(mem, |window| {
                        if window.len() > 1 {
                            println!("Error traceback:");
                        }

                        for frame in &window[1..] {
                            println!("  {}", frame.as_string(mem));
                        }
                    });

                    // Unwind by clearing all frames from the stack
                    frames.clear(mem)?;
                    self.stack_base.set(0);

                    return Err(rt_error);
                }
            }
        }

        Ok(EvalStatus::Pending)
    }

    // Evaluate a Function completely, returning the result. The Function passed in should expect
    // no arguments.
    pub fn quick_vm_eval<'guard>(
        &self,
        mem: &'guard MutatorView,
        function: ScopedPtr<'guard, Function>,
        output_stream: &mut Box<dyn Write>,
    ) -> Result<TaggedScopedPtr<'guard>, RuntimeError> {
        let mut status = EvalStatus::Pending;
        let frames = self.frames.get(mem);
        let main_frame = CallFrame::new_main(function, mem)?;

        self.set_func(mem, function);
        frames.push(mem, main_frame)?;

        while status == EvalStatus::Pending {
            status = self.eval_stream(mem, 1024, output_stream)?;
            match status {
                EvalStatus::Return(value) => return Ok(value),
                _ => (),
            }
        }

        Err(err_eval("Unexpected end of evaluation"))
    }

    // identical to quick vm eval but doesn't take a function
    pub fn execute<'guard>(
        &self,
        mem: &'guard MutatorView,
        output_stream: &mut Box<dyn Write>,
    ) -> Result<TaggedScopedPtr<'guard>, RuntimeError> {
        let mut status = EvalStatus::Pending;

        while status == EvalStatus::Pending {
            status = self.eval_stream(mem, 1024, output_stream)?;
            match status {
                EvalStatus::Return(value) => return Ok(value),
                _ => (),
            }
        }

        Err(err_eval("Unexpected end of evaluation"))
    }

    pub fn set_func<'guard>(
        &self,
        mem: &'guard MutatorView,
        function: ScopedPtr<'guard, Function>,
    ) -> Result<(), RuntimeError> {
        let code = function.code(mem);
        let instr = self.instr.get(mem);
        let frames = self.frames.get(mem);
        let main_frame = CallFrame::new_main(function, mem)?;

        frames.push(mem, main_frame)?;
        instr.switch_frame(code, 0);

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::list::List;
    use crate::memory::{Memory, Mutator};
    use crate::safe_ptr::{ScopedPtr, TaggedScopedPtr};

    fn test_helper<'guard>(
        view: &'guard MutatorView,
        bytecode: ScopedPtr<'guard, ByteCode>,
    ) -> Result<TaggedScopedPtr<'guard>, RuntimeError> {
        let thread = Thread::alloc(view).unwrap();
        let list = List::alloc(view).unwrap();
        let nil_ptr = TaggedScopedPtr::new(view, TaggedPtr::nil());
        let function = Function::alloc(view, nil_ptr, list, bytecode, None, 0).unwrap();

        let cell_stream: RefCell<Box<dyn Write>> = RefCell::new(Box::new(io::stdout()));
        let stream = &mut *cell_stream.borrow_mut();

        thread.quick_vm_eval(view, function, stream)
    }

    #[test]
    fn test_run_no_ops() {
        let mem = Memory::new();

        struct Test {}
        impl Mutator for Test {
            type Input = ();
            type Output = ();

            fn run(
                &self,
                view: &MutatorView,
                _input: Self::Input,
            ) -> Result<Self::Output, RuntimeError> {
                let bytecode = ByteCode::alloc(view).unwrap();
                let nil_ptr = TaggedScopedPtr::new(view, TaggedPtr::nil());

                for _ in 0..10 {
                    bytecode.push(view, Opcode::NoOp)?;
                }

                bytecode.push(view, Opcode::Return { reg: 0 })?;

                let result = test_helper(view, bytecode);

                assert!(result.expect("no error") == nil_ptr);

                Ok(())
            }
        }

        let test = Test {};
        mem.mutate(&test, ()).unwrap();
    }

    #[test]
    fn test_return_literal() {
        let mem = Memory::new();

        struct Test {}
        impl Mutator for Test {
            type Input = ();
            type Output = ();

            fn run(
                &self,
                view: &MutatorView,
                _input: Self::Input,
            ) -> Result<Self::Output, RuntimeError> {
                let bytecode = ByteCode::alloc(view).unwrap();
                let literal_id = bytecode
                    .push_lit(view, TaggedScopedPtr::new(view, TaggedPtr::number(69)))
                    .unwrap();

                bytecode.push(
                    view,
                    Opcode::LoadLiteral {
                        dest: 0,
                        literal_id,
                    },
                )?;
                bytecode.push(view, Opcode::Return { reg: 0 })?;

                let result = test_helper(view, bytecode).unwrap().value();

                if let Value::Number(n) = result {
                    assert!(n == 69);
                } else {
                    assert!(false);
                }

                Ok(())
            }
        }

        let test = Test {};
        mem.mutate(&test, ()).unwrap();
    }

    #[test]
    fn test_return_nil() {
        let mem = Memory::new();

        struct Test {}
        impl Mutator for Test {
            type Input = ();
            type Output = ();

            fn run(
                &self,
                view: &MutatorView,
                _input: Self::Input,
            ) -> Result<Self::Output, RuntimeError> {
                let bytecode = ByteCode::alloc(view).unwrap();
                let nil_ptr = TaggedScopedPtr::new(view, TaggedPtr::nil());
                let literal_id = bytecode
                    .push_lit(view, TaggedScopedPtr::new(view, TaggedPtr::number(69)))
                    .unwrap();

                bytecode.push(
                    view,
                    Opcode::LoadLiteral {
                        dest: 0,
                        literal_id,
                    },
                )?;
                bytecode.push(view, Opcode::LoadNil { dest: 0 })?;
                bytecode.push(view, Opcode::Return { reg: 0 })?;

                let result = test_helper(view, bytecode).unwrap();

                assert!(result == nil_ptr);

                Ok(())
            }
        }

        let test = Test {};
        mem.mutate(&test, ()).unwrap();
    }

    #[test]
    fn test_return_addition() {
        let mem = Memory::new();

        struct Test {}
        impl Mutator for Test {
            type Input = ();
            type Output = ();

            fn run(
                &self,
                view: &MutatorView,
                _input: Self::Input,
            ) -> Result<Self::Output, RuntimeError> {
                let bytecode = ByteCode::alloc(view).unwrap();
                let literal_id = bytecode
                    .push_lit(view, TaggedScopedPtr::new(view, TaggedPtr::number(69)))
                    .unwrap();

                bytecode.push(
                    view,
                    Opcode::LoadLiteral {
                        dest: 0,
                        literal_id,
                    },
                )?;
                bytecode.push(
                    view,
                    Opcode::LoadLiteral {
                        dest: 1,
                        literal_id,
                    },
                )?;
                bytecode.push(
                    view,
                    Opcode::Add(0,0,1)
                )?;
                bytecode.push(
                    view,
                    Opcode::Add(0,0,1)
                )?;
                bytecode.push(view, Opcode::Return { reg: 0 })?;

                let result = test_helper(view, bytecode).unwrap();

                if let Value::Number(n) = result.value() {
                    assert!(n == 69 * 3);
                } else {
                    assert!(false);
                }

                Ok(())
            }
        }

        let test = Test {};
        mem.mutate(&test, ()).unwrap();
    }
}
