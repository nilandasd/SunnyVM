use std::cell::Cell;

use crate::array::{Array, ArraySize};
use crate::bytecode::{ByteCode, InstructionStream, Opcode};
use crate::container::{
    Container, FillAnyContainer, HashIndexedAnyContainer, IndexedAnyContainer, IndexedContainer,
    SliceableContainer, StackAnyContainer, StackContainer,
};
use crate::dict::Dict;
use crate::error::{err_eval, RuntimeError};
use crate::function::{Function, Closure};
use crate::list::List;
use crate::memory::MutatorView;
use crate::safe_ptr::{CellPtr, MutatorScope, ScopedPtr, TaggedCellPtr, TaggedScopedPtr};
use crate::tagged_ptr::{TaggedPtr, Value};

pub const RETURN_REG: usize = 0;
pub const ENV_REG: usize = 1;
pub const FIRST_ARG_REG: usize = 2;

#[derive(PartialEq)]
pub enum EvalStatus<'guard> {
    Pending,
    Return(TaggedScopedPtr<'guard>),
}

#[derive(Clone)]
pub struct CallFrame {
    function: CellPtr<Function>,
    ip: Cell<ArraySize>,
    base: ArraySize,
}

impl CallFrame {
    pub fn new_main<'guard>(main_fn: ScopedPtr<'guard, Function>) -> CallFrame {
        CallFrame {
            function: CellPtr::new_with(main_fn),
            ip: Cell::new(0),
            base: 0,
        }
    }

    /// Instantiate a new stack frame for the given function, beginning execution at the given
    /// instruction pointer and a register window at `base`
    fn new<'guard>(
        function: ScopedPtr<'guard, Function>,
        ip: ArraySize,
        base: ArraySize,
    ) -> CallFrame {
        CallFrame {
            function: CellPtr::new_with(function),
            ip: Cell::new(ip),
            base,
        }
    }

    /// Return a string representation of this stack frame
    fn as_string<'guard>(&self, guard: &'guard dyn MutatorScope) -> String {
        let function = self.function.get(guard);
        format!("in {}", function)
    }
}

pub type CallFrameList = Array<CallFrame>;

/// A closure upvalue as generally described by Lua 5.1 implementation.
/// There is one main difference - in the Lua (and Crafting Interpreters) documentation, an upvalue
/// is closed by pointing the `location` pointer at the `closed` pointer directly in the struct.
/// This isn't a good idea _here_ because a stack location may be invalidated by the stack List
/// object being reallocated. This VM doesn't support pointers into objects.
#[derive(Clone)]
pub struct Upvalue {
    // Upvalue location can't be a pointer because it would be a pointer into the dynamically
    // alloocated stack List - the pointer would be invalidated if the stack gets reallocated.
    value: TaggedCellPtr,
    closed: Cell<bool>,
    location: ArraySize,
}

impl Upvalue {
    // Allocate a new Upvalue on the heap. The absolute stack index of the object must be
    // provided.
    fn alloc<'guard>(
        mem: &'guard MutatorView,
        location: ArraySize,
    ) -> Result<ScopedPtr<'guard, Upvalue>, RuntimeError> {
        mem.alloc(Upvalue {
            value: TaggedCellPtr::new_nil(),
            closed: Cell::new(false),
            location,
        })
    }

    fn get<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        stack: ScopedPtr<'guard, List>,
    ) -> Result<TaggedPtr, RuntimeError> {
        match self.closed.get() {
            true => Ok(self.value.get_ptr()),
            false => Ok(IndexedContainer::get(&*stack, guard, self.location)?.get_ptr()),
        }
    }

    fn set<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        stack: ScopedPtr<'guard, List>,
        ptr: TaggedPtr,
    ) -> Result<(), RuntimeError> {
        match self.closed.get() {
            true => self.value.set_to_ptr(ptr),
            false => {
                IndexedContainer::set(&*stack, guard, self.location, TaggedCellPtr::new_ptr(ptr))?
            }
        };
        Ok(())
    }

    fn close<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        stack: ScopedPtr<'guard, List>,
    ) -> Result<(), RuntimeError> {
        let ptr = IndexedContainer::get(&*stack, guard, self.location)?.get_ptr();
        self.value.set_to_ptr(ptr);
        self.closed.set(true);
        Ok(())
    }
}

/// Get the Upvalue for the index into the given closure environment.
/// Function will panic if types are not as expected.
fn env_upvalue_lookup<'guard>(
    guard: &'guard dyn MutatorScope,
    closure_env: TaggedScopedPtr<'guard>,
    upvalue_id: u8,
) -> Result<ScopedPtr<'guard, Upvalue>, RuntimeError> {
    match *closure_env {
        Value::List(env) => {
            let upvalue_ptr = IndexedAnyContainer::get(&*env, guard, upvalue_id as ArraySize)?;

            match *upvalue_ptr {
                Value::Upvalue(upvalue) => Ok(upvalue),
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

/// An execution Thread object.
/// It is composed of all the data structures required for execution of a bytecode stream -
/// register stack, call frames, closure upvalues, thread-local global associations and the current
/// instruction pointer.
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
        // create an empty stack frame array
        let frames = CallFrameList::alloc_with_capacity(mem, 16)?;

        // create a minimal value stack
        let stack = List::alloc_with_capacity(mem, 256)?;
        stack.fill(mem, 256, mem.nil())?;

        // create an empty upvalue stack->heap mapping
        let upvalues = Dict::alloc(mem)?;

        // create an empty globals dict
        let globals = Dict::alloc(mem)?;

        // create an empty instruction stream
        let blank_code = ByteCode::alloc(mem)?;
        let instr = InstructionStream::alloc(mem, blank_code)?;

        mem.alloc(Thread {
            frames: CellPtr::new_with(frames),
            stack: CellPtr::new_with(stack),
            stack_base: Cell::new(0),
            upvalues: CellPtr::new_with(upvalues),
            globals: CellPtr::new_with(globals),
            instr: CellPtr::new_with(instr),
        })
    }

    /// Retrieve an Upvalue for the given absolute stack offset.
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

    /// Retrieve an Upvalue for the given absolute stack offset or allocate a new one if none was
    /// found
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

    /// Execute the next instruction in the current instruction stream
    fn eval_next_instr<'guard>(
        &self,
        mem: &'guard MutatorView,
    ) -> Result<EvalStatus<'guard>, RuntimeError> {
        // TODO not all these locals are required in every opcode - optimize and get them only
        // where needed
        let frames = self.frames.get(mem);
        let stack = self.stack.get(mem);
        let globals = self.globals.get(mem);
        let instr = self.instr.get(mem);

        // Establish a 256-register window into the stack from the stack base
        stack.access_slice(mem, |full_stack| {
            let stack_base = self.stack_base.get() as usize;
            let window = &mut full_stack[stack_base..stack_base + 256];

            let opcode = instr.get_next_opcode(mem)?;

            match opcode {
                Opcode::NoOp => return Ok(EvalStatus::Pending),

                // Set the return register to the given register's value and pop the top call
                // frame, updating the instruction stream to the previous call frame's saved state.
                // If the call frame stack is empty, the program completed.
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
                    let test_val = window[test as usize].get(mem);

                    match *test_val {
                        Value::Nil => window[dest as usize].set(mem.lookup_sym("true")),
                        _ => window[dest as usize].set_to_nil(),
                    }
                }

                // Evaluate whether the `test` register contains an atomic value - i.e. a
                // non-container type. Set the `dest` register to "true" or `nil`.
                Opcode::IsAtom { dest, test } => {
                    let test_val = window[test as usize].get(mem);

                    match *test_val {
                        Value::Nil => window[dest as usize].set_to_nil(),
                        // TODO what other types?
                        _ => window[dest as usize].set(mem.lookup_sym("true")),
                    }
                }

                Opcode::IsIdentical { dest, test1, test2 } => {
                    let test1_val = window[test1 as usize].get_ptr();
                    let test2_val = window[test2 as usize].get_ptr();

                    if test1_val == test2_val {
                        window[dest as usize].set(mem.lookup_sym("true"));
                    } else {
                        window[dest as usize].set(mem.nil());
                    }
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

                // Lookup a global binding and put it in the register `dest`
                Opcode::LoadGlobal { dest, name } => {
                    let name_val = window[name as usize].get(mem);

                    if let Value::Symbol(_) = *name_val {
                        let lookup_result = globals.lookup(mem, name_val);

                        match lookup_result {
                            Ok(binding) => window[dest as usize].set(binding),
                            Err(_) => {
                                return Err(err_eval(&format!(
                                    "Symbol {} is not bound to a value",
                                    name_val
                                )))
                            }
                        }
                    } else {
                        return Err(err_eval("Cannot lookup global for non-symbol type"));
                    }
                }

                // Bind a symbol to the `src` register in the globals dict
                Opcode::StoreGlobal { src, name } => {
                    let name_val = window[name as usize].get(mem);
                    if let Value::Symbol(_) = *name_val {
                        let src_val = window[src as usize].get(mem);
                        globals.assoc(mem, name_val, src_val)?;
                    } else {
                        return Err(err_eval("Cannot bind global to non-symbol type"));
                    }
                }

                // Call the function referred to by the `function` register, put the result in the
                // `dest` register.
                Opcode::Call {
                    function,
                    dest,
                    arg_count,
                } => {
                    let binding = window[function as usize].get(mem);

                    // To avoid duplicating code in function and partial application cases,
                    // this is declared as a closure so it can access local variables
                    let new_call_frame = |function| -> Result<(), RuntimeError> {
                        // Modify the current call frame, saving the return ip
                        let current_frame_ip = instr.get_next_ip();
                        frames.access_slice(mem, |f| {
                            f.last()
                                .expect("No CallFrames in slice!")
                                .ip
                                .set(current_frame_ip)
                        });

                        // Create a new call frame, pushing it to the frame stack
                        let new_stack_base = self.stack_base.get() + dest as ArraySize;
                        let frame = CallFrame::new(function, 0, new_stack_base);
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

                    match *binding {
                        Value::Function(function) => {
                            let arity = function.arity();
                            if arg_count != arity {
                                return Err(err_eval(&format!(
                                    "Function {} expected {} arguments, got {}",
                                    binding,
                                    function.arity(),
                                    arg_count
                                )));
                            }

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

                // TODO
                Opcode::Add { dest, reg1, reg2 } => unimplemented!(),

                // TODO
                Opcode::Subtract { dest, left, right } => unimplemented!(),

                // TODO
                Opcode::Multiply { dest, reg1, reg2 } => unimplemented!(),

                // TODO
                Opcode::DivideInteger { dest, num, denom } => unimplemented!(),

                // Follow the indirection of an Upvalue to retrieve the value, copy the value to a
                // local register
                Opcode::GetUpvalue { dest, src } => {
                    let closure_env = window[ENV_REG].get(mem);
                    let upvalue = env_upvalue_lookup(mem, closure_env, src)?;
                    window[dest as usize].set_to_ptr(upvalue.get(mem, stack)?);
                }

                // Follow the indirection of an Upvalue to set the value from a local register
                Opcode::SetUpvalue { dest, src } => {
                    let closure_env = window[ENV_REG].get(mem);
                    let upvalue = env_upvalue_lookup(mem, closure_env, dest)?;
                    upvalue.set(mem, stack, window[src as usize].get_ptr())?;
                }

                // Move up to 3 stack register values to the Upvalue objects referring to them
                Opcode::CloseUpvalues { reg1, reg2, reg3 } => {
                    for reg in &[reg1, reg2, reg3] {
                        // Registers 0 and 1 cannot be closed over
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
            }

            Ok(EvalStatus::Pending)
        })
    }

    // Given ByteCode, execute up to max_instr more instructions
    fn vm_eval_stream<'guard>(
        &self,
        mem: &'guard MutatorView,
        code: ScopedPtr<'guard, ByteCode>,
        max_instr: ArraySize,
    ) -> Result<EvalStatus<'guard>, RuntimeError> {
        let instr = self.instr.get(mem);
        // TODO this is broken logic, this function shouldn't switch back to this code object every
        // time it is called
        instr.switch_frame(code, 0);

        for _ in 0..max_instr {
            match self.eval_next_instr(mem) {
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
    ) -> Result<TaggedScopedPtr<'guard>, RuntimeError> {
        let mut status = EvalStatus::Pending;

        let frames = self.frames.get(mem);
        frames.push(mem, CallFrame::new_main(function))?;

        let code = function.code(mem);

        while status == EvalStatus::Pending {
            status = self.vm_eval_stream(mem, code, 1024)?;
            match status {
                EvalStatus::Return(value) => return Ok(value),
                _ => (),
            }
        }

        Err(err_eval("Unexpected end of evaluation"))
    }
}
