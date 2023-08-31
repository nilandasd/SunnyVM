use std::convert::TryFrom;
use std::marker::PhantomData;

use zapalloc::ArraySize;

use crate::bytecode::JumpOffset;
use crate::error::RuntimeError;
use crate::function::Function;
use crate::memory::{Mutator, MutatorView};
use crate::safe_ptr::CellPtr;
use crate::tagged_ptr::Value;
use crate::thread::Thread;
use crate::function_generator::{FunctionGenerator, VarId, VarKind};

macro_rules! scoped_gen {
    ($generator:expr, $method:ident) => {{
        let top_idx = $generator.function_stack.len() - 1;
        $generator.function_stack[top_idx].$method()
    }};

    ($generator:expr, $method:ident, $($arg:expr),*) => {{
        let top_idx = $generator.function_stack.len() - 1;
        $generator.function_stack[top_idx].$method($($arg),*)
    }};
}

macro_rules! gen_to_func_gen3 {
    ($opcode:ident) => {
        impl<'guard> Generator<'guard> {
            pub fn $opcode(
                &mut self,
                op1: VarId,
                op2: VarId,
                op3: VarId
            ) -> Result<ArraySize, RuntimeError> {
                scoped_gen!(self, $opcode, op1, op2, op3)
            }
        }
    };
}

macro_rules! gen_to_func_gen2 {
    ($opcode:ident) => {
        impl<'guard> Generator<'guard> {
            pub fn $opcode(
                &mut self,
                op1: VarId,
                op2: VarId,
            ) -> Result<ArraySize, RuntimeError> {
                scoped_gen!(self, $opcode, op1, op2)
            }
        }
    };
}

macro_rules! gen_to_func_gen1 {
    ($opcode:ident) => {
        impl<'guard> Generator<'guard> {
            pub fn $opcode(
                &mut self,
                op1: VarId,
            ) -> Result<ArraySize, RuntimeError> {
                scoped_gen!(self, $opcode, op1)
            }
        }
    };
}


pub trait Compiler {
    fn compile<'guard>(self, generator: &mut Generator) -> Result<(), RuntimeError>;
}

pub struct MetaGenerator<T> {
    _compiler_type: PhantomData<T>,
}


pub struct Generator<'guard> {
    mem: &'guard MutatorView<'guard>,
    function_stack: Vec<FunctionGenerator<'guard>>,
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

gen_to_func_gen3!(get_list);
gen_to_func_gen3!(set_list);
gen_to_func_gen3!(remove_dict);
gen_to_func_gen3!(set_dict);
gen_to_func_gen3!(get_dict);
gen_to_func_gen3!(add);
gen_to_func_gen3!(sub);
gen_to_func_gen3!(equal);
gen_to_func_gen3!(not_equal);
gen_to_func_gen3!(gt);
gen_to_func_gen3!(gte);
gen_to_func_gen3!(lt);
gen_to_func_gen3!(lte);

gen_to_func_gen2!(push_list);
gen_to_func_gen2!(pop_list);
gen_to_func_gen2!(copy);

gen_to_func_gen1!(new_list);
gen_to_func_gen1!(new_dict);
gen_to_func_gen1!(gen_return);
gen_to_func_gen1!(print);

impl<'guard> Generator<'guard> {
    pub fn new(mem: &'guard MutatorView<'guard>) -> Result<Generator<'guard>, RuntimeError> {
        let function = Function::new_default(mem)?;
        Ok(Generator {
            mem,
            function_stack: vec![FunctionGenerator::new(function, mem)],
        })
    }

    pub fn nop(&mut self) -> Result<ArraySize, RuntimeError> {
        scoped_gen!(self, nop)
    }

    pub fn backpatch(
        &mut self,
        index: ArraySize,
        new_offset: JumpOffset
    ) -> Result<(), RuntimeError> {
        scoped_gen!(self, backpatch, index, new_offset)
    }

    pub fn jump(
        &mut self,
        offset: JumpOffset
    ) -> Result<ArraySize, RuntimeError> {
        scoped_gen!(self, jump, offset)
    }

    pub fn jump_if_true(
        &mut self,
        test: VarId,
        offset: JumpOffset
    ) -> Result<ArraySize, RuntimeError> {
        scoped_gen!(self, jump_if_true, test, offset)
    }

    pub fn jump_if_not_true(
        &mut self,
        test: VarId,
        offset: JumpOffset
    ) -> Result<ArraySize, RuntimeError> {
        scoped_gen!(self, jump_if_not_true, test, offset)
    }

    pub fn call(
        &mut self,
        function: VarId,
        dest: VarId,
        args: Vec<VarId>
    ) -> Result<ArraySize, RuntimeError> {
        scoped_gen!(self, call, function, dest, args)
    }

    pub fn load_num(&mut self, var_id: VarId, num: isize) -> Result<ArraySize, RuntimeError> {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].load_num(var_id, num)
    }

    pub fn get_temp(&mut self) -> VarId {
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].get_temp()
    }

    pub fn load_sym(&mut self, dest: VarId, name: String) -> Result<(), RuntimeError> {
        if let Value::Symbol(sym_id) = self.mem.lookup_sym(&name).value() {
            let top_idx = self.function_stack.len() - 1;
            self.function_stack[top_idx].load_sym(dest, sym_id)?;
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

            for (frame_offset, func_gen) in self.function_stack.iter_mut().enumerate().rev() {
                if func_gen.is_declared(var_id) {
                    if frame_offset == top_idx {
                        // declared as a local
                        return Ok(var_id);
                    }

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

    pub fn push_func(&mut self, args: Vec<String>) -> Result<(), RuntimeError> {
        let mut func_gen = FunctionGenerator::new(
            Function::new_default(self.mem)?,
            self.mem,
        );

        for arg in args {
            if let Value::Symbol(sym_id) = self.mem.lookup_sym(&arg).value() {
                let var_id = VarId::Symbol(sym_id);
                func_gen.decl_var(VarKind::Local, var_id);
                func_gen.bind(var_id)?;
            }
        }

        self.function_stack.push(func_gen);

        Ok(())
    }

    pub fn pop_func(&mut self, var_id: VarId) -> Result<(), RuntimeError> {
        let func_ptr = self.function_stack.pop().unwrap().alloc_func()?;
        let top_idx = self.function_stack.len() - 1;
        self.function_stack[top_idx].load_and_close_func(var_id, func_ptr)?;
        Ok(())
    }
}
