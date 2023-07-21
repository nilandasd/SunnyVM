use std::collections::HashMap;
use std::marker::PhantomData;
use std::cell::Cell;

use zapalloc::ArraySize;

use crate::tagged_ptr::TaggedPtr;
use crate::safe_ptr::{CellPtr, TaggedScopedPtr};
use crate::bytecode::{ByteCode, LiteralId, JumpOffset};
use crate::bytecode::Register;
use crate::bytecode::Opcode;
use crate::error::{RuntimeError, GeneratorError};
use crate::memory::{Mutator, MutatorView};
use crate::thread::Thread;
use crate::list::List;
use crate::function::Function;


// BYTECODE TODO:
// isnil
// isatom
// isidentical
// je
// jne
// jz
// loadglobal
// storeglobal
// call
// makeclosure
// copyregister
// getupvalue
// closeupvalue

pub trait Compiler {
    fn compile<'guard>(self, generator: &mut Generator) -> Result<(), ()>;
}

pub type Offset = u32;

pub struct GeneratorGenerator<T> {
    _compiler_type: PhantomData<T>,
}

impl<T> GeneratorGenerator<T> {
    pub fn new() -> GeneratorGenerator<T> {
        GeneratorGenerator {
            _compiler_type: PhantomData,
        }
    }
}

impl<T: Compiler> Mutator for GeneratorGenerator<T> {
    type Input = T;
    type Output = ();

    fn run(
        &self,
        view: &MutatorView,
        compiler: Self::Input,
    ) -> Result<Self::Output, RuntimeError> {
        let globals = HashMap::<String, Register>::new();
        let mut func_stack = vec![];
        let thread = CellPtr::from(Thread::alloc(view)?);
        let args = CellPtr::from(List::alloc(view)?);
        let main_func = CellPtr::from(Function::default_alloc(view)?);
        let func_gen = FunctionGenerator::new(main_func);

        func_stack.push(func_gen);

        let mut generator = Generator::new(globals, func_stack, thread, args, view);

        compiler.compile(&mut generator);

        Ok(())
    }
}

pub struct Generator<'guard> {
    globals: HashMap<String, Register>,
    func_stack: Vec<FunctionGenerator>,
    thread: CellPtr<Thread>,
    args: CellPtr<List>,
    view: &'guard MutatorView<'guard>,
}

impl<'guard> Generator<'guard> {
    fn new(
        globals: HashMap<String, Register>,
        func_stack: Vec<FunctionGenerator>,
        thread: CellPtr<Thread>,
        args: CellPtr<List>,
        view: &'guard MutatorView<'guard>,
    ) -> Generator<'guard> {
        Generator { globals, func_stack, thread, args, view }
    }

    // this could cause a global alloc or a local alloc
    pub fn decl_sym(&self, sym: String) -> Register {
        todo!()
    }

    pub fn load_num(&self, dest: Register, num: isize) -> Result<Offset, GeneratorError> {
        let code = if (num as i16) < i16::MIN || i16::MAX < (num as i16) {
            let literal_id = self.lit_num(num)?;

            Opcode::LoadLiteral { dest, literal_id }
        } else {
            Opcode::LoadInteger { dest, integer: num as i16}
        };

        self.push_code(code)
    }

    pub fn load_nil(&self, dest: Register) -> Result<Offset, GeneratorError> {
        let code = Opcode::LoadNil { dest };

        self.push_code(code)
    }

    pub fn jump(&self, offset: JumpOffset) -> Result<Offset, GeneratorError> {
        let code = Opcode::Jump { offset };

        self.push_code(code)
    }

    pub fn noop(&self) -> Result<Offset, GeneratorError> {
        let code = Opcode::NoOp;

        self.push_code(code)
    }

    pub fn gen_return(&self, reg: Register) -> Result<Offset, GeneratorError> {
        let code = Opcode::Return { reg };

        self.push_code(code)
    }

    pub fn add(&self, dest: Register, reg1: Register, reg2: Register) -> Result<Offset, GeneratorError> {
        let code = Opcode::Add { dest, reg1, reg2 };

        self.push_code(code)
    }

    pub fn sub(&self, dest: Register, reg1: Register, reg2: Register) -> Result<Offset, GeneratorError> {
        let code = Opcode::Add { dest, reg1, reg2 };

        self.push_code(code)
    }

    pub fn mul(&self, dest: Register, reg1: Register, reg2: Register) -> Result<Offset, GeneratorError> {
        let code = Opcode::Multiply { dest, reg1, reg2 };

        self.push_code(code)
    }

    pub fn div(&self, dest: Register, num: Register, denom: Register) -> Result<Offset, GeneratorError> {
        let code = Opcode::DivideInteger { dest, num, denom };

        self.push_code(code)
    }

    fn lit_num(&self, num: isize) -> Result<LiteralId, GeneratorError> {
        // create a number object from an isize
        // load the number object into the current bytecode
        // return the literal id

        todo!()
    }

    fn push_code(&self, code: Opcode) -> Result<Offset, GeneratorError> {
        let func = self.top_func();

        Ok(func.push_code(code, self.view)?)
    }

    fn top_func(&self) -> &FunctionGenerator {
        self.func_stack.last().unwrap()
    }
}

enum Binding {
    Local(Register),
    NonLocal(NonLocal),
}

struct Variable {
    register: Register,
    closed_over: Cell<bool>,
}

struct NonLocal {
    upvalue_id: u8,
    frame_offset: u8,
    frame_register: u8,
}

struct FunctionGenerator {
    next_reg: u8,
    vars: HashMap<String, Variable>,
    nonlocals: HashMap<String, NonLocal>,
    function: CellPtr<Function>,
}

impl FunctionGenerator {
    fn new(function: CellPtr<Function>) -> FunctionGenerator {
        FunctionGenerator {
            next_reg: 0,
            vars: HashMap::new(),
            nonlocals: HashMap::new(),
            function,
        }
    }

    fn push_code<'guard>(&self, code: Opcode, view: &'guard MutatorView) -> Result<Offset, RuntimeError> {
        let bytecode = self.function.get(view).code(view);

        bytecode.push(view, code)?;

        Ok(bytecode.last_instruction())
    }
}
