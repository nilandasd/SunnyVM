use std::collections::HashMap;
use std::marker::PhantomData;
use std::cell::Cell;

use zapalloc::ArraySize;

use crate::tagged_ptr::TaggedPtr;
use crate::safe_ptr::{CellPtr, TaggedScopedPtr};
use crate::bytecode::{ByteCode, LiteralId};
use crate::bytecode::Register;
use crate::bytecode::Opcode;
use crate::error::RuntimeError;
use crate::memory::{Mutator, MutatorView};
use crate::thread::Thread;
use crate::list::List;
use crate::function::Function;


// BYTECODE TODO:
// loadliteral
// isnil
// isatom
// isidentical
// jump
// jumpiftrue
// jumpiffalse
// loadnil
// loadglobal
// storeglobal
// call
// makeclosure
// loadinteger
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

    pub fn decl_sym(&self, sym: String) -> Register {
        todo!()
    }

    // 2 cases sym is a global or an upvalue
    // case: global
    //  generate a load global instruction
    // case: upvalue  
    //   add a upvalue to the current function
    //   and create a 
    pub fn close_sym(&self, sym: String) -> Register {
        todo!()
    }

    pub fn noop(&self) -> Offset {
        let code = Opcode::NoOp;

        self.push_code(code)
    }

    pub fn gen_return(&self, reg: Register) -> Offset {
        let code = Opcode::Return { reg };

        self.push_code(code)
    }

    pub fn add(&self, dest: Register, reg1: Register, reg2: Register) -> Offset {
        let code = Opcode::Add { dest, reg1, reg2 };

        self.push_code(code)
    }

    pub fn sub(&self, dest: Register, reg1: Register, reg2: Register) -> Offset {
        let code = Opcode::Add { dest, reg1, reg2 };

        self.push_code(code)
    }

    pub fn mul(&self, dest: Register, reg1: Register, reg2: Register) -> Offset {
        let code = Opcode::Multiply { dest, reg1, reg2 };

        self.push_code(code)
    }

    pub fn div(&self, dest: Register, num: Register, denom: Register) -> Offset {
        let code = Opcode::DivideInteger { dest, num, denom };

        self.push_code(code)
    }

    fn push_code(&self, code: Opcode) -> Offset {
        let func = self.top_func();

        func.push_code(code, self.view)
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

struct Scope {
    vars: HashMap<String, Variable>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            vars: HashMap::new()
        }
    }
}

struct FunctionGenerator {
    next_reg: u8,
    scopes: Vec<Scope>,
    nonlocals: HashMap<String, NonLocal>,
    function: CellPtr<Function>,
}

impl FunctionGenerator {
    fn new(function: CellPtr<Function>) -> FunctionGenerator {
        FunctionGenerator {
            next_reg: 0,
            scopes: vec![Scope::new()],
            nonlocals: HashMap::new(),
            function,
        }
    }

    fn push_code<'guard>(&self, code: Opcode, view: &'guard MutatorView) -> Offset {
        let bytecode = self.function.get(view).code(view);

        bytecode.push(view, code);

        bytecode.last_instruction()
    }
}
