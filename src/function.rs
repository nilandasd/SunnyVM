use itertools::join;
use std::fmt;

use crate::array::ArrayU16;
use crate::bytecode::ByteCode;
use crate::container::{Container, SliceableContainer};
use crate::error::RuntimeError;
use crate::list::List;
use crate::memory::MutatorView;
use crate::printer::Print;
use crate::safe_ptr::{CellPtr, MutatorScope, ScopedPtr, TaggedCellPtr, TaggedScopedPtr};
use crate::tagged_ptr::Value;

#[derive(Clone)]
pub struct Function {
    name: TaggedCellPtr,
    // Number of arguments required to activate the function
    arity: u8,
    // Instructions comprising the function code
    code: CellPtr<ByteCode>,
    // Param names are stored for introspection of a function signature
    param_names: CellPtr<List>,
    // List of (CallFrame-index: u8 | Window-index: u8) relative offsets from this function's
    // declaration where nonlocal variables will be found. Needed when creating a closure. May be
    // nil
    nonlocal_refs: TaggedCellPtr,

    overflow_capacity: u16,
}

impl Function {
    pub fn new_default<'guard>(mem: &'guard MutatorView) -> Result<Function, RuntimeError> {
        Ok(Function {
            name: TaggedCellPtr::new_nil(),
            arity: 0,
            code: CellPtr::new_with(ByteCode::alloc(mem)?),
            param_names: CellPtr::new_with(List::alloc(mem)?),
            nonlocal_refs: TaggedCellPtr::new_nil(),
            overflow_capacity: 0,
        })
    }

    /// Allocate a Function object on the heap.
    ///
    /// The nonlocal_refs arg must contain a list of 16 bit values composed of two
    /// 8 bit values: CallFrame relative offset << 8 | Window offset
    /// These values should follow the same order as given in param_names
    pub fn alloc<'guard>(
        mem: &'guard MutatorView,
        name: TaggedScopedPtr<'guard>,
        param_names: ScopedPtr<'guard, List>,
        code: ScopedPtr<'guard, ByteCode>,
        nonlocal_refs: Option<ScopedPtr<'guard, ArrayU16>>,
        overflow_capacity: u16,
    ) -> Result<ScopedPtr<'guard, Function>, RuntimeError> {
        // Store a nil ptr if no nonlocal references are given
        let nonlocal_refs = if let Some(refs_ptr) = nonlocal_refs {
            TaggedCellPtr::new_with(refs_ptr.as_tagged(mem))
        } else {
            TaggedCellPtr::new_nil()
        };

        mem.alloc(Function {
            name: TaggedCellPtr::new_with(name),
            arity: param_names.length() as u8,
            code: CellPtr::new_with(code),
            param_names: CellPtr::new_with(param_names),
            nonlocal_refs,
            overflow_capacity,
        })
    }

    pub fn default_alloc<'guard>(
        mem: &'guard MutatorView,
    ) -> Result<ScopedPtr<'guard, Function>, RuntimeError> {
        mem.alloc(Function::new_default(mem)?)
    }

    /// Return the number of arguments the Function can take
    pub fn arity(&self) -> u8 {
        self.arity
    }

    pub fn overflow_capacity(&self) -> u16 {
        self.overflow_capacity
    }

    pub fn set_overflow_capacity(&mut self, x: u16) {
        self.overflow_capacity = x;
    }

    /// Return the names of the parameters that the Function takes
    pub fn param_names<'guard>(&self, guard: &'guard dyn MutatorScope) -> ScopedPtr<'guard, List> {
        self.param_names.get(guard)
    }

    /// Return the ByteCode object associated with the Function
    pub fn code<'guard>(&self, guard: &'guard dyn MutatorScope) -> ScopedPtr<'guard, ByteCode> {
        self.code.get(guard)
    }

    /// Return true if the function is a closure - it has nonlocal variable references
    pub fn is_closure<'guard>(&self) -> bool {
        !self.nonlocal_refs.is_nil()
    }

    /// Return a list of nonlocal stack references referenced by the function. It is a panickable
    /// offense to call this when there are no nonlocals referenced by the function. This would
    /// indicate a compiler bug.
    pub fn nonlocals<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
    ) -> ScopedPtr<'guard, ArrayU16> {
        match *self.nonlocal_refs.get(guard) {
            Value::ArrayU16(nonlocals) => nonlocals,
            _ => unreachable!(),
        }
    }
}

impl Print for Function {
    fn print<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        let name = self.name.get(guard);
        let params = self.param_names.get(guard);

        let mut param_string = String::new();
        params.access_slice(guard, |items| {
            param_string = join(items.iter().map(|item| item.get(guard)), " ")
        });

        match *name {
            Value::Text(s) => write!(f, "Function {:?} ({})", s.as_str(guard), param_string),
            _ => write!(f, "Function ({})", param_string),
        }
    }

    fn debug<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        self.print(guard, f)?;
        write!(f, "\nbytecode follows:\n")?;
        self.code(guard).debug(guard, f)
    }
}

#[derive(Clone)]
pub struct Closure {
    // Closure environment - must be either nil or a List of Upvalues
    env: TaggedCellPtr,
    // Function that will be activated when all arguments are applied
    func: CellPtr<Function>,
}

impl Closure {
    /// Allocate a Partial application of a Function on the heap with the given set of arguments
    pub fn alloc<'guard>(
        mem: &'guard MutatorView,
        function: ScopedPtr<'guard, Function>,
        env: Option<ScopedPtr<'guard, List>>,
    ) -> Result<ScopedPtr<'guard, Closure>, RuntimeError> {
        // Store a nil ptr if no closure env is given
        let env = if let Some(env_ptr) = env {
            TaggedCellPtr::new_with(env_ptr.as_tagged(mem))
        } else {
            TaggedCellPtr::new_nil()
        };

        mem.alloc(Closure {
            env,
            func: CellPtr::new_with(function),
        })
    }

    /// Return the closure environment. This will be nil if the Partial does not close over any
    /// variables.
    pub fn closure_env(&self) -> TaggedCellPtr {
        self.env.clone()
    }

    /// Return the Function object that the Partial will call
    pub fn function<'guard>(&self, guard: &'guard dyn MutatorScope) -> ScopedPtr<'guard, Function> {
        self.func.get(guard)
    }
}

impl Print for Closure {
    /// Prints a string representation of the Partial object
    fn print<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        let function = self.func.get(guard);

        function.print(guard, f)
    }

    /// Prints the associated function's disassembled bytecode
    fn debug<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        self.print(guard, f)?;
        write!(f, "\nbytecode follows:\n")?;
        self.func.get(guard).code(guard).debug(guard, f)
    }
}
