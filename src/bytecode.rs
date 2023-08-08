use itertools::join;
use std::cell::Cell;
use std::fmt;

use crate::array::{Array, ArraySize};
use crate::container::{
    Container, IndexedContainer, SliceableContainer, StackAnyContainer, StackContainer,
};
use crate::error::{err_eval, RuntimeError};
use crate::list::List;
use crate::memory::MutatorView;
use crate::printer::Print;
use crate::safe_ptr::{CellPtr, MutatorScope, ScopedPtr, TaggedScopedPtr};
use crate::tagged_ptr::TaggedPtr;

pub type Register = u8;
pub type LiteralInteger = i16;
pub type LiteralSymbol = u16;
pub type LiteralId = u16;
pub type OverflowId = u16;
pub type UpvalueId = u8;
pub type JumpOffset = i16;
pub const JUMP_UNKNOWN: i16 = 0x7fff;
pub type NumArgs = u8;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Opcode {
    NoOp,
    Return {
        reg: Register,
    },
    LoadLiteral {
        dest: Register,
        literal_id: LiteralId,
    },
    IsNil {
        dest: Register,
        test: Register,
    },
    IsAtom {
        dest: Register,
        test: Register,
    },
    IsIdentical {
        dest: Register,
        test1: Register,
        test2: Register,
    },
    Jump {
        offset: JumpOffset,
    },
    JumpIfTrue {
        test: Register,
        offset: JumpOffset,
    },
    JumpIfNotTrue {
        test: Register,
        offset: JumpOffset,
    },
    LoadNil {
        dest: Register,
    },
    LoadGlobal {
        dest: Register,
        name: Register,
    },
    StoreGlobal {
        src: Register,
        name: Register,
    },
    Call {
        function: Register,
        dest: Register,
        arg_count: NumArgs,
    },
    MakeClosure {
        dest: Register,
        function: Register,
    },
    LoadInteger {
        dest: Register,
        integer: LiteralInteger,
    },
    LoadSymbol {
        dest: Register,
        symbol: LiteralSymbol,
    },
    CopyRegister {
        dest: Register,
        src: Register,
    },
    Add {
        dest: Register,
        reg1: Register,
        reg2: Register,
    },
    Subtract {
        dest: Register,
        left: Register,
        right: Register,
    },
    Multiply {
        dest: Register,
        reg1: Register,
        reg2: Register,
    },
    DivideInteger {
        dest: Register,
        num: Register,
        denom: Register,
    },
    LoadUpvalue {
        dest: Register,
        src: UpvalueId,
    },
    StoreUpvalue {
        dest: UpvalueId,
        src: Register,
    },
    CloseUpvalues {
        reg1: Register,
        reg2: Register,
        reg3: Register,
    },
    LoadOverflow {
        dest: Register,
        overflow_id: OverflowId,
    },
    StoreOverflow {
        overflow_id: OverflowId,
        src: Register,
    }
}

pub type ArrayOpcode = Array<Opcode>;
pub type Literals = List;

#[derive(Clone)]
pub struct ByteCode {
    code: ArrayOpcode,
    literals: Literals,
}

impl ByteCode {
    pub fn alloc<'guard>(
        mem: &'guard MutatorView,
    ) -> Result<ScopedPtr<'guard, ByteCode>, RuntimeError> {
        mem.alloc(ByteCode {
            code: ArrayOpcode::new(),
            literals: Literals::new(),
        })
    }

    pub fn push<'guard>(&self, mem: &'guard MutatorView, op: Opcode) -> Result<u32, RuntimeError> {
        self.code.push(mem, op)?;
        Ok(self.code.length() - 1)
    }

    pub fn update_jump_offset<'guard>(
        &self,
        mem: &'guard MutatorView,
        instruction: ArraySize,
        offset: JumpOffset,
    ) -> Result<(), RuntimeError> {
        let code = self.code.get(mem, instruction)?;
        let new_code = match code {
            Opcode::Jump { offset: _ } => Opcode::Jump { offset },
            Opcode::JumpIfTrue { test, offset: _ } => Opcode::JumpIfTrue { test, offset },
            Opcode::JumpIfNotTrue { test, offset: _ } => Opcode::JumpIfNotTrue { test, offset },
            _ => {
                return Err(err_eval(
                    "Cannot modify jump offset for non-jump instruction",
                ))
            }
        };
        self.code.set(mem, instruction, new_code)?;
        Ok(())
    }

    // Append a literal-load operation to the back of the sequence
    pub fn push_loadlit<'guard>(
        &self,
        mem: &'guard MutatorView,
        dest: Register,
        literal_id: LiteralId,
    ) -> Result<(), RuntimeError> {
        // TODO clone anything mutable
        self.code.push(
            mem,
            Opcode::LoadLiteral { dest, literal_id }
        )
    }

    pub fn push_lit<'guard>(
        &self,
        mem: &'guard MutatorView,
        literal: TaggedScopedPtr<'guard>,
    ) -> Result<LiteralId, RuntimeError> {
        let lit_id = self.literals.length() as u16;
        StackAnyContainer::push(&self.literals, mem, literal)?;
        Ok(lit_id)
    }

    pub fn last_instruction(&self) -> ArraySize {
        self.code.length() - 1
    }

    pub fn next_instruction(&self) -> ArraySize {
        self.code.length()
    }
}

impl Print for ByteCode {
    fn print<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        let mut instr_str = String::new();

        self.code.access_slice(guard, |code| {
            instr_str = join(code.iter().map(|opcode| format!("{:?}", opcode)), "\n")
        });

        write!(f, "{}", instr_str)
    }
}

pub struct InstructionStream {
    instructions: CellPtr<ByteCode>,
    ip: Cell<ArraySize>,
}

impl InstructionStream {
    // Create an InstructionStream instance with the given ByteCode instance that will be iterated over
    pub fn alloc<'guard>(
        mem: &'guard MutatorView,
        code: ScopedPtr<'_, ByteCode>,
    ) -> Result<ScopedPtr<'guard, InstructionStream>, RuntimeError> {
        mem.alloc(InstructionStream {
            instructions: CellPtr::new_with(code),
            ip: Cell::new(0),
        })
    }

    pub fn switch_frame(&self, code: ScopedPtr<'_, ByteCode>, ip: ArraySize) {
        self.instructions.set(code);
        self.ip.set(ip);
    }

    // Retrieve the next instruction and return it, incrementing the instruction pointer
    // TODO: https://github.com/rust-hosted-langs/book/issues/39
    pub fn get_next_opcode<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
    ) -> Result<Opcode, RuntimeError> {
        let instr = self
            .instructions
            .get(guard)
            .code
            .get(guard, self.ip.get())?;
        self.ip.set(self.ip.get() + 1);
        Ok(instr)
    }

    pub fn get_literal<'guard>(
        &self,
        guard: &'guard dyn MutatorScope,
        lit_id: LiteralId,
    ) -> Result<TaggedPtr, RuntimeError> {
        Ok(IndexedContainer::get(
            &self.instructions.get(guard).literals,
            guard,
            lit_id as ArraySize,
        )?
        .get_ptr())
    }

    pub fn get_next_ip(&self) -> ArraySize {
        self.ip.get()
    }

    pub fn jump(&self, offset: JumpOffset) {
        let mut ip = self.ip.get() as i32;
        ip += offset as i32;
        self.ip.set(ip as ArraySize);
    }
}

#[cfg(test)]
mod test {
    use super::Opcode;
    use std::mem::size_of;

    #[test]
    fn test_opcode_is_32_bits() {
        assert!(size_of::<Opcode>() == 4);
    }
}
