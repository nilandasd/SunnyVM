use std::collections::HashMap;

use crate::memory::Memory;
use crate::error::RuntimeError;
use crate::memory::{MutatorView, Mutator};
use crate::generator::{Generator, MetaGenerator, Compiler};
use crate::safe_ptr::CellPtr;
use crate::thread::Thread;

// Sunny VM ☀️

pub struct SVM {
    memory: Memory,
}

impl SVM {
    pub fn new() -> SVM {
        SVM {
            memory: Memory::new(),
        }
    }

    pub fn compile<T: Compiler>(&mut self, compiler: T) -> Result<CellPtr<Thread>, RuntimeError> {
        let meta_gen = MetaGenerator::<T>::new();
        let thread = self.memory.mutate(&meta_gen, compiler);

        thread
    }

    pub fn execute(&mut self, thread: CellPtr<Thread>) -> Result<(), RuntimeError> {
        self.memory.mutate(self, thread)
    }
}

impl Mutator for SVM {
    type Input = CellPtr<Thread>;
    type Output = ();

    fn run(
        &self,
        view: &MutatorView,
        thread: Self::Input,
    ) -> Result<Self::Output, RuntimeError> {
        // run thread until GC trigger,
        // collect roots (a bunch of CellPtrs
        //  pass roots to the memory GC
        // continue running thread
        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct AddTestCompiler {}

    impl Compiler for AddTestCompiler {
        fn compile<'guard>(self, gen: &mut Generator) -> Result<(), ()> {
            let mut foo = gen.decl_var("foo".to_string());
            gen.load_num(&mut foo, 1);
            let mut bar = gen.decl_var("bar".to_string());
            gen.load_num(bar, 1);
            let mut temp = gen.get_temp();
            gen.add(temp, &mut bar, &mut foo);
            gen.add(temp, &mut bar, &mut foo);
            gen.return_var(temp);
            Ok(())
        }
    }

    #[test]
    fn test_add() {
        let mut vm = SVM::new();
        let test_compiler = AddTestCompiler{};

        let thread = vm.compile(test_compiler).expect("compiles");
        vm.execute(thread).expect("no errors");
    }
}
