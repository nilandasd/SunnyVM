use std::collections::HashMap;

use crate::memory::Memory;
use crate::error::RuntimeError;
use crate::memory::{MutatorView, Mutator};
use crate::generator::{Generator, MetaGenerator, Compiler};
use crate::safe_ptr::{CellPtr, ScopedPtr};
use crate::thread::{Thread, EvalStatus};

// Sunny VM ☀️

pub struct SVM {
    memory: Memory,
    thread: Option<CellPtr<Thread>>,
}

impl SVM {
    pub fn new() -> SVM {
        SVM {
            memory: Memory::new(),
            thread: None,
        }
    }

    pub fn compile<T: Compiler>(&mut self, compiler: T) -> Result<(), RuntimeError> {
        let meta_gen = MetaGenerator::<T>::new();
        let thread = self.memory.mutate(&meta_gen, compiler)?;

        self.thread = Some(thread);

        Ok(())
    }

    pub fn execute(&mut self) -> Result<(), RuntimeError> {
        self.memory.mutate(self, ())
    }
}

impl Mutator for SVM {
    type Input = ();
    type Output = ();

    fn run(
        &self,
        view: &MutatorView,
        _input: Self::Input,
    ) -> Result<Self::Output, RuntimeError> {
        let thread = self.thread.as_ref().unwrap().get(view);

        thread.execute(view)?;

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct AddTestCompiler {}

    impl Compiler for AddTestCompiler {
        fn compile<'guard>(self, gen: &mut Generator) -> Result<(), ()> {
            let foo = gen.decl_var("foo".to_string());
            let bar = gen.decl_var("bar".to_string());
            let temp = gen.get_temp();

            // TODO: create a generator error type
            gen.load_num(foo, 1);
            gen.load_num(bar, 1);
            gen.add(temp, bar, foo);
            gen.gen_return(temp);

            Ok(())
        }
    }

    #[test]
    fn test_add() {
        let mut vm = SVM::new();
        let test_compiler = AddTestCompiler{};

        vm.compile(test_compiler).expect("compiles");
        vm.execute().expect("no errors");
    }
}
