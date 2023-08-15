use std::collections::HashMap;

use crate::memory::Memory;
use crate::error::RuntimeError;
use crate::memory::{MutatorView, Mutator};
use crate::generator::{Generator, MetaGenerator, Compiler};
use crate::safe_ptr::CellPtr;
use crate::thread::{Thread, EvalStatus};

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
        loop {
            let eval_status = thread.get(view).eval_stream(view, 1024)?;

            match eval_status {
                EvalStatus::Pending => {},
                EvalStatus::Return(_) => {break;},
            }
        }

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

        let thread = vm.compile(test_compiler).expect("compiles");
        vm.execute(thread).expect("no errors");
    }
}
