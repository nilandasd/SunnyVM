use std::io::{self, Cursor, Write, Read};
use std::fs::{self, File};
use std::cell::RefCell;

use crate::memory::Memory;
use crate::error::RuntimeError;
use crate::memory::{MutatorView, Mutator};
use crate::generator::{MetaGenerator, Compiler};
use crate::safe_ptr::CellPtr;
use crate::thread::Thread;

// Sunny VM

pub struct SVM {
    memory: Memory,
    thread: Option<CellPtr<Thread>>,
    output_stream: RefCell<Box<dyn Write>>,
}

impl SVM {
    pub fn new() -> SVM {
        SVM {
            memory: Memory::new(),
            thread: None,
            output_stream: RefCell::new(Box::new(io::stdout())),
        }
    }

    pub fn set_output_stream(&mut self, stream: Box<dyn Write>) {
        self.output_stream = RefCell::new(stream);
    }

    pub fn compile<T: Compiler>(&mut self, compiler: T) -> Result<(), RuntimeError> {
        let meta_gen = MetaGenerator::<T>::new();
        let thread = self.memory.mutate(&meta_gen, compiler)?;
        self.thread = Some(thread);
        Ok(())
    }

    pub fn execute(&self) -> Result<(), RuntimeError> {
        self.memory.mutate(self, ())
    }
}

impl Mutator for SVM {
    type Input = (); // command line args?
    type Output = ();

    fn run(
        &self,
        view: &MutatorView,
        _input: Self::Input,
    ) -> Result<Self::Output, RuntimeError> {
        let thread = self.thread.as_ref().unwrap().get(view);
        let mut stream = self.output_stream.borrow_mut();
        thread.execute(view, &mut stream)?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::generator::Generator;

    struct TestCompiler {
        test_case: fn(gen: &mut Generator) -> Result<(), ()>
    }

    impl Compiler for TestCompiler {
        fn compile<'guard>(self, gen: &mut Generator) -> Result<(), ()> {
            (self.test_case)(gen)
        }
    }

    // test name should end in .test so they can be cleaned up
    fn vm_test_helper(
        test_case: fn(&mut Generator)->Result<(), ()>,
        file_name: &str,
        expected_output: &str
    ) {
        let mut vm = SVM::new();
        let compiler = TestCompiler { test_case };
        let output = Box::new(File::create(file_name).unwrap());

        vm.compile(compiler).expect("compiles");
        vm.set_output_stream(output);
        vm.execute().expect("no errors");

        let mut f = File::open(file_name).unwrap();
        let mut data = vec![];
        f.read_to_end(&mut data).unwrap();
        let output_str = String::from_utf8(data).unwrap();
        fs::remove_file(file_name);

        assert!(output_str == expected_output);
    }

    #[test]
    fn test_add() {
        fn case(gen: &mut Generator) -> Result<(), ()> {
            let foo = gen.decl_var("foo".to_string());
            let bar = gen.decl_var("bar".to_string());
            let baz = gen.decl_var("baz".to_string());
            // TODO: create a generator error type

            gen.load_num(foo, 1);
            gen.load_num(bar, 1);
            gen.load_num(baz, 1);
            gen.add(bar, bar, foo);
            gen.add(baz, bar, foo);
            gen.print(foo);
            gen.print(bar);
            gen.print(baz);
            gen.gen_return(foo);

            Ok(())
        }
        vm_test_helper(case, "test_add.test", "1\n2\n3\n");
    }

    #[test]
    fn test_sub() {
        fn case(gen: &mut Generator) -> Result<(), ()> {
            let foo = gen.decl_var("foo".to_string());
            let bar = gen.decl_var("bar".to_string());
            // TODO: create a generator error type

            gen.load_num(foo, 0);
            gen.load_num(bar, 1);
            for _ in 0..1337 {
                gen.sub(foo, foo, bar);
            }

            gen.print(foo);

            Ok(())
        }
        vm_test_helper(case, "test_sub.test", "-1337\n");
    }

    #[test]
    fn test_func_call() {
        fn case(gen: &mut Generator) -> Result<(), ()> {

            Ok(())
        }
        //vm_test_helper(case, "test_func_call.test", "1\n2\n3\n");
    }
}
