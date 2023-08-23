use std::cell::RefCell;
use std::io::{self, Write};

use crate::error::RuntimeError;
use crate::generator::{Compiler, MetaGenerator};
use crate::memory::Memory;
use crate::memory::{Mutator, MutatorView};
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

    fn run(&self, view: &MutatorView, _input: Self::Input) -> Result<Self::Output, RuntimeError> {
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
    use std::fs::{self, File};
    use std::io::Read;
    use crate::bytecode::JUMP_UNKNOWN;

    struct TestCompiler {
        test_case: fn(gen: &mut Generator) -> Result<(), RuntimeError>,
    }

    impl Compiler for TestCompiler {
        fn compile<'guard>(self, gen: &mut Generator) -> Result<(), RuntimeError> {
            (self.test_case)(gen)
        }
    }

    // test name should end in .test so they can be cleaned up
    fn vm_test_helper(
        test_case: fn(&mut Generator) -> Result<(), RuntimeError>,
        file_name: &str,
        expected_output: &str,
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
        fs::remove_file(file_name).unwrap();

        assert!(output_str == expected_output);
    }

    #[test]
    fn test_add() {
        fn case(gen: &mut Generator) -> Result<(), RuntimeError> {
            let foo = gen.decl_var("foo".to_string());
            let bar = gen.decl_var("bar".to_string());
            let baz = gen.decl_var("baz".to_string());

            gen.load_num(foo, 1)?;
            gen.load_num(bar, 1)?;
            gen.load_num(baz, 1)?;
            gen.add(bar, bar, foo)?;
            gen.add(baz, bar, foo)?;
            gen.print(foo)?;
            gen.print(bar)?;
            gen.print(baz)?;
            gen.gen_return(foo)?;

            Ok(())
        }
        vm_test_helper(case, "vm_add.test", "1\n2\n3\n");
    }

    #[test]
    fn test_sub() {
        fn case(gen: &mut Generator) -> Result<(), RuntimeError> {
            let foo = gen.decl_var("foo".to_string());
            let bar = gen.decl_var("bar".to_string());

            gen.load_num(foo, 0)?;
            gen.load_num(bar, 1)?;
            for _ in 0..600 {
                gen.sub(foo, foo, bar)?;
            }

            gen.print(foo)?;
            gen.gen_return(foo)?;

            Ok(())
        }
        vm_test_helper(case, "vm_sub.test", "-600\n");
    }

    #[test]
    fn test_func_call() {
        fn case(gen: &mut Generator) -> Result<(), RuntimeError> {
            let baz = gen.decl_var("baz".to_string());

            gen.push_func(vec![])?;

            let foo = gen.decl_var("foo".to_string());
            let bar = gen.decl_var("bar".to_string());
            let temp = gen.get_temp();

            gen.load_num(foo, 12)?;
            gen.load_num(bar, 83)?;
            gen.add(temp, bar, foo)?;
            gen.gen_return(temp)?;
            gen.pop_func(baz)?;

            let temp = gen.get_temp();

            // do some juggling

            gen.call(baz, temp)?;
            gen.print(temp)?;
            gen.copy(temp, baz)?;
            gen.call(temp, baz)?;
            gen.print(baz)?;
            gen.copy(baz, temp)?;
            gen.call(baz, temp)?;
            gen.print(temp)?;

            gen.gen_return(temp)?;

            Ok(())
        }
        vm_test_helper(case, "vm_call.test", "95\n95\n95\n");
    }

    #[test]
    fn test_nested_func_call() {
        fn case(gen: &mut Generator) -> Result<(), RuntimeError> {
            let baz = gen.decl_var("baz".to_string());

            //FUNC DEF ==
            gen.push_func(vec![])?;
            let foo = gen.decl_var("foo".to_string());
            let bar = gen.decl_var("bar".to_string());
            let temp = gen.get_temp();
            let temp3 = gen.get_temp();
            gen.load_num(foo, 12)?;
            gen.load_num(bar, 83)?;
            gen.add(temp, bar, foo)?;

            // FUNC DEF =============
            gen.push_func(vec![])?;
            let qux = gen.decl_var("qux".to_string());
            let qaz = gen.decl_var("qaz".to_string());
            let temp2 = gen.get_temp();
            gen.load_num(qux, 3)?;
            gen.load_num(qaz, 2)?;
            gen.add(temp2, qux, qaz)?;
            gen.gen_return(temp2)?;
            gen.pop_func(temp3)?;
            // FUNC DEF ==============

            gen.call(temp3, foo)?;
            gen.add(temp, temp, foo)?;
            gen.gen_return(temp)?;

            gen.pop_func(baz)?;

            let temp = gen.get_temp();

            // do some juggling

            gen.call(baz, temp)?;
            gen.print(temp)?;
            gen.copy(temp, baz)?;
            gen.call(temp, baz)?;
            gen.print(baz)?;
            gen.copy(baz, temp)?;
            gen.call(baz, temp)?;
            gen.print(temp)?;

            gen.gen_return(temp)?;

            Ok(())
        }
        vm_test_helper(case, "vm_nested_call.test", "100\n100\n100\n");
    }

    #[test]
    fn test_array() {
        fn case(gen: &mut Generator) -> Result<(), RuntimeError> {
            let foo = gen.decl_var("foo".to_string());
            let bar = gen.decl_var("bar".to_string());
            let t1 = gen.get_temp();
            let t2 = gen.get_temp();
            let t3 = gen.get_temp();

            gen.load_num(bar, 500)?;
            gen.new_list(foo)?;
            gen.push_list(foo, bar)?;
            gen.push_list(foo, bar)?;
            gen.load_num(t3, 0)?;
            gen.get_list(foo, t3, t1)?;
            gen.print(t1)?;
            gen.load_num(t3, 1)?;
            gen.get_list(foo, t3, t1)?;
            gen.print(t1)?;
            gen.pop_list(foo, t1)?;
            gen.pop_list(foo, t2)?;
            gen.add(t1, t1, t2)?;
            gen.print(t1)?;
            gen.load_num(t1, -500)?;
            gen.push_list(foo, bar)?;
            gen.push_list(foo, t1)?;
            gen.load_num(t3, 1)?;
            gen.set_list(foo, t3, t1)?;
            gen.pop_list(foo, t1)?;
            gen.pop_list(foo, t2)?;
            gen.add(t1, t1, t2)?;
            gen.print(t1)?;
            gen.gen_return(t1)?;

            Ok(())
        }
        vm_test_helper(case, "vm_array.test", "500\n500\n1000\n0\n");
    }

    #[test]
    fn test_jump() {
        fn case(gen: &mut Generator) -> Result<(), RuntimeError> {
            let foo = gen.decl_var("foo".to_string());
            gen.load_num(foo, 1)?;
            let i0 = gen.jump(JUMP_UNKNOWN)?;
            let _i1 = gen.jump(-9999)?;
            let i2 = gen.load_num(foo, 777)?;
            let _i3 = gen.print(foo)?;

            gen.backpatch(i0, (i2 - i0) as i16)?;
            gen.gen_return(foo)?;

            Ok(())
        }
        vm_test_helper(case, "vm_jump.test", "1\n");
    }

    #[test]
    fn test_dict() {
        fn case(gen: &mut Generator) -> Result<(), RuntimeError> {
            let foo = gen.decl_var("foo".to_string());
            let bar = gen.decl_var("bar".to_string());
            let temp = gen.get_temp();
            gen.load_num(temp, 555)?;
            gen.load_sym(bar, "bar".to_string())?;
            gen.new_dict(foo)?;
            gen.set_dict(foo, bar, temp)?;
            gen.get_dict(foo, bar, foo)?;
            gen.print(foo)?;
            gen.gen_return(foo)?;
            Ok(())
        }
        vm_test_helper(case, "vm_dict.test", "555\n");
    }
}
