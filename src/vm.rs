use std::collections::HashMap;

use crate::memory::Memory;
use crate::error::RuntimeError;
use crate::memory::{MutatorView, Mutator};
use crate::generator::{Generator, GeneratorGenerator, Compiler};
use crate::safe_ptr::{CellPtr};
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

    pub fn compile<T: Compiler>(&mut self, compiler: T) -> Result<(), RuntimeError> {
        let gen_gen = GeneratorGenerator::<T>::new();

        self.memory.mutate(&gen_gen, compiler)
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
        // tell the thread in the generator to run the global code
        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct TestCompiler {}

    impl Compiler for TestCompiler {
        fn compile<'guard>(self, generator: &mut Generator) -> Result<(), ()> {
            todo!()
        }
    }

    #[test]
    fn test_add() {
        let mut vm = SVM::new();
        let test_compiler = TestCompiler{};

        vm.compile(test_compiler);
        // vm.run();
    }
}
/*
 * vm.begin_func("outer", arity, params);
 * vm.gen_load_num("foo", 69);
 * vm.gen_load_sym("bar", "foo");
 * vm.begin_func("inner", arity, params);
 * vm.gen_load_num("buzz", 20);
 * vm.gen_load_num("bazz", 30);
 * vm.gen_add(vm.return_reg(), vm.sym("buzz"), vm.sym("bazz"))
 * vm.gen_add(vm.return_reg(), vm.return_reg(), vim.num(69))
 * vm.gen_return();
 * vm.exit_func();
 * vm.gen_add(vm.return_reg(), vm.gen_call("inner", arg_count), vm.sym("bar"))
 * vm.exit_func();
 * vm.gen_call("outer", arg_count);
 *
 * fn outer() {
 *      foo = 69
 *      bar = foo
 *
 *      fn inner() {
 *          buzz = 20
 *          bazz = 30
 *
 *          return buzz + bazz + 69
 *      }
 *
 *      return inner() + bar
 * }
*/

/*
 * vm.begin_func("control_test", 1, ["n"]);
 * vm.gen_load("foo", vm.num(0));
 * vm.gen_lt(vm.temp(), vm.sym("foo"), vm.sym("n"));
 * let offset = vm.gen_noop();
 * vm.gen_add(vm.sym("foo"), vm.sym("foo"), vim.num(1));
 * vm.gen_jmp(offset);
 * vm.back_patch(offset, vm.gen_jnz(vm.temp(), offset))
 * vm.gen_return();
 * vm.end_func();
 * vm.gen_call("control_test", None, arg_count);
 * vm.gen_return();
 *
 * fn control_test(n) {
 *      foo = 0
 *
 *      while(foo < n) {
 *          foo = foo + 1
 *      }
 *
 *      return
 * }
 *
 * control_test(10);
 */

/*
 * vm.begin_func("control_test", 1, ["n"]);
 * let dict = vm.create_dict(capacity);
 * let list = vm.create_list(capacity);
 * vm.dict_insert(obj, "a", vm.sym("n"))
 * vm.dict_insert(obj, "b", vm.num(1))
 * vm.dict_insert(obj, "c", vm.num(2))
 * vm.gen_load("foo", obj);
 * vm.gen_lt(vm.temp(), "foo", "n");
 * let offset = vm.gen_jnz(vm.temp(), 0);
 * vm.gen_add(vm.sym("foo"), vm.sym("foo"), vim.num(1));
 * vm.gen_jmp(offset);
 * vm.back_patch_jmp(offset, vm.offset())
 * vm.gen_return();
 * vm.end_func();
 * vm.gen_call("control_test", vm.trash_reg(), arg_count);
 * vm.gen_return();
 *
 * fn control_test(n) {
 *      foo = { a: n, b: 1, c: 3}
 *      n = n + foo.a + foo.b + foo.c
 *
 *      while(foo < n) {
 *          foo = foo + 1
 *      }
 *
 *      return
 * }
 *
 * control_test(10);
 */
