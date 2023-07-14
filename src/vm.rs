use crate::symbol_map::SymbolMap;
use crate::memory::Memory;
use crate::thread::Thread;
use crate::generator::Generator;
use zapalloc::{AllocRaw, RawPtr};
use crate::symbol::{Symbol};
use crate::bytecode::Register;

// Sunny VM ☀️

pub struct SVM {
    memory: Memory,
    symbols: SymbolMap,
    //generator: Generator,
}

impl SVM {
    pub fn new() -> SVM {
        SVM {
            symbols: SymbolMap::new(),
            memory: Memory::new(),
            //generator: Generator::new(),
        }
    }


    // Returns the register for the given symbol in the current scope
    // either by finding the assigned register or assigning a new one.
    /*
    pub fn sym_reg(&self, name: &str) {
        let sym_ptr = self.symbols.lookup(name);

        self.generator.set_sym(&self.symbols, name)
    }

    pub fn num(&self, n: isize) {
        self.generator.create_lit(&self.symbols, name)
    }
    */
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::error::RuntimeError;
    use crate::memory::MutatorView;
    use crate::memory::{Memory, Mutator};
    use crate::list::List;
    use crate::safe_ptr::{ScopedPtr, TaggedScopedPtr};

    #[test]
    fn test_store_load_global() {
        let mut vm = SVM::new();

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
