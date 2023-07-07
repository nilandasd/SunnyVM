use std::fmt;
//use std::io;

use crate::safe_ptr::MutatorScope;
use crate::tagged_ptr::Value;

pub trait Print {
    fn print<'guard>(
        &self,
        _guard: &'guard dyn MutatorScope,
        f: &mut fmt::Formatter,
    ) -> fmt::Result;

    fn debug<'guard>(
        &self,
        _guard: &'guard dyn MutatorScope,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        self.print(_guard, f)
    }

    //fn repr<'guard, F: fmt::Write>(&self, _guard: &'guard dyn MutatorScope, f: &mut F) -> fmt::Result;

    //fn output<'guard, F: io::Write>(
    //    &self,
    //    _guard: &'guard dyn MutatorScope,
    //    f: &mut F,
    //) -> io::Result<()>;
}

pub fn print(value: Value) -> String {
    format!("{}", value)
}

pub fn debug(value: Value) -> String {
    format!("{:?}", value)
}
