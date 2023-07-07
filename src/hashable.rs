use std::hash::Hasher;

use crate::safe_ptr::MutatorScope;

pub trait Hashable {
    fn hash<'guard, H: Hasher>(&self, _guard: &'guard dyn MutatorScope, hasher: &mut H);
}
