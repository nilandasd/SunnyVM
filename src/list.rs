/// List is an Array type that can contain any other object
use crate::array::Array;
use crate::safe_ptr::TaggedCellPtr;

/// A List can contain a mixed sequence of any type of value
pub type List = Array<TaggedCellPtr>;
