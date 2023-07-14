use zapalloc::{
    AllocHeader, AllocObject, AllocRaw, AllocTypeId, ArraySize, Mark, RawPtr, SizeClass,
};

use crate::array::{ArrayU16, ArrayU32, ArrayU8};
use crate::bytecode::{ArrayOpcode, ByteCode, InstructionStream};
use crate::dict::Dict;
use crate::function::{Function, Closure};
use crate::list::List;
use crate::memory::HeapStorage;
use crate::number::NumberObject;
use crate::ptr_ops::{AsNonNull, Tagged};
use crate::tagged_ptr::FatPtr;
use crate::text::Text;
use crate::thread::Thread;
use crate::callframe::CallFrameList;
use crate::upvalue::Upvalue;

#[repr(u16)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TypeList {
    ArrayBackingBytes,
    ArrayOpcode,
    ArrayU8,
    ArrayU16,
    ArrayU32,
    ByteCode,
    CallFrameList,
    Dict,
    Function,
    Closure,
    InstructionStream,
    List,
    NumberObject,
    Text,
    Thread,
    Upvalue,
}

impl AllocTypeId for TypeList {}

pub struct ObjectHeader {
    mark: Mark,
    size_class: SizeClass,
    type_id: TypeList,
    size_bytes: u32,
}

impl ObjectHeader {
    /// Convert the ObjectHeader address to a FatPtr pointing at the object itself.
    // NOTE Any type that is a runtime dynamic type must be added to the below list
    // NOTE Be careful to match the correct TypeList discriminant with it's corresponding FatPtr discriminant
    // NOTE Be careful to untag the pointer before putting it into a `FatPtr`
    pub unsafe fn get_object_fatptr(&self) -> FatPtr {
        let ptr_to_self = self.non_null_ptr();
        let object_addr = HeapStorage::get_object(ptr_to_self);

        match self.type_id {
            TypeList::ArrayU8 => FatPtr::ArrayU8(RawPtr::untag(object_addr.cast::<ArrayU8>())),
            TypeList::ArrayU16 => FatPtr::ArrayU16(RawPtr::untag(object_addr.cast::<ArrayU16>())),
            TypeList::ArrayU32 => FatPtr::ArrayU32(RawPtr::untag(object_addr.cast::<ArrayU32>())),
            TypeList::Dict => FatPtr::Dict(RawPtr::untag(object_addr.cast::<Dict>())),
            TypeList::Function => FatPtr::Function(RawPtr::untag(object_addr.cast::<Function>())),
            TypeList::Closure => FatPtr::Closure(RawPtr::untag(object_addr.cast::<Closure>())),
            TypeList::List => FatPtr::List(RawPtr::untag(object_addr.cast::<List>())),
            TypeList::NumberObject => {
                FatPtr::NumberObject(RawPtr::untag(object_addr.cast::<NumberObject>()))
            }
            TypeList::Text => FatPtr::Text(RawPtr::untag(object_addr.cast::<Text>())),
            TypeList::Upvalue => FatPtr::Upvalue(RawPtr::untag(object_addr.cast::<Upvalue>())),

            // Other types not represented by FatPtr are an error to id here
            _ => panic!("Invalid ObjectHeader type tag {:?}!", self.type_id),
        }
    }
}

impl AsNonNull for ObjectHeader {}

impl AllocHeader for ObjectHeader {
    type TypeId = TypeList;

    fn new<O: AllocObject<Self::TypeId>>(
        size: u32,
        size_class: SizeClass,
        mark: Mark,
    ) -> ObjectHeader {
        ObjectHeader {
            mark,
            size_class,
            type_id: O::TYPE_ID,
            size_bytes: size,
        }
    }

    fn new_array(size: ArraySize, size_class: SizeClass, mark: Mark) -> ObjectHeader {
        ObjectHeader {
            mark,
            size_class,
            type_id: TypeList::ArrayBackingBytes,
            size_bytes: size as u32,
        }
    }

    fn mark(&mut self) {
        self.mark = Mark::Marked;
    }

    fn is_marked(&self) -> bool {
        self.mark == Mark::Marked
    }

    fn size_class(&self) -> SizeClass {
        self.size_class
    }

    fn size(&self) -> u32 {
        self.size_bytes
    }

    fn type_id(&self) -> TypeList {
        self.type_id
    }
}

macro_rules! declare_allocobject {
    ($T:ty, $I:tt) => {
        impl AllocObject<TypeList> for $T {
            const TYPE_ID: TypeList = TypeList::$I;
        }
    };
}

declare_allocobject!(ArrayOpcode, ArrayOpcode);
declare_allocobject!(ArrayU8, ArrayU8);
declare_allocobject!(ArrayU16, ArrayU16);
declare_allocobject!(ArrayU32, ArrayU32);
declare_allocobject!(ByteCode, ByteCode);
declare_allocobject!(CallFrameList, CallFrameList);
declare_allocobject!(Dict, Dict);
declare_allocobject!(Function, Function);
declare_allocobject!(Closure, Closure);
declare_allocobject!(InstructionStream, InstructionStream);
declare_allocobject!(List, List);
declare_allocobject!(NumberObject, NumberObject);
declare_allocobject!(Text, Text);
declare_allocobject!(Thread, Thread);
declare_allocobject!(Upvalue, Upvalue);
