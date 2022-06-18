use std::vec::Vec;
#[derive(Debug)]
pub enum LxValue { 
  ValBool(bool),
  ValNil,
  ValNumber(f64),
  ValString(String),
  ValObjPtr(u64)
}
#[derive(Debug)]
pub enum LxObject {
  ObjFunction(i64, Chunk, String)
}
#[derive(Debug)]
pub struct Chunk {
  pub opcodes: Vec<OpCode>,
  pub constants: Vec<LxObject>
}
#[derive(Debug)]
pub enum OpCode { 
  OpConstant(LxValue),
  OpNil,
  OpTrue,
  OpFalse,
  OpPop,
  OpGetLocal(String),
  OpSetLocal(String),
  OpGetGlobal(String),
  OpDefineGlobal(String), 
  OpSetGlobal(String),
  OpGetUpValue, // unimplemented
  OpSetUpValue, // ...
  OpGetProperty,
  OpSetProperty, // ...
  OpGetSuper,  // unimplemented
  OpEqual,
  OpGreater,
  OpLess, 
  OpAdd,
  OpSubtract,
  OpMultiply,
  OpDivide,
  OpNot,
  OpNegate,
  OpPrint, 
  OpJump(i16),
  OpJumpIfFalse(u16), 
  OpCall, 
  OpInvoke,
  OpSuperInvoke,
  OpClosure,
  OpCloseUpValue,
  OpReturn,
  OpClass,
  OpInherit,
  OpMethod
}


