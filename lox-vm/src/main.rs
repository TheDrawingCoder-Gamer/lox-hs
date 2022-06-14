use crate::lox::vm::{OpCode, LxValue};
use std::env;
use std::fs::File;
use std::io::Result;
use std::io::{Read, BufRead, BufReader};
use std::io::{Error, ErrorKind};
mod lox {
  pub mod vm;
}
fn main() -> std::io::Result<()> {
  let args:Vec<String> = env::args().collect();
  match args.get(1) { 
    Some(cmd) => { 
      match cmd.as_str() {
        "decompile" => {
          // produce a list of chunks, then print them (?)
          match args.get(2) { 
            Some(path) => {
              let mut file = File::open(path)?;
              let mut reader = BufReader::new(file);
              let opcodes = parseOpCodes(&mut reader)?;
              println!("{:#?}", opcodes);
              return Ok(());
            } 
            None => {
              println!("decompile needs a file to decompile");
              return Ok(());
            }
          }
        }
        _ => {
          println!("unknown command");
          return Ok(());
        }
      }
    }
    None => {
      println!("command not given");
      return Ok(());
    }
  }
}
fn parseOpCodes<R:BufRead>(source: &mut R) -> Result<Vec<OpCode>> {
  let mut vec : Vec<OpCode> = Vec::new();
  loop {
    let atEof = !hasDataLeft(source)?;
    if atEof { break; } 
    let opcode = parseOpCode(source)?; 
    vec.push(opcode);
  } 
  return Ok(vec);
}
fn parseOpCode<R:BufRead>(source: &mut R) -> Result<OpCode> {
  // because I'm already borrowing it I don't need to reborrow it???
  let opcode = getOneByte(source)?;
  match opcode { 
    0 => {
      let constKind = getOneByte(source)?;
      match constKind {
        0 => {
          let boolByte = getOneByte(source)?;
          let daBool :bool = match boolByte { 
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(Error::new(ErrorKind::InvalidData, "Bool must be 1 or 0"))
          }?;
          return Ok(OpCode::OpConstant(LxValue::ValBool(daBool)));
        }
        1 => Ok(OpCode::OpConstant(LxValue::ValNil)),
        2 => Ok(OpCode::OpConstant(LxValue::ValNumber(readF64(source)?))),
        3 => Ok(OpCode::OpConstant(LxValue::ValString(readStringLen(source)?))), 
        4 => {
          let arity = readI64(source)?;
          let size  = readU64(source)?; // size of chunk
          // take is sane in that Take<BufRead> has the trait BufRead
          let mut handle = source.take(size);
          let opcodes = parseOpCodes(&mut handle)?;
          let name = readStringLen(source)?;
          return (Ok(OpCode::OpConstant(LxValue::ValFunction(arity, opcodes, name))));
        }
        _ => Err(Error::new(ErrorKind::InvalidData, "Invalid constant kind"))
      } 
    }
    1 => Ok(OpCode::OpNil),
    2 => Ok(OpCode::OpTrue),
    3 => Ok(OpCode::OpFalse),
    4 => Ok(OpCode::OpPop), 
    5 => {
      let daVar = readStringLen(source)?; 
      return Ok(OpCode::OpGetLocal(daVar));
    } 
    6 => {
      let daVar = readStringLen(source)?;
      return Ok(OpCode::OpSetLocal(daVar));
    }
    7 => {
      let daVar = readStringLen(source)?;
      return Ok(OpCode::OpGetGlobal(daVar));
    }
    8 => Ok(OpCode::OpDefineGlobal(readStringLen(source)?)),
    9 => Ok(OpCode::OpSetGlobal(readStringLen(source)?)),
    10 => Err(errUnimplemented()),
    11 => Err(errUnimplemented()),
    12 => Err(errUnimplemented()),
    13 => Err(errUnimplemented()),
    14 => Err(errUnimplemented()),
    15 => Ok(OpCode::OpEqual),
    16 => Ok(OpCode::OpGreater),
    17 => Ok(OpCode::OpLess),
    18 => Ok(OpCode::OpAdd),
    19 => Ok(OpCode::OpSubtract),
    20 => Ok(OpCode::OpMultiply),
    21 => Ok(OpCode::OpDivide),
    22 => Ok(OpCode::OpNot),
    23 => Ok(OpCode::OpNegate),
    24 => Ok(OpCode::OpPrint),
    25 => Ok(OpCode::OpJump(readI16(source)?)),
    26 => Ok(OpCode::OpJumpIfFalse(readU16(source)?)),
    27 => Err(errUnimplemented()),
    28 => Err(errUnimplemented()),
    29 => Err(errUnimplemented()),
    30 => Err(errUnimplemented()),
    31 => Err(errUnimplemented()),
    32 => Ok(OpCode::OpReturn),
    33 => Err(errUnimplemented()),
    34 => Err(errUnimplemented()),
    35 => Err(errUnimplemented()),
    _  => Err(Error::new(ErrorKind::InvalidData, "invalid opcode"))
  }

}
fn readStringLen<R:BufRead>(source: &mut R) -> Result<String> {
  let size = readU64(source)?; // put bytestring puts it as BIG ENDIAN!
  let mut string = String::new();
  let mut handle = source.take(size); 
  handle.read_to_string(&mut string)?;
  return Ok(string);
}
fn readI64<R:BufRead>(source: &mut R) -> Result<i64> {
  let mut buf:[u8; 8] = [0; 8]; 
  source.read_exact(&mut buf)?;
  // currently encoded in big endian... todo: on haskell side, gen little endian
  return Ok(i64::from_be_bytes(buf));
}
fn readU64<R:BufRead>(source: &mut R) -> Result<u64> {
  let mut buf:[u8; 8] = [0; 8]; 
  source.read_exact(&mut buf)?;
  // copy pasted code LOL
  return Ok(u64::from_be_bytes(buf));
}
fn readI16<R:BufRead>(source: &mut R) -> Result<i16> {
  let mut buf:[u8; 2] = [0; 2];
  source.read_exact(&mut buf)?;
  return Ok(i16::from_be_bytes(buf));
}

fn readU16<R:BufRead>(source:&mut R) -> Result<u16> {
  let mut buf:[u8; 2] = [0; 2];
  source.read_exact(&mut buf)?;
  return Ok(u16::from_be_bytes(buf));
}
fn readF64<R:BufRead>(source: &mut R) -> Result<f64> {
  let mut buf:[u8; 8] = [0; 8];
  source.read_exact(&mut buf)?; 
  return Ok(f64::from_be_bytes(buf));
}
fn errUnimplemented() -> Error {
  return Error::new(ErrorKind::Other, "Unimplemented");
} 
fn getOneByte<R:BufRead>(source: &mut R) -> Result<u8> {
  let mut buf:[u8; 1] = [0];
  source.read_exact(&mut buf)?; 
  // unwrap is safe here as if it encountered EOF, then it will return an error before it reaches
  // this. and if no early EOF, then we KNOW that the buf is full
  // not like this matters, as even if it's not full, it will have a zero in it (altho this is
  // undesirable)
  return Ok(*buf.get(0).unwrap());
}

fn hasDataLeft<R:BufRead>(source: &mut R) -> Result<bool> {
  return source.fill_buf().map(|b| !b.is_empty());
}
