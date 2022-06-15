use crate::lox::vm::{OpCode, LxValue};
use std::env;
use std::fs::File;
use std::io::{Read, BufRead, BufReader, Seek, Error, ErrorKind, Result};
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
              let file = File::open(path)?;
              let mut reader = BufReader::new(file);
              let opcodes = parse_op_codes(&mut reader, None)?;
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
fn parse_op_codes<R:BufRead + Seek>(source: &mut R, size: Option<u64>) -> Result<Vec<OpCode>> {
  let mut vec : Vec<OpCode> = Vec::new();
  let start_pos = source.stream_position()?;
  loop {
    let at_eof = !has_data_left(source)?;
    if at_eof { break; } 
    let opcode = parse_op_code(source)?;
    vec.push(opcode);
    if let Some(limit) = size {
      let cur_pos = source.stream_position()?;
      let amount_taken = cur_pos - start_pos; 
      if amount_taken > limit {
        return Err(Error::new(ErrorKind::UnexpectedEof, "Took too many bytes"));
      } else if amount_taken == limit {
        break;
      }
    }
  } 
  return Ok(vec);
}
fn parse_op_code<R:BufRead + Seek>(source: &mut R) -> Result<OpCode> {
  // because I'm already borrowing it I don't need to reborrow it???
  let opcode = get_one_byte(source)?;
  match opcode { 
    0 => {
      let const_kind = get_one_byte(source)?;
      match const_kind {
        0 => {
          let bool_byte = get_one_byte(source)?;
          let da_bool :bool = match bool_byte { 
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(Error::new(ErrorKind::InvalidData, "Bool must be 1 or 0"))
          }?;
          return Ok(OpCode::OpConstant(LxValue::ValBool(da_bool)));
        }
        1 => Ok(OpCode::OpConstant(LxValue::ValNil)),
        2 => Ok(OpCode::OpConstant(LxValue::ValNumber(read_f64(source)?))),
        3 => Ok(OpCode::OpConstant(LxValue::ValString(read_string_len(source)?))), 
        4 => {
          let arity = read_i64(source)?;
          let size  = read_u64(source)?; // size of chunk
          // take is sane in that Take<BufRead> has the trait BufRead
          
          
          let opcodes = parse_op_codes(source, Some(size))?;
          let name = read_string_len(source)?;
          return Ok(OpCode::OpConstant(LxValue::ValFunction(arity, opcodes, name)));
        }
        _ => Err(Error::new(ErrorKind::InvalidData, "Invalid constant kind"))
      } 
    }
    1 => Ok(OpCode::OpNil),
    2 => Ok(OpCode::OpTrue),
    3 => Ok(OpCode::OpFalse),
    4 => Ok(OpCode::OpPop), 
    5 => Ok(OpCode::OpGetLocal(read_string_len(source)?)),
    6 => Ok(OpCode::OpSetLocal(read_string_len(source)?)),
    7 => Ok(OpCode::OpGetGlobal(read_string_len(source)?)),
    8 => Ok(OpCode::OpDefineGlobal(read_string_len(source)?)),
    9 => Ok(OpCode::OpSetGlobal(read_string_len(source)?)),
    10 => Err(err_unimplemented()),
    11 => Err(err_unimplemented()),
    12 => Err(err_unimplemented()),
    13 => Err(err_unimplemented()),
    14 => Err(err_unimplemented()),
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
    25 => Ok(OpCode::OpJump(read_i16(source)?)),
    26 => Ok(OpCode::OpJumpIfFalse(read_u16(source)?)),
    27 => Err(err_unimplemented()),
    28 => Err(err_unimplemented()),
    29 => Err(err_unimplemented()),
    30 => Err(err_unimplemented()),
    31 => Err(err_unimplemented()),
    32 => Ok(OpCode::OpReturn),
    33 => Err(err_unimplemented()),
    34 => Err(err_unimplemented()),
    35 => Err(err_unimplemented()),
    _  => Err(Error::new(ErrorKind::InvalidData, "invalid opcode"))
  }

}

fn read_string_len<R:BufRead>(source: &mut R) -> Result<String> {
  let size = read_u64(source)?; // put bytestring puts it as BIG ENDIAN!
  let mut string = String::new();
  let mut handle = source.take(size); 
  handle.read_to_string(&mut string)?;
  return Ok(string);
}
fn read_i64<R:BufRead>(source: &mut R) -> Result<i64> {
  let mut buf:[u8; 8] = [0; 8]; 
  source.read_exact(&mut buf)?;
  // currently encoded in big endian... todo: on haskell side, gen little endian
  return Ok(i64::from_be_bytes(buf));
}
fn read_u64<R:BufRead>(source: &mut R) -> Result<u64> {
  let mut buf:[u8; 8] = [0; 8]; 
  source.read_exact(&mut buf)?;
  // copy pasted code LOL
  return Ok(u64::from_be_bytes(buf));
}
fn read_i16<R:BufRead>(source: &mut R) -> Result<i16> {
  let mut buf:[u8; 2] = [0; 2];
  source.read_exact(&mut buf)?;
  return Ok(i16::from_be_bytes(buf));
}

fn read_u16<R:BufRead>(source:&mut R) -> Result<u16> {
  let mut buf:[u8; 2] = [0; 2];
  source.read_exact(&mut buf)?;
  return Ok(u16::from_be_bytes(buf));
}
fn read_f64<R:BufRead>(source: &mut R) -> Result<f64> {
  let mut buf:[u8; 8] = [0; 8];
  source.read_exact(&mut buf)?; 
  return Ok(f64::from_be_bytes(buf));
}
fn err_unimplemented() -> Error {
  return Error::new(ErrorKind::Other, "Unimplemented");
} 
fn get_one_byte<R:BufRead>(source: &mut R) -> Result<u8> {
  let mut buf:[u8; 1] = [0];
  source.read_exact(&mut buf)?; 
  // unwrap is safe here as if it encountered EOF, then it will return an error before it reaches
  // this. and if no early EOF, then we KNOW that the buf is full
  // not like this matters, as even if it's not full, it will have a zero in it (altho this is
  // undesirable)
  return Ok(*buf.get(0).unwrap());
}

fn has_data_left<R:BufRead>(source: &mut R) -> Result<bool> {
  return source.fill_buf().map(|b| !b.is_empty());
}
