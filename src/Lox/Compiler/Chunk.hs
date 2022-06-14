{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies #-}
module Lox.Compiler.Chunk where 

import Data.Word (Word8, Word32)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary
import Data.Bool (bool)
import GHC.Generics (Generic)
import Data.ByteString.Lazy qualified as B
import Data.List (sortBy)
data OpCode 
  = OpConstant
  | OpNil
  | OpTrue
  | OpFalse
  | OpPop
  | OpGetLocal
  | OpSetLocal
  | OpGetGlobal
  | OpDefineGlobal
  | OpSetGlobal
  | OpGetUpValue
  | OpSetUpValue
  | OpGetProperty
  | OpSetProperty
  | OpGetSuper
  | OpEqual
  | OpGreater
  | OpLess
  | OpAdd
  | OpSubtract
  | OpMultiply
  | OpDivide 
  | OpNot
  | OpNegate
  | OpPrint
  | OpJump
  | OpJumpIfFalse
  | OpCall
  | OpInvoke
  | OpSuperInvoke
  | OpClosure
  | OpCloseUpValue
  | OpReturn
  | OpClass
  | OpInherit
  | OpMethod
  deriving (Enum, Show) 

opCodeByte :: OpCode -> Word8
opCodeByte = fromIntegral . fromEnum

opCodeFromByte :: Word8 -> OpCode
opCodeFromByte = toEnum . fromIntegral

instance Binary OpCode where 
  put = put . opCodeByte
  get = opCodeFromByte <$> getWord8
data Value 
  = ValBool Bool
  | ValNil 
  | ValNumber Double
  | ValString Text
  | ValFunction ObjFunction

instance Binary Value where 
  put (ValBool b) = putWord8 0 *> putWord8 (if b then 1 else 0)
  put ValNil = putWord8 1
  put (ValNumber d) = putWord8 2 *> putDoublebe d
  put (ValString t) = putWord8 3 *> put t
  put (ValFunction fun) = putWord8 4 *> put fun
  get = do 
    kind <- getWord8
    case kind of 
      0 -> ValBool . (==1) <$> getWord8
      1 -> pure ValNil
      2 -> ValNumber <$> getDoublebe
      3 -> ValString <$> get
      4 -> ValFunction <$> get
      _ -> fail "Invalid Value kind"

newtype Chunk = Chunk 
  { chCode :: B.ByteString  -- all the data. can't be longer than 2 ^ 64 
  } 
  deriving newtype Binary


data ObjFunction = ObjFunction 
  { funArity :: Int
  , funChunk :: Chunk
  , funName  :: Text }
  deriving (Generic)

instance Binary ObjFunction
