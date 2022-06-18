{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingVia #-}
module Lox.Compiler.Chunk where 

import Data.Word (Word8, Word32, Word64, Word16)
import Data.Int (Int16)
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
  = OpConstant Value 
  | OpNil
  | OpTrue
  | OpFalse
  | OpPop
  | OpGetLocal Text
  | OpSetLocal Text
  | OpGetGlobal Text
  | OpDefineGlobal Text
  | OpSetGlobal Text
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
  | OpJump Int16
  | OpJumpIfFalse Word16
  | OpCall
  | OpInvoke
  | OpSuperInvoke
  | OpClosure
  | OpCloseUpValue
  | OpReturn
  | OpClass
  | OpInherit
  | OpMethod
  deriving (Show, Generic) 
  deriving anyclass Binary -- AFAIK - Generic binary does basically what I did anyway :heart:


data Value 
  = ValBool Bool
  | ValNil 
  | ValNumber Double
  | ValString Text
  | ValObj Int
  deriving (Show, Generic)
  deriving anyclass Binary
data Chunk = Chunk 
  { chCode :: [OpCode] -- very helpful :wink: 
  , chConstants :: [Obj] 
  } 
  deriving (Generic, Show)
  deriving anyclass Binary

data Obj = ObjFunction 
  { funArity :: Int
  , funChunk :: Chunk
  , funName  :: Text }
  | ObjUnused -- force Binary to use sum encoding
  deriving (Generic, Show)
  deriving anyclass Binary




data FunctionType 
  = TypeFunction
  | TypeScript
  deriving Generic
  deriving anyclass Binary
