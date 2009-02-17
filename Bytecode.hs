
module Bytecode (LexicalSpecifier, BodyID, AO (..)) where

type LexicalSpecifier = (Int, Int)

type BodyID = Int

data AO  =  AOPushClosure   BodyID
         |  AOPushVariable  LexicalSpecifier
         |  AOTailcall
