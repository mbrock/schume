{-# LANGUAGE ExistentialQuantification #-}

module Schume.Bytecode (LexicalSpecifier, 
                        BodyID,
                        PrimitiveID,
                        AO (..),
                        tagForPushClosure,
                        tagForPushVariable,
                        tagForTailcall,
                        CompiledModule (..)) where

import Data.Binary

import qualified Data.Map as Map
import           Data.Map   (Map)

import Data.Maybe (fromMaybe)

import Control.Arrow
import Control.Applicative
import Control.Monad

type BodyID           = Word16
type PrimitiveID      = Word16
type LexicalSpecifier = (Word16, Word16)

data AO  =  AOPushClosure   BodyID
         |  AOPushVariable  LexicalSpecifier
         |  AOPushPrimitive PrimitiveID
         |  AOTailcall
            deriving Show

data CompiledModule = 
    CompiledModule { compiledModuleEntryPoint :: BodyID,
                     compiledModuleBodies     :: Map BodyID [AO] }

tagFor :: AO -> Word8
tagFor (AOPushClosure _)   = 0
tagFor (AOPushVariable _)  = 1
tagFor (AOPushPrimitive _) = 2
tagFor  AOTailcall         = 3

tagForPushClosure :: Word8
tagForPushClosure  = tagFor (AOPushClosure undefined)

tagForPushVariable :: Word8
tagForPushVariable = tagFor (AOPushVariable undefined)

tagForPushPrimitive :: Word8
tagForPushPrimitive = tagFor (AOPushPrimitive undefined)

tagForTailcall :: Word8
tagForTailcall  = tagFor AOTailcall

data AnyBinary = forall a. Binary a => AnyBinary a

argumentFor :: AO -> AnyBinary
argumentFor (AOPushClosure x)   = AnyBinary x
argumentFor (AOPushVariable x)  = AnyBinary x
argumentFor (AOPushPrimitive x) = AnyBinary x
argumentFor AOTailcall          = AnyBinary ()

instance Binary AnyBinary where
    put (AnyBinary x) = put x
    get               = undefined

instance Binary AO where
    put = put . (tagFor &&& argumentFor)
    get = do tag <- getWord8
             fromMaybe undefined 
               (lookup tag [(tagForPushClosure,   AOPushClosure  <$> get),
                            (tagForPushVariable,  AOPushVariable <$> get),
                            (tagForPushPrimitive, AOPushPrimitive <$> get),
                            (tagForTailcall,      return AOTailcall)])

instance Binary CompiledModule where
    put = put . (compiledModuleEntryPoint &&& compiledModuleBodies)
    get = CompiledModule <$> get <*> get

