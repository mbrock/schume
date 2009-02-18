
module Schume.Bytecode (LexicalSpecifier, 
                        BodyID, 
                        AO (..), 
                        AOs (..),
                        CompiledModule (..)) where

import Data.Binary

import qualified Data.Map as Map
import           Data.Map   (Map)

import Data.Maybe (fromMaybe)

import Control.Monad

type LexicalSpecifier = (Word16, Word16)

type BodyID = Word16

data AO  =  AOPushClosure   BodyID
         |  AOPushVariable  LexicalSpecifier
         |  AOTailcall

newtype AOs = AOs [AO]

data CompiledModule = 
    CompiledModule { compiledModuleEntryPoint :: BodyID,
                     compiledModuleBodies     :: Map BodyID AOs }

tagFor :: AO -> Word8
tagFor (AOPushClosure _)   = 0
tagFor (AOPushVariable _)  = 1
tagFor  AOTailcall         = 2

tagForPushClosure :: Word8
tagForPushClosure  = tagFor (AOPushClosure undefined)

tagForPushVariable :: Word8
tagForPushVariable = tagFor (AOPushVariable undefined)

tagForTailcall :: Word8
tagForTailcall  = tagFor AOTailcall

instance Binary AO where
    put x = do put (tagFor x :: Word8) 
               case x of
                 AOPushClosure   i    -> put i
                 AOPushVariable (i,j) -> put i >> put j
                 AOTailcall           -> return ()
    get = do tag <- getWord8
             fromMaybe undefined 
               (lookup tag [(tagForPushClosure,  liftM AOPushClosure get),
                            (tagForPushVariable, liftM AOPushVariable get),
                            (tagForTailcall,     return AOTailcall)])

instance Binary AOs where
    put (AOs x) = do put (fromIntegral $ length x :: Word16)
                     mapM_ put x
    get = do n <- get :: Get Word16
             aos <- replicateM (fromIntegral n) get
             return (AOs aos)

instance Binary CompiledModule where
    put x = do put $ compiledModuleEntryPoint x
               let bodies = compiledModuleBodies x
               put (fromIntegral $ Map.size bodies :: Word16)
               Map.foldWithKey (\k v m -> put k >> put v >> m) (return ()) bodies
    get = do entryPoint <- get :: Get BodyID
             n <- get :: Get Word16
             bodies <- replicateM (fromIntegral n) (liftM2 (,) get get)
             return $ CompiledModule entryPoint (Map.fromList bodies)
