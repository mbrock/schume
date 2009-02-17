module Schume.Codegen where

import Schume.Compiler
import Schume.Bytecode

import Control.Monad.State
import Control.Monad.Identity

import            Data.Map     (Map)
import qualified  Data.Map as  Map

import Data.List (elemIndex)

type Codegen a = StateT CodegenState Identity a 

data CodegenState = 
    CodegenState {
      codegenNextBodyID  :: BodyID,
      codegenBodies      :: Map BodyID [AO]
    }

generateCodeFor :: CPS -> ([AO], Map BodyID [AO])
generateCodeFor t = (code, codegenBodies state)
    where (code, state) = runCodegen (generateCode [] t)

runCodegen :: Codegen a -> (a, CodegenState)
runCodegen m = runIdentity (runStateT m state)
    where state = CodegenState 0 Map.empty

allocateBodyID :: Codegen BodyID
allocateBodyID = do  i <- gets codegenNextBodyID
                     modify (\s -> s { codegenNextBodyID = i + 1 })
                     return i

type LexicalEnvironment = [[Variable]]

generateCode :: LexicalEnvironment -> CPS -> Codegen [AO]
generateCode e t = 
    case t of
      CPSVariable v           -> 
           return [AOPushVariable (v `specifiedLexicallyIn` e)]
      CPSAbstraction vs t'    ->
           do  bodyID <- generateCodeForBody (vs:e) t'
               return [AOPushClosure bodyID]
      CPSAdministrative v t'  ->
           generateCode e (CPSAbstraction [v] t')
      CPSApplication t' ts    ->
           do  argumentCodes  <- mapM (generateCode e) ts
               calleeCode     <- generateCode e t'
               return $ concat argumentCodes ++ calleeCode ++ [AOTailcall]

specifiedLexicallyIn :: Variable -> LexicalEnvironment -> LexicalSpecifier
specifiedLexicallyIn = f 0
    where f _ _ []      = error "internal error"
          f n v (x:xs)  = case elemIndex v x of
                            Nothing  -> f (n + 1) v xs
                            Just i   -> (n, i)

generateCodeForBody :: LexicalEnvironment -> CPS -> Codegen BodyID
generateCodeForBody e t =
    do  bodyID  <- allocateBodyID
        code    <- generateCode e t
        modify (\s -> s { codegenBodies = 
                              Map.insert bodyID code (codegenBodies s) })
        return bodyID
