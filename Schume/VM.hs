{-# LANGUAGE EmptyDataDecls #-}

module Schume.VM where

import Schume.Bytecode

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity

import qualified Data.Map as Map

type VM a = ReaderT CompiledModule
              (ErrorT VMError
                (StateT VMState IO)) a

data VMState = VMState { vmOperationQueue :: [AO],
                         vmEnvironment    :: Environment,
                         vmStack          :: [Value] }
               deriving Show

type Environment = [[Value]]

emptyState :: VMState
emptyState = VMState { vmOperationQueue = [],
                       vmEnvironment    = [],
                       vmStack          = [] }

data Value = Closure { closureCode        :: [AO],
                       closureEnvironment :: Environment }
           | BuiltinExit
             deriving Show

data VMError = XCodeUnderrun
             | XStackUnderrun
             | XNoSuchBody BodyID
             | XInternalError String
               deriving Show

instance Error VMError where
    strMsg = XInternalError

evalVM :: CompiledModule -> VM a -> IO (Either VMError a)
evalVM m vm = run m
    where run = flip evalStateT emptyState .
                runErrorT .
                runReaderT vm

evalModule :: CompiledModule -> IO (Either VMError Value)
evalModule m = evalVM m runModule

runModule :: VM Value
runModule = do push BuiltinExit
               getEntryPoint >>= getBody >>= makeClosure >>= setupCall
               bigStep

bigStep :: VM Value
bigStep = do x <- step
             case x of
               Nothing -> get >>= liftIO . print >> bigStep
               Just y  -> return y

push :: Value -> VM ()
push v = modify (\s -> s { vmStack = v : vmStack s })

getBody :: BodyID -> VM [AO]
getBody bodyID = do bodies <- asks compiledModuleBodies
                    case Map.lookup bodyID bodies of
                      Nothing   -> throwError (XNoSuchBody bodyID)
                      Just code -> return code

getEntryPoint :: VM BodyID
getEntryPoint = asks compiledModuleEntryPoint

makeClosure :: [AO] -> VM Value
makeClosure code = liftM (Closure code) (gets vmEnvironment)

setupCall :: Value -> VM ()
setupCall (Closure code environment) =
    do enqueueOperations code
       replaceEnvironment environment
       moveStackToEnvironment
setupCall BuiltinExit =
    error "should never setupCall to BuiltinExit"

enqueueOperations :: [AO] -> VM ()
enqueueOperations code = modify (\s -> s { vmOperationQueue = code })

replaceEnvironment :: Environment -> VM ()
replaceEnvironment e = modify (\s -> s { vmEnvironment = e })

moveStackToEnvironment :: VM ()
moveStackToEnvironment =
  modify (\s -> s { vmEnvironment = reverse (vmStack s) : vmEnvironment s,
                    vmStack       = [] })

readOperation :: VM AO
readOperation = do queue <- gets vmOperationQueue
                   case queue of
                     [] -> throwError XCodeUnderrun
                     (x:xs) -> do modify (\s -> s { vmOperationQueue = xs })
                                  return x

lookupVariable :: LexicalSpecifier -> VM Value
lookupVariable (m, n) = liftM f (gets vmEnvironment)
    where f e = (e !! (fromIntegral m)) !! (fromIntegral n)

pop :: VM Value
pop = do stack <- gets vmStack
         case stack of
           [] -> throwError XStackUnderrun
           (x:xs) -> do modify (\s -> s { vmStack = xs })
                        return x

step :: VM (Maybe Value)
step =
    do op <- readOperation
       case op of
         AOPushClosure bodyID ->
             do getBody bodyID >>= makeClosure >>= push
                return Nothing
         AOPushVariable v ->
             do lookupVariable v >>= push
                return Nothing
         AOTailcall ->
             do callee <- pop
                case callee of
                  BuiltinExit -> liftM Just pop
                  Closure _ _ -> do setupCall callee
                                    return Nothing
