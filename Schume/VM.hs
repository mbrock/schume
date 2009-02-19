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

primitives :: [VM ()]
primitives = [primitiveExit, primitivePrintStar, primitivePrintNewline]

primitiveExit :: VM ()
primitiveExit = lookupVariable (0, 1) >>= throwError . XExit

primitivePrintStar :: VM ()
primitivePrintStar = do liftIO (putStr "*")
                        k <- lookupVariable (0, 0)
                        push Nil
                        setupCall k

primitivePrintNewline :: VM ()
primitivePrintNewline = do liftIO (putStr "\n")
                           k <- lookupVariable (0, 0)
                           push Nil
                           setupCall k

data Value = Closure { closureCode        :: [AO],
                       closureEnvironment :: Environment }
           | Nil
           | Primitive PrimitiveID
             deriving Show

exitContinuation :: Value
exitContinuation = Closure [AOPushPrimitive 0, -- "nil"
                            AOPushVariable (0, 0),
                            AOPushPrimitive 0,
                            AOTailcall]
                           []

data VMError = XCodeUnderrun
             | XStackUnderrun
             | XNoSuchBody BodyID
             | XInternalError String
             | XNotFunction Value
             | XExit Value
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
runModule = do push exitContinuation
               getEntryPoint >>= getBody >>= makeClosure >>= setupCall
               bigStep

bigStep :: VM Value
bigStep = do -- get >>= liftIO . print
             x <- (step >> return Nothing) `catchError` handler
             case x of
               Nothing -> bigStep
               Just v  -> return v
    where handler e = case e of
                        XExit v -> return (Just v)
                        _       -> throwError e

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
setupCall (Primitive n) =
    do replaceEnvironment []
       moveStackToEnvironment
       primitives !! (fromIntegral n)
setupCall Nil =
    throwError (XNotFunction Nil)

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

step :: VM ()
step =
    do op <- readOperation
       case op of
         AOPushClosure bodyID ->
             do getBody bodyID >>= makeClosure >>= push
         AOPushVariable v ->
             do lookupVariable v >>= push
         AOPushPrimitive n ->
             do push (Primitive n)
         AOTailcall ->
             do pop >>= setupCall
