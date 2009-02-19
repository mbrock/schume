
{-# LANGUAGE DeriveDataTypeable,
             FlexibleInstances,
             GeneralizedNewtypeDeriving #-}

module Schume.Compiler where

-- Use Neil Mitchell's delightful Uniplate library.
import Data.Generics
import Data.Generics.PlateData

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import            Data.Map     (Map)
import qualified  Data.Map as  Map

data E v  =  EVariable     v
          |  EAbstraction  [v]    (E v)
          |  EApplication  (E v)  [E v]  
          |  ECallcc       (E v)

type ES  = E String
type EV  = E Variable

data Variable = MkVariable {  variableID    :: Integer,
                              variableName  :: String }
   deriving (Eq, Data, Typeable, Show)

data CPS  =  CPSVariable        Variable
          |  CPSAbstraction     [Variable]  CPS
          |  CPSAdministrative  Variable    CPS
          |  CPSApplication     CPS         [CPS]  
    deriving (Eq, Data, Typeable, Show)

isCPSTrivial :: CPS ->Bool
isCPSTrivial (CPSApplication _ _)  = False
isCPSTrivial _                     = True

(@@) :: CPS -> [CPS] -> CPS
(@@) = CPSApplication

type Compiler a =  ErrorT CompilationError 
                     (StateT CompilerState Identity) a

data CompilerState = 
    CompilerState {
      csNextGensymID :: Integer
    }

data CompilationError =
       XUnboundVariable  String
    |  XInternalError    String
    |  XInsaneCPS        CPS
       deriving Show

instance Error CompilationError where
    strMsg = XInternalError

throwWhen :: MonadError e m => Bool -> e -> m ()
throwWhen p e = when p (throwError e)

runCompiler :: Compiler a -> Either CompilationError a
runCompiler x = runIdentity (evalStateT (runErrorT x) state)
    where state =  CompilerState { 
                     csNextGensymID = 0
                   }

compile :: ES -> Either CompilationError CPS
compile e0 = runCompiler (do e1 <- giveVariablesUniqueIDs e0
                             e2 <- convertToCPS e1
                             return e2)

-- Generates a fresh variable with the given name.
gensym :: String -> Compiler Variable
gensym name = do  i <- gets csNextGensymID
                  modify (\s -> s { csNextGensymID = i + 1 })
                  return (MkVariable i name)

-- Assigns a unique ID to every lexical variable.
giveVariablesUniqueIDs :: ES -> Compiler EV
giveVariablesUniqueIDs = f Map.empty
    where 
      f renamings expression = 
        case expression of
          EAbstraction ps e ->
              do  ps'  <- mapM gensym ps
                  e'   <- f (renamings `withBindings` (zip ps ps')) e
                  return $ EAbstraction ps' e'
          EVariable v ->
              case Map.lookup v renamings of
                  Nothing  -> throwError $ XUnboundVariable v
                  Just v'  -> return $ EVariable v'
          EApplication callee args -> 
              liftM2 EApplication  (f renamings callee)
                                   (mapM (f renamings) args)
          ECallcc e ->
              do  e' <- f renamings e
                  return $ ECallcc e'

-- Adds mappings to a map, overwriting.
withBindings :: Ord k => Map k a -> [(k, a)] -> Map k a
withBindings m bs = Map.union (Map.fromList bs) m


-- Transforms a top-level expression to continuation-passing style.
convertToCPS :: EV -> Compiler CPS
convertToCPS e =
    do  k    <- gensym "%root-k"
        cps  <- cpsify e (CPSVariable k)
        cpsSanityCheck cps
        return (CPSAbstraction [k] (reduceAdministrativeRedexes cps))

-- Checks that a CPS term has no applications with nontrivial arguments.
cpsSanityCheck :: CPS -> Compiler ()
cpsSanityCheck t = throwWhen (any p (universe t)) (XInsaneCPS t)
    where  p (CPSApplication t' ts)  = any (not . isCPSTrivial) (t' : ts)
           p _                       = False

-- Dispatches on expression type.
cpsify :: EV -> CPS -> Compiler CPS
cpsify e k =
    case e of
      EVariable     v       -> return (k @@ [CPSVariable v])
      EAbstraction  vs  e'  -> cpsifyAbstraction vs e' k
      EApplication  e'  es  -> cpsifyApplication e' es k
      ECallcc       e'      -> cpsifyCallcc e' k

cpsifyAbstraction :: [Variable] -> EV -> CPS -> Compiler CPS
cpsifyAbstraction vs e k =
    do  k'  <- gensym "%k"
        e'  <- cpsify e (CPSVariable k')
        return  (k @@ [CPSAbstraction (k' : vs) e'])

cpsifyApplication :: EV -> [EV] -> CPS -> Compiler CPS
cpsifyApplication e es k =
    do  ts   <- replicateM (length es) (gensym "%arg")
        v    <- gensym "%f"
        let  step (t, x) r  =  cpsify x (CPSAdministrative t r)
             finale         =  CPSVariable v @@ (k : map CPSVariable ts)
        call <- foldrM step finale (zip ts es)
        cpsify e (CPSAdministrative v call)

cpsifyCallcc :: EV -> CPS -> Compiler CPS
cpsifyCallcc e k =
    do  f   <- gensym "%f"
        cpsify e  (CPSAdministrative f (CPSVariable f @@ [k, k]))

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM _  z []        = return z
foldrM f  z (x : xs)  = foldrM f z xs >>= f x

-- Substitution on CPS terms is easy since variables are bound and never
-- shadowed.
substitute :: Variable -> CPS -> CPS -> CPS
substitute v x e =
    case e of
      CPSVariable w | (v == w)  -> x
      _                         -> descend (substitute v x) e

-- Reduce redexes with administrative callees.
reduceAdministrativeRedexes :: CPS -> CPS
reduceAdministrativeRedexes = rewrite f
    where
      f (CPSApplication (CPSAdministrative v e) [x]) = 
          Just (substitute v x e)
      f _ = 
          Nothing
