
\section{Framework}

> {-# LANGUAGE DeriveDataTypeable,
>              FlexibleInstances,
>              GeneralizedNewtypeDeriving #-}
> module Compiler where
>
> -- Use Neil Mitchell's delightful Uniplate library.
> import Data.Generics
> import Data.Generics.PlateData
>
> import Control.Monad.State
> import Control.Monad.Error
> import Control.Monad.Identity
>
> import            Data.Map     (Map)
> import qualified  Data.Map as  Map
>
> import Data.List (elemIndex)

 This is the type of  expressions in the input language, parameterized
 over the type of variables.

> data E v  =  -- A (possibly nonunique) variable.
>              EVariable     v
>              -- A $\lambda$-abstraction.
>           |  EAbstraction  [v]    (E v)
>              -- An application.
>           |  EApplication  (E v)  [E v]  
>              -- A call-with-current-continuation form.
>           |  ECallcc       (E v)

 Raw source is  typed |E String|, but the first  pass transforms to |E
 Variable|.

> type ES  = E String
> type EV  = E Variable

> data Variable = MkVariable {  variableID    :: Integer,
>                               variableName  :: String }
>    deriving (Eq, Data, Typeable, Show)

 Then we  transform to continuation-passing style.  This  type is |EV|
 plus a constructor  for `administrative' $\lambda$-abstractions which
 are are used to name results  of subterms, and minus the special form
 for |callcc|.  

> data CPS  =  -- An (unshadowed) variable.
>              CPSVariable        Variable
>              -- A $\lambda$-abstraction from the source.
>           |  CPSAbstraction     [Variable]  CPS
>              -- A $\lambda$-abstraction invented by the CPS transform.
>           |  CPSAdministrative  Variable    CPS
>              -- An application.
>           |  CPSApplication     CPS         [CPS]  
>     deriving (Eq, Data, Typeable, Show)

> -- A `trivial' term is one whose evaluation evaluates no subterms.
> isCPSTrivial :: CPS -> Bool
> isCPSTrivial (CPSApplication _ _)  = False
> isCPSTrivial _                     = True

> -- Pretty syntax for building |CPSApplication|s.
> (@@) :: CPS -> [CPS] -> CPS
> (@@) = CPSApplication

\section{Compiler Monad}

> -- Type of compiler actions.
> type Compiler a =  ErrorT CompilationError 
>                      (StateT CompilerState Identity) a

> -- Global state for a compilation unit. 
> data CompilerState = 
>     CompilerState {
>       -- Used to generate unique IDs.
>       csNextGensymID :: Integer
>     }

> data CompilationError =
>        XUnboundVariable  String
>     |  XInternalError    String
>     |  XInsaneCPS        CPS
>        deriving Show

> -- Needed to use ErrorT.
> instance Error CompilationError where
>     strMsg = XInternalError

> throwWhen :: MonadError e m => Bool -> e -> m ()
> throwWhen p e = when p (throwError e)

 Compiler computations are run thusly:

> runCompiler :: Compiler a -> Either CompilationError a
> runCompiler x = runIdentity (evalStateT (runErrorT x) state)
>     where state =  CompilerState { 
>                      csNextGensymID = 0
>                    }

 Here is the entry point:

%format e_0
%format e_1

> compile :: ES -> Either CompilationError ([AO], Map BodyID [AO])
> compile e_0 = runCompiler (do  e_1 <- giveVariablesUniqueIDs e_0
>                                e_2 <- convertToCPS e_1
>                                let (code, bodies) = generateCodeFor e_2
>                                return (code, bodies))



 \section{Giving Variables Unique IDs}

> -- Generates a fresh variable with the given name.
> gensym :: String -> Compiler Variable
> gensym name = do  i <- gets csNextGensymID
>                   modify (\s -> s { csNextGensymID = i + 1 })
>                   return (MkVariable i name)

> -- Assigns a unique ID to every lexical variable.
> giveVariablesUniqueIDs :: ES -> Compiler EV
> giveVariablesUniqueIDs = f Map.empty
>     where 
>       f renamings expression = 
>         case expression of
>           EAbstraction ps e ->
>               do  ps'  <- mapM gensym ps
>                   e'   <- f (renamings `withBindings` (zip ps ps')) e
>                   return $ EAbstraction ps' e'
>           EVariable v ->
>               case Map.lookup v renamings of
>                   Nothing  -> throwError $ XUnboundVariable v
>                   Just v'  -> return $ EVariable v'
>           EApplication callee args -> 
>               liftM2 EApplication  (f renamings callee)
>                                    (mapM (f renamings) args)
>           ECallcc e ->
>               do  e' <- f renamings e
>                   return $ ECallcc e'


> -- Adds mappings to a map, overwriting.
> withBindings :: Ord k => Map k a -> [(k, a)] -> Map k a
> withBindings m bs = Map.union (Map.fromList bs) m

\section{CPS Transformation}

> -- Transforms a top-level expression to continuation-passing style.
> convertToCPS :: EV -> Compiler CPS
> convertToCPS e =
>     do  -- Create top-level continuation.
>         k    <- gensym "%root-k"
>         cps  <- cpsify e (CPSVariable k)
>         cpsSanityCheck cps
>         return (CPSAbstraction [k] (reduceAdministrativeRedexes cps))

> -- Checks that a CPS term has no applications with nontrivial arguments.
> cpsSanityCheck :: CPS -> Compiler ()
> cpsSanityCheck t = throwWhen (any f (universe t)) (XInsaneCPS t)
>     where  f (CPSApplication t' ts)  = any (not . isCPSTrivial) (t' : ts)
>            f _                       = False

> -- Dispatches on expression type.
> cpsify :: EV -> CPS -> Compiler CPS
> cpsify e k =
>     case e of
>       EVariable     v       -> return (k @@ [CPSVariable v])
>       EAbstraction  vs  e'  -> cpsifyAbstraction vs e' k
>       EApplication  e'  es  -> cpsifyApplication e' es k
>       ECallcc       e'      -> cpsifyCallcc e' k

> cpsifyAbstraction :: [Variable] -> EV -> CPS -> Compiler CPS
> cpsifyAbstraction vs e k =
>     do  -- Make a new continuation variable for the abstraction.
>         k'  <- gensym "%k"
>         -- Cpsify the body, having it yield to its continuation.
>         e'  <- cpsify e (CPSVariable k')
>         -- Yield the cpsified $\lambda$ to the current continuation.
>         return  (k @@ [CPSAbstraction (k' : vs) e'])

> cpsifyApplication :: EV -> [EV] -> CPS -> Compiler CPS
> cpsifyApplication e es k =
>     do  -- Make a new continuation variable for each argument.
>         ts   <- replicateM (length es) (gensym "%arg")
>         -- And one for the callee.
>         v    <- gensym "%f"
>         -- Cpsify each argument, yielding into temps, then call.
>         let  step (t, x) r  =  cpsify x (CPSAdministrative t r)
>              finale         =  CPSVariable v @@ (k : map CPSVariable ts)
>         call <- foldrM step finale (zip ts es)
>         -- Cpsify callee, yielding to its temp, then call.
>         cpsify e (CPSAdministrative v call)

> cpsifyCallcc :: EV -> CPS -> Compiler CPS
> cpsifyCallcc e k =
>     do  -- Make a temporary for the function value.
>         f   <- gensym "%f"
>         -- Cpsify $e$, yielding to a $\lambda$ that calls with $k$.
>         cpsify e  (CPSAdministrative f (CPSVariable f @@ [k, k]))

> -- Like |foldr|, but monadic.
> foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
> foldrM _  z []        = return z
> foldrM f  z (x : xs)  = foldrM f z xs >>= f x

 Substitution on CPS terms is easy since variables are bound and never
 shadowed.

> substitute :: Variable -> CPS -> CPS -> CPS
> substitute v x e =
>     case e of
>       CPSVariable w | (v == w)  -> x
>       _                         -> descend (substitute v x) e

 The CPS  transform introduces one administrative  $\lambda$ for every
 subterm  of an  application.  Thus  $ f  \, x  \mapsto \lambda  k. \,
 \left(\lambda e.  \, ((\lambda t. \, e \, k \, t) \, x)\right) \, f$.
 These  subterms   are  values  ---  they   call  their  continuations
 immediately  --- so  their administrative  $\lambda$s  are redundant.
 This transformation  removes redexes with  administrative callees, so
 $f \, x \mapsto \lambda k.\,f\,k\,x.$

> reduceAdministrativeRedexes :: CPS -> CPS
> reduceAdministrativeRedexes = rewrite f
>     where
>       f (CPSApplication (CPSAdministrative v e) [x]) = 
>           Just (substitute v x e)
>       f _ = 
>           Nothing

 \section{Abstract Machine Code}

> type LexicalEnvironment = [[Variable]]
> type LexicalSpecifier = (Int, Int)

> data AO  =  AOPushClosure   Int
>          |  AOPushVariable  LexicalSpecifier
>          |  AOTailcall
>             deriving Show

> type Codegen a = StateT CodegenState Identity a 

> type BodyID = Int

> data CodegenState = 
>     CodegenState {
>       codegenNextBodyID  :: BodyID,
>       codegenBodies      :: Map BodyID [AO]
>     }

> generateCodeFor :: CPS -> ([AO], Map BodyID [AO])
> generateCodeFor t = (code, codegenBodies state)
>     where (code, state) = runCodegen (generateCode [] t)

> runCodegen :: Codegen a -> (a, CodegenState)
> runCodegen m = runIdentity (runStateT m state)
>     where state = CodegenState 0 Map.empty

> allocateBodyID :: Codegen BodyID
> allocateBodyID = do  i <- gets codegenNextBodyID
>                      modify (\s -> s { codegenNextBodyID = i + 1 })
>                      return i

> generateCode :: LexicalEnvironment -> CPS -> Codegen [AO]
> generateCode e t = 
>     case t of
>       CPSVariable v           -> 
>            return [AOPushVariable (v `specifiedLexicallyIn` e)]
>       CPSAbstraction vs t'    ->
>            do  bodyID <- generateCodeForBody (vs:e) t'
>                return [AOPushClosure bodyID]
>       CPSAdministrative v t'  ->
>            generateCode e (CPSAbstraction [v] t')
>       CPSApplication t' ts    ->
>            do  argumentCodes  <- mapM (generateCode e) ts
>                calleeCode     <- generateCode e t'
>                return $ concat argumentCodes ++ calleeCode ++ [AOTailcall]

> specifiedLexicallyIn :: Variable -> LexicalEnvironment -> LexicalSpecifier
> specifiedLexicallyIn = f 0
>     where f _ _ []      = error "internal error"
>           f n v (x:xs)  = case elemIndex v x of
>                             Nothing  -> f (n + 1) v xs
>                             Just i   -> (n, i)

> generateCodeForBody :: LexicalEnvironment -> CPS -> Codegen BodyID
> generateCodeForBody e t =
>     do  bodyID  <- allocateBodyID
>         code    <- generateCode e t
>         modify (\s -> s { codegenBodies = 
>                               Map.insert bodyID code (codegenBodies s) })
>         return bodyID
