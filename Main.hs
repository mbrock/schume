
module Main where

import System.IO
import System.Exit
import System.Environment

import Syntax.Abs
import Syntax.Par
import Syntax.ErrM

import Schume.Compiler
import Schume.Codegen
import Schume.Pretty

cToE :: CSExpr -> E String
cToE (CSVar i)      = EVariable (idName i)
cToE (CSApp c cs)   = EApplication (cToE c) (map cToE cs)
cToE (CSAbs is cs)  = EAbstraction (map idName is) (cToE cs)
cToE (CSCallcc cs)  = ECallcc (cToE cs)

idName :: ID -> String
idName (ID s) = s

doCompile :: String -> IO ()
doCompile s =
    case pCSExpr (myLexer s) of
      Bad e -> do hPutStrLn stderr "Error:"
                  hPutStrLn stderr e
                  exitFailure
      Ok  tree -> hPutStrLn stderr (either show showResult $ compile $ cToE tree)
    where showResult = showProgram . generateCodeFor

main :: IO ()
main = do args <- getArgs
          case args of
            [filename] -> do source <- readFile filename
                             doCompile source
            _ -> do hPutStrLn stderr "usage: schumec <file>"
                    exitFailure
