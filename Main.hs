
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

import qualified Data.Binary as Binary

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
      Ok  tree -> case compile (cToE tree) of
                    Left e -> hPutStrLn stderr (show e)
                    Right cps -> do let cm = generateCodeForProgram cps
                                    hPutStrLn stderr "CPS transformed:"
                                    hPutStrLn stderr (showCPS cps)
                                    hPutStrLn stderr "\nBytecode:"
                                    hPutStrLn stderr (showProgram cm)
                                    Binary.encodeFile "a.out" cm

main :: IO ()
main = do args <- getArgs
          case args of
            [filename] -> do source <- readFile filename
                             doCompile source
            _ -> do hPutStrLn stderr "usage: schumec <file>"
                    exitFailure
