
module Schume.Pretty where

import Schume.Compiler
import Schume.Bytecode

import Data.Binary
import Text.PrettyPrint

import Control.Arrow (first)

import qualified Data.Map as Map
import           Data.Map   ()

showCPS :: CPS -> String
showCPS = render . printCPS

showProgram :: CompiledModule -> String
showProgram = render . printProgram

printProgram :: CompiledModule -> Doc
printProgram (CompiledModule entry bodies) =
    vcat ((text ("Entry point: " ++ show entry)) :
          (map (\(x, y) -> hang (text (x ++ ":")) 2 (printCode y))
           (map (first show) (Map.toList bodies))))

printCode :: [AO] -> Doc
printCode = fsep . map printAO

printAO :: AO -> Doc
printAO (AOPushClosure bodyID) = 
    text "push-closure" <+> printBodyID bodyID <> text ";"
printAO (AOPushVariable x) =
    text "push-variable" <+> text (show x) <> text ";"
printAO (AOPushPrimitive x) =
    text "push-primitive" <+> text (show x) <> text ";"
printAO (AOTailcall) =
    text "tailcall;"

printBodyID :: Word16 -> Doc
printBodyID x = text "<" <> text (show x) <> text ">"

printCPS :: CPS -> Doc
printCPS e = case e of
               CPSVariable v -> printCPSVariable v
               CPSPrimitive v -> printCPSPrimitive v
               CPSAdministrative v e' ->
                   hang (parens (text "%lambda" <+>
                                 parens (printCPSVariable v)))
                        2 (printCPS e')
               CPSAbstraction vs e' ->
                   hang (parens (text "lambda" <+>
                                 parens (hsep (map printCPSVariable vs))))
                        2 (printCPS e')
               CPSApplication e' es ->
                   parens (sep (map printCPS (e' : es)))

printCPSVariable :: Variable -> Doc
printCPSVariable v =
    text (variableName v) <> text "-" <> text (show (variableID v))

printCPSPrimitive :: Integer -> Doc
printCPSPrimitive n =
    parens (text "%primitive" <+> text (show n))