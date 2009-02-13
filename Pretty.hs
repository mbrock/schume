
module Pretty where

import Compiler

import Text.PrettyPrint

showCPS :: CPS -> String
showCPS = render . printCPS

printCPS :: CPS -> Doc
printCPS e = case e of
               CPSVariable v -> printCPSVariable v
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