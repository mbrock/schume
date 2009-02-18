
module Main where

import Schume.Bytecode

enumDeclaration :: String
enumDeclaration = enumify . map (\(a, b) -> a ++ " = " ++ show b ++ ",\n") $ xs
    where xs = [("AOPushClosure",  tagForPushClosure),
                ("AOPushVariable", tagForPushVariable),
                ("AOTailcall",     tagForTailcall)]
          enumify x = "enum AOType {\n" ++ concatMap ("  " ++) x ++ "};\n"

main :: IO ()
main = putStr enumDeclaration