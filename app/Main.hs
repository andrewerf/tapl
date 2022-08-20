module Main (main) where

import TypelessArithmetics
import TypelessArithmeticsLexer
import TypelessArithmeticsParser


main :: IO ()
main = print ( eval $ parseTypelessArithmetics $ alexScanTokens "if iszero(0) then if iszero(pred(succ(0))) then succ(0) else succ(succ(0)) else 0" )