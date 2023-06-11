module Main (main) where

import Lambda2.Core.AST
import Lambda2.Core.BasicTypes
import Lambda2.Lambda2

import Relude.Lifted.Env ( getArgs )
import Control.Applicative ( liftA2 )

-- Γ = β:*, γ:*, b:β, c:γ
testContext :: Context
testContext = basicContext
--  extendContextWithVar "bb" ( TpVar 3 ) $
--    extendContextWithVar "c" ( TpVar 1 ) $
--      extendContextWithVar "b" ( TpVar 1 ) $
--        extendContextWithTypeVar "γ" $
--          extendContextWithTypeVar "β" mempty

main :: IO ()
main = do
  args <- getArgs
  fileContent <- readFile $ head args
  putStrLn $ case processString testContext fileContent of
    ( Left err ) -> err
    ( Right ( tm, tp ) ) -> show ( termToTermSimple testContext tm ) ++ " : " ++ show ( typeToTypeSimple testContext tp )