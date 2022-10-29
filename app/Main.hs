module Main (main) where

import Lambda2.Core.AST
import Lambda2.Core.Core
import Lambda2.Core.Typing
import Lambda2.Core.Preprocess
import Lambda2.Parsing.Parser
import Lambda2.Core.BasicTypes

import Relude.Lifted.Env ( getArgs )
import Control.Applicative ( liftA2 )

processString :: Context -> String -> Either String ( Term, Type )
processString ctx s = case ( maybeDesugared, maybeType ) of
  ( Left err, _ ) -> Left $ show err
  ( _, Left err ) -> Left err
  ( Right term, _ ) -> liftA2 (,) ( eval ctx term ) maybeType
  where
    maybeDesugared = ( desugar ctx.parse ) s
    maybeType = case maybeDesugared of
      ( Left err ) ->  Left $ show err
      ( Right term ) -> case typeof ctx term of
        ( Left err ) -> Left $ show err
        ( Right tp ) -> Right tp

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
  putStr $ case processString testContext fileContent of
    ( Left err ) -> err
    ( Right ( tm, tp ) ) -> show ( termToTermSimple testContext tm ) ++ " : " ++ show ( typeToTypeSimple testContext tp )