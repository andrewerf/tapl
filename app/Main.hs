module Main (main) where

import SimpleLambda
import SimpleLambdaAST
import SimpleLambdaPreprocess
import SimpleLambdaTypes
import SimpleLambdaParser ( parse )
import System.Environment.Blank ( getArgs )


processString :: Context -> String -> Either String Term
processString ctx s = case ( maybeDesugared, maybeType ) of
  ( Left err, _ ) -> Left $ show err
  ( _, Left err ) -> Left err
  ( Right term, _ ) -> eval term
  where
    maybeDesugared = ( desugarAndRemoveNames ctx.parse ) s
    maybeType = case maybeDesugared of
      ( Left err ) ->  Left $ show err
      ( Right term ) -> case typeof ctx term of
        ( Left err ) -> Left $ show err
        ( Right tp ) -> Right tp

main :: IO ()
main = do
  args <- getArgs
  let ctx = [VarWithType "a" ( TypeVar "A" ), VarWithType "aa" ( TypeVar "A" ), VarWithType "b" ( TypeVar "B" )]
  fileContent <- readFile $ head args
  putStr $ case processString ctx fileContent of
    ( Left err ) -> err
    ( Right t ) -> show $ TermWithContext ctx t