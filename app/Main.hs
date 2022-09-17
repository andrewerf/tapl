module Main (main) where

import SimpleLambda
import SimpleLambdaAST
import SimpleLambdaParser ( parse )
import System.Environment.Blank ( getArgs )


processString :: [(String, Type)] -> String -> TermEither
processString ctx s = ( desugarAndRemoveNames ctx.parse ) s >>= eval

main :: IO ()
main = do
  args <- getArgs
  let ctx = [( "a", TypeVar "A" ), ( "b", TypeVar "A" )] :: [(String, Type)]
  fileContent <- readFile $ head args
  print $ TermWithNamingContext ( map fst ctx ) <$>  processString ctx fileContent


--main :: IO ()
--main =
--  print term >>
--  print desugaredTerm >>
--  print maybeNamelessTerm >>
--  print isVal >>
--  print maybeEvaluatedTerm >>
--  print ( TermWithContext ctx <$> maybeEvaluatedTerm )
--  where
--    ctx = ["p1", "p2"]
--    term = parse "p1 p2"
--    desugaredTerm = desugar term
--    maybeNamelessTerm = removeNames ctx desugaredTerm
--    maybeEvaluatedTerm = maybeNamelessTerm >>= eval
--    isVal = isval <$> maybeNamelessTerm