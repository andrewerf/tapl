module Main (main) where

import TypelessLambda
import TypelessLambdaAST
import TypelessLambdaParser ( parse )
import System.Environment.Blank ( getArgs )


processString :: Context -> String -> Maybe Term
processString ctx s = ( removeNames ctx.desugar.parse ) s >>= eval

main :: IO ()
main = do
  args <- getArgs
  let ctx = tail args
  fileContent <- readFile $ head args
  print $ TermWithContext ctx <$>  processString ctx fileContent


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