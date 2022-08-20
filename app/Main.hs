module Main (main) where

import TypelessArithmetics
import TypelessArithmeticsParser ( parse )

main :: IO ()
main =
  lines <$> getContents >>= mapM_ ( print.eval.parse )
