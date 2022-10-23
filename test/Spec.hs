import Test.HUnit
import Test.Hspec
import Test.Hspec.Contrib.HUnit

import Test.Lambda2.Core.AST
import Test.Lambda2.Core.Typing
import Test.Lambda2.Core.Preprocess

tests = TestList [
    TestLabel "AST" testsAST,
    TestLabel "Typing" testsTyping,
    TestLabel "Preprocess" testsPreprocess
  ]

main :: IO ()
main = hspec $ fromHUnitTest tests
