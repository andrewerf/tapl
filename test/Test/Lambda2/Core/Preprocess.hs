module Test.Lambda2.Core.Preprocess
  (
  testsPreprocess
  )
where

import Test.HUnit

import Helper
import Lambda2.Core.AST
import Lambda2.Core.Preprocess


-- Γ = β:*, γ:*, b:β, c:γ
testContext :: Context
testContext =
  extendContextWithVar "c" ( TpVar 1 ) $
    extendContextWithVar "b" ( TpVar 1 ) $
      extendContextWithTypeVar "γ" $
        extendContextWithTypeVar "β" mempty


testDesugar = TestLabel "desugar" $ numberedTestList [

    TestCase $ assertEqual "Desugaring \\α : *.\\x : α.x"
      ( Right $ TmPoly "α" ( TmAbs "x" ( TpVar 0 ) ( TmVar 0 ) ) )
      ( desugar mempty ( TmsPoly "α" ( TmsAbs "x" ( TpsVar "α" ) ( TmsVar "x" ) ) ) ),

    TestCase $ assertEqual "Desugaring \\α : *.\\x : α.b"
      ( Right $ TmPoly "α" ( TmAbs "x" ( TpVar 0 ) ( TmVar 3 ) ) )
      ( desugar testContext ( TmsPoly "α" ( TmsAbs "x" ( TpsVar "α" ) ( TmsVar "b" ) ) ) ),

    TestCase $ assertEqual "Desugaring \\α : *.\\f : β -> α.f b"
      ( Right $ TmPoly "α" ( TmAbs "f" ( TpArrow ( TpVar 4 ) ( TpVar 0 ) ) ( TmApp ( TmVar 0 ) ( TmVar 3 ) ) ) )
      ( desugar testContext ( TmsPoly "α" ( TmsAbs "f" ( TpsArrow ( TpsVar "β" ) ( TpsVar "α" ) ) ( TmsApp ( TmsVar "f" ) ( TmsVar "b" ) ) ) ) )

  ]


testsPreprocess = TestList [
    testDesugar
  ]