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
      extendContextWithTypeVar "γ" ( TpVar 0 ) KndStar $
        extendContextWithTypeVar "β" ( TpVar 0 ) KndStar mempty
        

makeBoundVar i = TmVar ( BoundVar i )
makeBoundVarSimple i = TmsVar ( BoundVar i )

makeTailAbs v tp t = TmAbs v tp ( TailAbs t )
makeTailAbsSimple v tp t = TmsAbs v tp ( TailAbs t )


testDesugar = TestLabel "desugar" $ numberedTestList [

    TestCase $ assertEqual "Desugaring \\α : *.\\x : α.x"
      ( Right $ TmPoly "α" KndStar ( makeTailAbs "x" ( TpVar 0 ) ( makeBoundVar 0 ) ) )
      ( desugar mempty ( TmsPoly "α" KndsStar ( makeTailAbsSimple "x" ( TpsVar "α" ) ( makeBoundVarSimple "x" ) ) ) ),

    TestCase $ assertEqual "Desugaring \\α : *.\\x : α.b"
      ( Right $ TmPoly "α" KndStar ( makeTailAbs "x" ( TpVar 0 ) ( makeBoundVar 3 ) ) )
      ( desugar testContext ( TmsPoly "α" KndsStar ( makeTailAbsSimple "x" ( TpsVar "α" ) ( makeBoundVarSimple "b" ) ) ) ),

    TestCase $ assertEqual "Desugaring \\α : *.\\f : β -> α.f b"
      ( Right $ TmPoly "α" KndStar ( makeTailAbs "f" ( TpArrow ( TpVar 4 ) ( TpVar 0 ) ) ( TmApp ( makeBoundVar 0 ) ( makeBoundVar 3 ) ) ) )
      ( desugar testContext ( TmsPoly "α" KndsStar ( makeTailAbsSimple "f" ( TpsArrow ( TpsVar "β" ) ( TpsVar "α" ) ) ( TmsApp ( makeBoundVarSimple "f" ) ( makeBoundVarSimple "b" ) ) ) ) )

  ]


testsPreprocess = TestList [
    testDesugar
  ]