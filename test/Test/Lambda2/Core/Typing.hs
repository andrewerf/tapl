module Test.Lambda2.Core.Typing
  (
  testsTyping
  )
where

import Test.HUnit

import Helper
import Lambda2.Core.AST
import Lambda2.Core.Typing


makeBoundVar i = TmVar ( BoundVar i )
makeBoundVarSimple i = TmsVar ( BoundVar i )

makeTailAbs v tp t = TmAbs v tp ( TailAbs t )
makeTailAbsSimple v tp t = TmsAbs v tp ( TailAbs t )


-- TODO: write DSL for lambda in Haskell with usage of Free monad

-- Γ = β:*, γ:*, b:β, c:γ
testContext :: Context
testContext =
  extendContextWithVar "c" ( TpVar 1 ) $
    extendContextWithVar "b" ( TpVar 1 ) $
      extendContextWithTypeVar "γ" $
        extendContextWithTypeVar "β" mempty


testTypeOf = TestLabel "typeof" $ numberedTestList [

    TestCase $ assertEqual "Type of c is γ"
      ( Right $ TpVar 2 )
      ( typeof testContext ( makeBoundVar 0 ) ),

    TestCase $ assertEqual "Type of \\x : β. x is β -> β"
      ( Right $ TpArrow ( TpVar 3 ) ( TpVar 3 ) )
      ( typeof testContext ( makeTailAbs "x" ( TpVar 3 ) ( makeBoundVar 0 ) ) ),

    TestCase $ assertEqual "Type of \\f : β -> γ.\\x : β.f x is ( β -> γ ) -> β -> γ"
      ( Right $ TpArrow ( TpArrow ( TpVar 3 ) ( TpVar 2 ) ) ( TpArrow ( TpVar 3 ) ( TpVar 2 ) ) )
      ( typeof testContext ( makeTailAbs "f" ( TpArrow ( TpVar 3 ) ( TpVar 2 ) ) ( makeTailAbs "x" ( TpVar 4 ) ( TmApp ( makeBoundVar 1 ) ( makeBoundVar 0 ) ) ) ) ),

    TestCase $ assertEqual "Type of \\f : β -> γ.\\x : β.f b is ( β -> γ ) -> β -> γ"
      ( Right $ TpArrow ( TpArrow ( TpVar 3 ) ( TpVar 2 ) ) ( TpArrow ( TpVar 3 ) ( TpVar 2 ) ) )
      ( typeof testContext ( makeTailAbs "f" ( TpArrow ( TpVar 3 ) ( TpVar 2 ) ) ( makeTailAbs "x" ( TpVar 4 ) ( TmApp ( makeBoundVar 1 ) ( makeBoundVar 3 ) ) ) ) ),

    TestCase $ assertEqual "Type of \\δ : *.\\x : δ.x is Πδ:*.δ -> δ"
      ( Right $ TpPoly "δ" ( TpArrow ( TpVar 0 ) ( TpVar 0 ) ) )
      ( typeof testContext ( TmPoly "δ" ( makeTailAbs "x" ( TpVar 0 ) ( makeBoundVar 0 ) ) ) ),

    TestCase $ assertEqual "Type of \\x : β.\\δ : *.x is β -> Πδ:*.β"
      ( Right $ TpArrow ( TpVar 3 ) ( TpPoly "δ" ( TpVar 4 ) ) )
      ( typeof testContext ( makeTailAbs "x" ( TpVar 3 ) ( TmPoly "δ" ( makeBoundVar 1 ) ) ) ),

    TestCase $ assertEqual "Type of ( \\δ : *.\\x : δ. x ) β is β -> β"
      ( Right $ TpArrow ( TpVar 3 ) ( TpVar 3 ) )
      ( typeof testContext ( TmApp ( TmPoly "δ" ( makeTailAbs "x" ( TpVar 0 ) ( makeBoundVar 0 ) ) ) ( TmType ( TpVar 3 ) ) ) ),

    TestCase $ assertEqual "Type of ( \\δ : *.\\τ : *.\\x : δ. x ) ( β -> γ ) is Πτ:*.( β -> γ ) -> ( β -> γ )"
      ( Right $ TpPoly "τ"
        ( TpArrow
          ( TpArrow ( TpVar 4 ) ( TpVar 3 ) )
          ( TpArrow ( TpVar 4 ) ( TpVar 3 ) ) ) )
      ( typeof testContext ( TmApp
        ( TmPoly "δ" ( TmPoly "τ" ( makeTailAbs "x" ( TpVar 1 ) ( makeBoundVar 0 ) ) ) )
        ( TmType ( TpArrow ( TpVar 3 ) ( TpVar 2 ) ) ) ) )

  ]


testsTyping = TestList [
  testTypeOf
  ]