module Test.Lambda2.Core.AST
  (
  testsAST
  )
where

import Test.HUnit

import Helper
import Lambda2.Core.AST


testContext = extendContextWithVar "a" ( TpVar 0 ) $ extendContextWithTypeVar "A" mempty

testGetVarIndex = TestLabel "getVarIndex" $ numberedTestList [
    TestCase $ assertEqual ""
      ( Just 1 )
      ( getVarIndex testContext "A" ),
    TestCase $ assertEqual ""
      ( Just 0 )
      ( getVarIndex testContext "a" )
  ]

testGetType = TestLabel "getType" $ numberedTestList [
    TestCase $ assertEqual ""
      ( Just $ TpVar 1 )
      ( getType testContext "a" ),
    TestCase $ assertEqual ""
      Nothing
      ( getType testContext "A" ),
    TestCase $ assertEqual ""
      Nothing
      ( getType testContext "c" )
  ]

testGetVar = TestLabel "getVar" $ numberedTestList [
    TestCase $ assertEqual ""
      ( Just ( "a", TpVar 1 ) )
      ( getVar testContext 0 )
  ]


testsAST = TestList [
  testGetVarIndex,
  testGetType,
  testGetVar
  ]
