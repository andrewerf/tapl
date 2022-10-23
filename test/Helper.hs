module Helper
  (
  numberedTestList
  )
where

import Test.HUnit

numberedTestList :: [Test] -> Test
numberedTestList = TestList . go 0
  where
    go :: Int -> [Test] -> [Test]
    go i ( test : tests ) = TestLabel ( show i ) test : go ( i + 1 ) tests
    go _ _ = []