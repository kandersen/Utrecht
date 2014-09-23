module Utrecht.MasterMind.Test(
  masterMindSuite
  ) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit 
import Test.Tasty.QuickCheck as QC
import Utrecht.MasterMind.Core

masterMindSuite :: TestTree
masterMindSuite = testGroup "MasterMind"
  [ examples,
    properties ]

examples :: TestTree
examples = testGroup "Examples from assignment" [
  testGroup "black count" [
    testCase "ex1" $
      black [1, 1, 2, 2] [3, 4, 6, 6] @?= 0,
    testCase "ex2" $
      black [5, 1, 1, 4] [5, 4, 1, 1] @?= 2],
  testGroup "white count" [
    testCase "ex1" $
      white [1, 1, 2, 2] [3, 4, 6, 6] @?= 0,
    testCase "ex2" $
      white [5, 1, 1, 4] [5, 4, 1, 1] @?= 2],
  testGroup "check" [
    testCase "ex1" $
      check [5, 1, 6, 2] [5, 2, 1, 6] @?= (1, 3, False),
    testCase "ex2" $
      check [5, 1, 6, 2] [5, 1, 6, 2] @?= (4, 0, True)],
  testGroup "report" $ [
    testCase "not done" $
      report (check [5, 1, 6, 2] [5, 2, 1, 6]) @?= "1 black, 3 white",
    testCase "done" $
      report (check [5, 1, 6, 2] [5, 1, 6, 2]) @?= "4 black, 0 white\nCongratulations."]
  ]

properties :: TestTree
properties = testGroup "Properties" [
  QC.testProperty "QC: black + white <= Width" $
    forAll genCode $ \guess solution ->
      black guess solution + 
      white guess solution <= width,
  QC.testProperty "QC: black guess guess == width" $
    forAll genCode $ \guess ->
      black guess guess == width
  ]

genCode :: Gen [Int]
genCode = QC.vectorOf width $ elements [1..colors]