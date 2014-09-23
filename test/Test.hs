module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)

import Utrecht.MasterMind.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [
	masterMindSuite
  ]