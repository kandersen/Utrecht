module Main where

import Utrecht.MasterMind.Core

main :: IO ()
main = do
  s <- generateSolution -- initialization
  loop s  

