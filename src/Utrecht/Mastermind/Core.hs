module Utrecht.MasterMind.Core where

import System.Random  -- for randoms
import System.IO      -- for hFlush
import Control.Applicative ((<$>), (<*>))

type Row = [Int]
type Guess = Row
type Solution = Row

colors :: Int
colors = 6

width :: Int
width  = 4

tODO :: a -> a
tODO = id

generateSolution :: IO [Int]
generateSolution = do
  g <- getStdGen
  let rs = take width (randoms g)
  return (map ((+1) . (`mod` colors)) rs)

-- The loop function is supposed to perform a single interaction. It
-- reads an input, compares it with the solution, produces output to
-- the user, and if the guess of the player was incorrect, loops.
loop :: Solution -> IO ()
loop s = do
  i <- input            -- read (and parse) the user input
  tODO (return ())


black, white :: Solution -> Guess -> Int
black solution guess = length . filter id $ zipWith (==) solution guess
white solution guess = tODO 0

check :: Solution -> Guess -> (Int, Int, Bool)
check solution guess = (blackScore, whiteScore, isDone)
  where
    blackScore = black solution guess
    whiteScore = white solution guess
    isDone     = blackScore == width

-- report is supposed to take the result of calling check, and
-- produces a descriptive string to report to the player.
report :: (Int, Int, Bool) -> String
report (blacks, whites, done) = 
  show blacks ++ " black, " ++ show whites ++ " white"
  ++ if done then "\nCongratulations." else ""

-- The function input is supposed to read a single guess from the
-- player. The first three instructions read a line. You're supposed
-- to (primitively) parse that line into a sequence of integers.
input :: IO Guess
input = do
  putStr "? "
  hFlush stdout -- ensure that the prompt is printed
  map read . words <$> getLine

-- The following function |readInt| is given and parses a single
-- integer from a string. It produces |-1| if the parse fails. You
-- can use it in |input|.
readInt :: String -> Int
readInt x = case reads x of
  [(n, "")] -> n
  _         -> -1

-- The function valid tests a guess for validity. This is a bonus
-- exercise, so you do not have to implement this function.
valid :: Guess -> Bool
valid guess = tODO True