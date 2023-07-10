module Part1
  ( solve1a,
    solve1b,
  )
where

import Data.List (sort)
import System.IO

solve1a :: IO Int
solve1a = maximum <$> readCalories

solve1b :: IO Int
solve1b = sum . take 3 . reverse . sort <$> readCalories

readCalories :: IO [Int]
readCalories = do
  handle <- openFile "input/1" ReadMode
  contents <- hGetContents handle
  pure $ calculateCalories (lines contents)
  where
    calculateCalories fileLines = x : xs
      where
        (x, xs) = foldr f (0, []) fileLines
        f line (total, elves)
          | line /= "" = (total + read line, elves)
          | otherwise = (0, total : elves)
