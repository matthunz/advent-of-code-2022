module Part1
  ( solve1a,
    solve1b,
  )
where

import Data.List (sort)

solve1a :: String -> Int
solve1a input = maximum $ readCalories input

solve1b :: String -> Int
solve1b = sum . take 3 . reverse . sort <$> readCalories

readCalories :: String -> [Int]
readCalories input = calculateCalories (lines input)
  where
    calculateCalories fileLines = x : xs
      where
        (x, xs) = foldr f (0, []) fileLines
        f line (total, elves)
          | line /= "" = (total + read line, elves)
          | otherwise = (0, total : elves)
