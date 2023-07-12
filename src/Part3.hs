module Part3 (solve3a) where

import Data.Char
import Data.List (intersect, nub)

solve3a :: String -> Int
solve3a input = sum . map priority $ concatMap matches (lines input)
  where
    matches :: String -> String
    matches line = nub $ a `intersect` b
      where
        (a, b) = splitAt (length line `div` 2) line

    priority :: Char -> Int
    priority item
      | isAsciiLower item = fromEnum item - fromEnum 'a' + 1
      | otherwise = fromEnum item - fromEnum 'A' + 27
