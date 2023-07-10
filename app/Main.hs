module Main (main) where

import Lib
import Part2 (solve2b)

main :: IO ()
main = do 
    n <- run "2" solve2b
    print n
