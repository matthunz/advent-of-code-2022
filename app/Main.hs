module Main (main) where

import Lib
import Part3 (solve3a)

main :: IO ()
main = do 
    n <- run "3" solve3a
    print n
