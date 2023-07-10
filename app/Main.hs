module Main (main) where

import Lib

main :: IO ()
main = do 
    n <- run "2" solve2a
    print n
