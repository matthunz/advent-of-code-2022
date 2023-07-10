module Lib
  ( solve1a,
    solve1b,
    solve2a,
    solve1b,
    run
  )
where

import Part1
import Part2
import System.IO

run :: String -> (String -> Int) -> IO Int
run name f = do
  handle <- openFile ("input/" ++ name) ReadMode
  contents <- hGetContents handle
  pure $ f contents
