import Control.Monad (unless)
import Lib
import Part2 (solve2b)

test :: String -> Int -> IO Int -> IO ()
test name expected f = do
  res <- f
  unless (res == expected) (error "part 1")

main :: IO ()
main = do
  test "part1a" 69281 (run "1" solve1a)
  test "part1b" 201524 (run "1" solve1b)
  test "part2a" 14069 (run "2" solve2a)
  test "part2b" 12411 (run "2" solve2b)
