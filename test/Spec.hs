import Control.Monad (unless)
import Lib

test :: String -> Int -> IO Int -> IO ()
test name expected f = do
  res <- f
  unless (res == expected) (error "part 1")

main :: IO ()
main = do
  test "part1a" 69281 solve1a
  test "part1b" 201524 solve1b
