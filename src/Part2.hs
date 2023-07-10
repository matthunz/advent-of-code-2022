module Part2 (solve2a) where

import System.IO

data Move = Rock | Paper | Scissors

moveScore :: Move -> Int
moveScore move = case move of
  Rock -> 1
  Paper -> 2
  Scissors -> 3

parseTheirMove :: String -> Move
parseTheirMove line = case line of
  "A" -> Rock
  "B" -> Paper
  _ -> Scissors

parseYourMove :: String -> Move
parseYourMove line = case line of
  "X" -> Rock
  "Y" -> Paper
  _ -> Scissors

data Turn = Turn
  { yourMove :: Move,
    theirMove :: Move
  }

data Outcome = Win | Loss | Tie

outcomeScore :: Outcome -> Int
outcomeScore o = case o of
  Win -> 6
  Tie -> 3
  Loss -> 0

outcome :: Turn -> Outcome
outcome turn = case turn of
  Turn Rock Paper -> Loss
  Turn Rock Scissors -> Win
  Turn Paper Rock -> Win
  Turn Paper Scissors -> Loss
  Turn Scissors Rock -> Loss
  Turn Scissors Paper -> Win
  _ -> Tie

score :: Turn -> Int
score turn = mvScore + outcomeScore (outcome turn)
  where
    mvScore = moveScore $ yourMove turn

parseLine :: String -> Turn
parseLine line = Turn you them
  where
    w = words line
    them = parseTheirMove $ head w
    you = parseYourMove $ w !! 1

solve2a :: String -> Int
solve2a input = sum $ map (score . parseLine) (lines input)
