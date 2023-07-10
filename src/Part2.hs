module Part2 (solve2a, solve2b) where

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

data Turn = Turn
  { yourMove :: Move,
    theirMove :: Move
  }

data Outcome = Win | Loss | Tie

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
score turn =
  mvScore + case outcome turn of
    Win -> 6
    Tie -> 3
    Loss -> 0
  where
    mvScore = moveScore $ yourMove turn

parseLine :: String -> Turn
parseLine line = Turn you them
  where
    w = words line
    them = parseTheirMove $ head w
    you = case w !! 1 of
      "X" -> Rock
      "Y" -> Paper
      _ -> Scissors

solve2a :: String -> Int
solve2a input = sum $ map (score . parseLine) (lines input)

solve2b :: String -> Int
solve2b input = sum $ map (score . turnB . parseLineB) (lines input)
  where
    parseLineB line = (you, them)
      where
        w = words line
        them = parseTheirMove $ head w
        you = case w !! 1 of
          "X" -> Loss
          "Y" -> Tie
          _ -> Win
    turnB (out, move) = Turn yourMove move
      where
        yourMove = case out of
          Win -> case move of
            Rock -> Paper
            Paper -> Scissors
            Scissors -> Rock
          Loss -> case move of
            Rock -> Scissors
            Paper -> Rock
            Scissors -> Paper
          Tie -> move
