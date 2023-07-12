module Part2 (solve2a, solve2b) where

data Move = Rock | Paper | Scissors

data Turn = Turn
  { yourMove :: Move,
    theirMove :: Move
  }

data Outcome = Win | Loss | Tie

score :: Turn -> Int
score turn =
  mvScore + case outcome of
    Win -> 6
    Tie -> 3
    Loss -> 0
  where
    mvScore = case yourMove turn of
      Rock -> 1
      Paper -> 2
      Scissors -> 3
    outcome = case turn of
      Turn Rock Paper -> Loss
      Turn Rock Scissors -> Win
      Turn Paper Rock -> Win
      Turn Paper Scissors -> Loss
      Turn Scissors Rock -> Loss
      Turn Scissors Paper -> Win
      _ -> Tie

solve :: (Move -> String -> Move) -> String -> Int
solve makeMove input = sum $ map (score . turnOf) (lines input)
  where
    turnOf line = Turn you them
      where
        w = words line
        them = case head w of
          "A" -> Rock
          "B" -> Paper
          _ -> Scissors
        you = makeMove them (w !! 1)

solve2a :: String -> Int
solve2a = solve makeMove
  where
    makeMove _ s = case s of
      "X" -> Rock
      "Y" -> Paper
      _ -> Scissors

solve2b :: String -> Int
solve2b = solve makeMove
  where
    makeMove them s = getMove (getOutcome s) them
    getOutcome s = case s of
      "X" -> Loss
      "Y" -> Tie
      _ -> Win
    getMove out move = yourMove
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
