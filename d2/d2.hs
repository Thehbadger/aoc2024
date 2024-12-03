{-# OPTIONS_GHC -Wall #-}

main :: IO ()
main = do
  input <- readFile "input"
  let reports = parse input
  print input

type Level = Int

type Report = [Level]

data Direction
  = Increasing
  | Decreasing
  | Hold
  | Unk
  deriving (Eq)

getDirection :: Int -> Int -> (Direction, Int)
getDirection x y
  | x == y = (Hold, 0)
  | x > y = (Decreasing, x - y)
  | x < y = (Increasing, y - x)

parse :: String -> [Report]
parse = map (map (read :: String -> Level) . words) . lines

isSafe :: Report -> Direction -> Bool
isSafe [x, xs] d -- Last two of the list
  | direction == Hold = False
  | 
  where
    (direction, value) = getDirection x xs

isSafe (x : xs : xxs) Unk

isSafe (x : xs : xxs) d
  | direction == Hold = False
  | value < 3 = isSafe (xs : xxs) direction
  where
    (direction, value) = getDirection x xs
