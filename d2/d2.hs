{-# OPTIONS_GHC -Wall #-}

main :: IO ()
main = do
  input <- readFile "input"
  let reports = parse input
  print reports

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
  | x > y = (Decreasing, x - y)
  | x < y = (Increasing, y - x)
  | otherwise = (Hold, 0)

parse :: String -> Int
parse = sum . map (fromEnum . (`isSafe` Unk) . map (read :: String -> Level) . words) . lines

isSafe :: Report -> Direction -> Bool
isSafe [x, xs] d -- Last two of the list
  | direction == Hold = False
  | (value <= 3) && (direction == d) = True
  | otherwise = False
  where
    (direction, value) = getDirection x xs
isSafe (x : xs : xxs) Unk -- Starting offfffffff
  | value <= 3 = isSafe (xs : xxs) direction
  | otherwise = False
  where
    (direction, value) = getDirection x xs
isSafe (x : xs : xxs) d -- Normal Pattern
  | direction == Hold = False
  | (value <= 3) && (d == direction) = isSafe (xs : xxs) direction
  | otherwise = False
  where
    (direction, value) = getDirection x xs
isSafe _ _ = False
