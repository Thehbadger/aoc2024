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

isSafe :: [(Direction, Int)] -> Bool
isSafe =

-- | Looks at the values and creates a list 
createDirectionList :: Report -> [(Direction, Int)]
createDirectionList [] = []
createDirectionList [_] = []
createDirectionList [x, xs] = [getDirection x xs]
createDirectionList (x : xs : xxs) = getDirection x xs : createDirectionList (xs : xxs)
