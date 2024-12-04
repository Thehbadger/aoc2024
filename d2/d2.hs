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
parse = sum . map (fromEnum . isSafe . createDirectionList) . convertLinesToReports . lines

convertLinesToReports :: [String] -> [Report]
convertLinesToReports = map (map (read :: String -> Level) . words)

isSafe :: [(Direction, Int)] -> Bool
isSafe [] = True -- End of items end
isSafe [(x, y)] -- Last Item
  | (x /= Hold) && (y <= 3) = True
  | otherwise = False
isSafe ((Hold, _) : _) = False -- Found a Hold return
isSafe ((Unk, y1) : x2) -- Start
  | y1 <= 3 = isSafe x2
  | otherwise = False
isSafe ((x1, y1) : i2@(x2, _) : xs)
  | (x1 == Hold) || (x2 == Hold) = False
  | (x1 == x2) && (y1 <= 3) = isSafe (i2 : xs)
  | otherwise = False

-- | Looks at the values and creates a list
createDirectionList :: Report -> [(Direction, Int)]
createDirectionList [] = []
createDirectionList [_] = []
createDirectionList [x, xs] = [getDirection x xs]
createDirectionList (x : xs : xxs) = getDirection x xs : createDirectionList (xs : xxs)
