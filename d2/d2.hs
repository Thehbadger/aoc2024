{-# OPTIONS_GHC -Wall #-}

import Data.List
import Debug.Trace

main :: IO ()
main = do
  input <- readFile "input"
  print (parse input)
  print (parse' input)

type Level = Integer

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

-- parse :: String -> Int
-- parse = sum . map (fromEnum . isSafe . createDirectionList) . convertLinesToReports . lines

parse :: String -> Int
parse = countTrue . map (isSafe' . map (read :: String -> Integer) . words) . lines
  where
    countTrue = length . filter (&& True)

parse' :: String -> Int
parse' = countTrue . map (isSafeWithDampening . map (read :: String -> Integer) . words) . lines
  where
    countTrue = length . filter (&& True)

-- parse = sum . map (fromEnum . isSafe') . convertLinesToReports . lines

-- convertLinesToReports :: [String] -> [Integer]
-- convertLinesToReports = map (map (read :: String -> Integer) . words)

-- isSafe :: [(Direction, Int)] -> Bool
-- isSafe [] = True -- End of items end
-- isSafe [(x, y)] -- Last Item
--   | (x /= Hold) && (y <= 3) = True
--   | otherwise = False
-- isSafe ((Hold, _) : _) = False -- Found a Hold return
-- isSafe ((Unk, y1) : x2) -- Start
--   | y1 <= 3 = isSafe x2
--   | otherwise = False
-- isSafe ((x1, y1) : i2@(x2, _) : xs)
--   | (x1 == Hold) || (x2 == Hold) = False
--   | (x1 == x2) && (y1 <= 3) = isSafe (i2 : xs)
--   | otherwise = False

-- -- | Looks at the values and creates a list
-- createDirectionList :: Report -> [(Direction, Int)]
-- createDirectionList [] = []
-- createDirectionList [_] = []
-- createDirectionList [x, xs] = [getDirection x xs]
-- createDirectionList (x : xs : xxs) = getDirection x xs : createDirectionList (xs : xxs)

-- | Stolen implementation :)
isSafe' :: [Integer] -> Bool
isSafe' levels = (levels `goSameWay`) && (levels `areStable`)
  where
    differencesIn levels = zipWith (-) levels (tail levels)

    goSameWay :: [Integer] -> Bool
    goSameWay levels = (levels == sort levels) || (levels == reverse (sort levels))

    areStable :: [Integer] -> Bool
    areStable levels = and [abs eachDifference `elem` [1 .. 3] | eachDifference <- differencesIn levels]

isSafeWithDampening :: [Integer] -> Bool
isSafeWithDampening level = isSafe' `any` attempts
  where
    attempts = zipWith (++) (inits level) (tail $ tails level)
