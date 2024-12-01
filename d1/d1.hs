import Data.List
import System.IO

main = do
  contents <- readFile "input.txt"
  putStrLn (show (parse contents))

type LocationId = Int

-- 1. Parse the list into two lists of ints
parse :: String -> Int
parse = sum . createDistanceList . sortTupleList . splitAlternating . map (read :: String -> LocationId) . words

-- | Splits a list into two sub-lists based on every other.
splitAlternating xs =
  let evens = [x | (x, i) <- zip xs [0 ..], even i]
      odds = [x | (x, i) <- zip xs [0 ..], odd i]
   in (evens, odds)

sortTupleList :: ([LocationId], [LocationId]) -> ([LocationId], [LocationId])
sortTupleList (l1, l2) = (sort l1, sort l2)

createDistanceList :: ([LocationId], [LocationId]) -> [Int]
createDistanceList (l1, l2) = map abs (zipWith (-) l1 l2)

-- 2. Take that list and sort them
-- 3. Zip the lists together
-- 4. Get Distance in a new list
-- 5. Sum list together and get total
