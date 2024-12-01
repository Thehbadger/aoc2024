{-# OPTIONS_GHC -Wall #-}

import Data.List
import Debug.Trace

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (parseDistance contents)
  print (findSimilarity contents)

type LocationId = Int

-- | 1. Parse the list into two lists of ints
-- 2. Take that list and sort them
-- 3. Zip the lists together
-- 4. Get Distance in a new list
-- 5. Sum list together and get total
parseDistance :: String -> Int
parseDistance = sum . createDistanceList . sortTupleList . splitAlternating . map (read :: String -> LocationId) . words

-- | Splits a list into two sub-lists based on every other.
splitAlternating :: [a] -> ([a], [a])
splitAlternating xs =
  let evens = [x | (x, i) <- zip xs [0 ..], even i]
      odds = [x | (x, i) <- zip xs [0 ..], odd i]
   in (evens, odds)

sortTupleList :: ([LocationId], [LocationId]) -> ([LocationId], [LocationId])
sortTupleList (l1, l2) = (sort l1, sort l2)

createDistanceList :: ([LocationId], [LocationId]) -> [Int]
createDistanceList (l1, l2) = map abs (zipWith (-) l1 l2)

type Index = Int

type Count = Int

data Node = Node Index Count

type NodeZip = ([Node], [Node])

emptyNodeZip :: NodeZip
emptyNodeZip = ([], [])

resetZipList :: NodeZip -> NodeZip
resetZipList (x, y) = (reverse y ++ x, [])

inputZipList :: LocationId -> NodeZip -> NodeZip
inputZipList locId ([], y) = (Node locId 1 : y, []) -- No match found, need to create a new node.
inputZipList locId (node@(Node nid count) : xs, y)
  | locId == nid = resetZipList (Node nid (count + 1) : xs, y) -- Found match
  | otherwise = inputZipList locId (xs, node : y) -- Next index

findSimilarity :: String -> Int
findSimilarity = sum . diffCountList . createCountLists . splitAlternating . map (read :: String -> LocationId) . words

-- | Take in a tuple with lists of locationIds and output the node counts for each
createCountLists :: ([LocationId], [LocationId]) -> ([LocationId], NodeZip)
createCountLists (x, y) = (x, countList y emptyNodeZip)

-- | Take in a list of locationids and create a list without duplicates with the count of each object.
countList :: [LocationId] -> NodeZip -> NodeZip
countList [] list = resetZipList list -- Out of Ids
countList (x : xs) list = countList xs (inputZipList x list) -- There is a object up topo

diffCountList :: ([LocationId], NodeZip) -> [Int]
diffCountList (index : x1s, y) = diffCountInner index y : diffCountList (x1s, y)
diffCountList (_, _) = []

diffCountInner :: Index -> NodeZip -> Int
diffCountInner _ ([], _) = 0 -- Not found in list
diffCountInner index (node@(Node nid count) : xs, y)
  | index == nid = index * count -- Found the match
  | otherwise = diffCountInner index (xs, node : y)

testSimilarity :: Int
testSimilarity = findSimilarity input
  where
    input =
      "3   4\n\
      \4   3\n\
      \2   5\n\
      \1   3\n\
      \3   9\n\
      \3   3"
