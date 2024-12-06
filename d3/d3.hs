-- {-# OPTIONS_GHC -Wall #-}

import Data.List (foldl')
import Debug.Trace (traceShowId)
import Text.Regex.Posix

main :: IO ()
main = do
  input <- readFile "input.txt"
  let pattern = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"
  let matchSets = input =~ pattern :: [[String]]
  print $ sum [read (matchSet !! 1) * read (matchSet !! 2) | matchSet <- matchSets]
  solve3'

data Activity = On | Off

data Instruction
  = Activate
  | Deactivate
  | Multiply (Integer, Integer)
  deriving (Show)

compile :: [String] -> Instruction
compile [] = Deactivate
compile xs = case xs of
  "do" : _ -> Activate
  "dont'" : _ -> Deactivate
  "mul" : a : b : _ -> Multiply (read a, read b)
  _ : _ -> Deactivate

solve3' :: IO ()
solve3' = do
  input <- readFile "input.txt"
  let pattern = "(mul)\\(([0-9]{1,3}),([0-9]{1,3})\\)|(don't)\\(\\)|(do)\\(\\)"
  let matchSets = input =~ pattern :: [[String]]
  let cleanSteps = compile . tail . filter (/= mempty) <$> matchSets
  let (activityStatus, total) = foldl' go (On, 0) cleanSteps
  print total
  where
    go :: (Activity, Integer) -> Instruction -> (Activity, Integer)
    go (a, i) Activate = (On, i)
    go (a, i) Deactivate = (Off, i)
    go (On, i) (Multiply (x, y)) = (On, i + x * y)
    go (a, i) _ = (a, i)
