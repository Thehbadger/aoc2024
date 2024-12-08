import Data.List
import Debug.Trace (traceShowId)
import Text.Regex.PCRE

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print (part1 input)
  print (part2 input)

subArrays :: Int -> [[a]] -> [[[a]]]
subArrays n xss = [[take n t | t <- tails xs] | xs <- xss]

genDiagonal :: [[a]] -> [[a]]
genDiagonal =
  (++)
    <$> reverse . transpose . zipWith drop [0 ..]
    <*> transpose . zipWith drop [1 ..] . transpose

part1 :: [[Char]] -> Int
part1 grid =
  (sum . concatMap countXmas)
    [ grid,
      transpose grid,
      genDiagonal grid,
      genDiagonal $ map reverse grid
    ]
  where
    countXmas :: [[Char]] -> [Int]
    countXmas = map (length . filter ((||) <$> isPrefixOf "XMAS" <*> isPrefixOf "SAMX") . tails)

part2 :: [[Char]] -> Int
part2 grid =
  let groups = concat $ subArrays 3 $ transpose $ subArrays 3 grid
   in length $ filter check groups
  where
    check :: [[Char]] -> Bool
    check
      [ [a, _, b],
        [_, 'A', _],
        [c, _, d]
        ] = elem [a, d] ["MS", "SM"] && elem [b, c] ["MS", "SM"]
    check _ = False
