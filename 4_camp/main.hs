import Data.List (intersect)
import qualified Data.Text as T

main = do
  contents <- readFile "input"
  print (part2 contents)

part1 = sum . map fullOverlap . bothParts

part2 = sum . map anyOverlap . bothParts

bothParts = map (map rangeUnwrap) . map (splitStr ",") . lines

-- Wrapping Text things in a function so I don't lose my mind
splitStr :: String -> String -> [String]
splitStr delimiter str =
  map T.unpack $ T.splitOn (T.pack delimiter) (T.pack str)

rangeUnwrap :: String -> [Int]
rangeUnwrap str = [x .. y]
  where
    x = read (head $ splitStr "-" str) :: Int
    y = read (last $ splitStr "-" str) :: Int

-- There is a full overlap == return 1
-- There isn't == return 0
fullOverlap [x, y]
  | (x `intersect` y == x) || (x `intersect` y == y) = 1
  | otherwise = 0

anyOverlap [x, y]
  | (x `intersect` y /= []) = 1
  | otherwise = 0
