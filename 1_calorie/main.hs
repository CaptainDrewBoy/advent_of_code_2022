import Data.List
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  putStrLn $ highest contents
  hClose handle

highest = show . maximum . chunksum . lines

highest_three = show . sum . take 3 . reverse . sort . chunksum . lines

-- Takes a list, then takes elems until it reaches an empty str, THEN it sums those and adds that to a new list
chunksum [] = []
chunksum list =
  (sum $ map read $ takeWhile (/= "") list) : (chunksum $ tailDrop list)

-- This way we avoid attempting to tail an empty list
tailDrop list
  | (dropWhile (/= "") list) == [] = []
  | otherwise = tail $ dropWhile (/= "") list
