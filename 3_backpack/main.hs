import Data.Char

main = do
  contents <- readFile "input"
  print $ part1 contents

part1 =
  sum . map charToNum . map (strToChar . commonLetters) . map halve . lines

part2 = sum . map charToNum . map threesomeSquasher . splitInto3 . lines

halve list = splitAt (length list `div` 2) list

splitInto3 [] = []
splitInto3 list = (take 3 list) : (splitInto3 $ drop 3 list)

strToChar string = head string

threesomeSquasher [str1, str2, str3] =
  strToChar $ commonLetters ((commonLetters (str1, str2)), str3)

commonLetters ("", str2) = ""
commonLetters (str1, str2) =
  case (elem (head str1) str2) of
    True -> [head str1] ++ (commonLetters (tail str1, str2))
    False -> "" ++ (commonLetters (tail str1, str2))

charToNum char
  | isUpper char = (ord char) - 38
  | otherwise = (ord char) - 96
