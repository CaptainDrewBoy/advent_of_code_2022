main = do
  contents <- readFile "input"
  print $ totalscore (map words (lines contents))

totalscore input = foldl folder 0 input

folder score item =
  let thing = win_lose item
   in score + (points thing) + (rps thing)

points item
  | x == "X" = 1
  | x == "Y" = 2
  | x == "Z" = 3
  | otherwise = 0
  where
    x = last item

rps ["A", "X"] = 3
rps ["A", "Y"] = 6
rps ["A", "Z"] = 0
rps ["B", "X"] = 0
rps ["B", "Y"] = 3
rps ["B", "Z"] = 6
rps ["C", "X"] = 6
rps ["C", "Y"] = 0
rps ["C", "Z"] = 3

win_lose ["A", "X"] = ["A", "Z"]
win_lose ["A", "Y"] = ["A", "X"]
win_lose ["A", "Z"] = ["A", "Y"]
win_lose ["B", "X"] = ["B", "X"]
win_lose ["B", "Y"] = ["B", "Y"]
win_lose ["B", "Z"] = ["B", "Z"]
win_lose ["C", "X"] = ["C", "Y"]
win_lose ["C", "Y"] = ["C", "Z"]
win_lose ["C", "Z"] = ["C", "X"]
