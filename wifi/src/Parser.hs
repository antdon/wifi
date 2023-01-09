-- |

module Parser where


split :: String -> Char -> [String]
split x = split' x ""
split' (x:xs) current delimeter
  | x == delimeter && current == "" = split' xs current delimeter
  | x == delimeter = current : split' xs "" delimeter
  | otherwise = split' xs (current ++ [x]) delimeter
split' x current _
  | current /= "" = [current]
  | otherwise = []

lineSeperated input = split input '\n'
spaceSeperated input = split input ' '
justNames input = map head $ map spaceSeperated $ tail (lineSeperated input)

-- returns the fifth word of a sentence
fifthWord :: String -> String
fifthWord = head . drop 4 . words


stringify :: [[Char]] -> [Char]
stringify (x1:x2:xs) = x1 ++ "\n" ++ x2 ++ "\n" ++ stringify xs
stringify (x:xs) = x ++ stringify xs
stringify [] = []
