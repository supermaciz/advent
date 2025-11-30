module Main where

convertInput :: String -> [Int]
convertInput input = map (\x -> read x :: Int) $ lines input

makeCombinations :: [Int] -> [(Int, Int, Int)]
makeCombinations nbs = [(x, y, z) | x <- nbs, y <- nbs, z <- nbs]

goodNumbers :: (Int, Int, Int) -> Bool
goodNumbers (x, y, z) = x + y + z == 2020

isNotDuplicate :: (Int, Int, Int) -> Bool
isNotDuplicate (x, y, z) = x /= y && y /= z

main :: IO ()
main = do
  input <- readFile "input"
  let cs = makeCombinations $ convertInput input
   in let (a, b, c) = head $ dropWhile (not . goodNumbers) $ filter isNotDuplicate cs
       in print (a * b * c)
