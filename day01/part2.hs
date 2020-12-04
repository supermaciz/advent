module Main where

convertInput :: String -> [Int]
convertInput input = map (\x -> read x :: Int) $ lines input

makeCombinations :: [Int] -> [(Int, Int, Int)]
makeCombinations nbs = do
  x <- nbs
  y <- nbs
  z <- nbs
  return (x, y, z)

goodNumbers :: (Int, Int, Int) -> Bool
goodNumbers (x, y, z) = x + y + z == 2020

main :: IO ()
main = do
  input <- readFile "input"
  let cs = makeCombinations $ convertInput input
   in let (a, b, c) = head $ dropWhile (not . goodNumbers) $ filter (\(x, y, z) -> x /= y && y /= z) cs
       in print (a * b * c)
