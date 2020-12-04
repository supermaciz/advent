module Main where

main :: IO ()
main = do
  input <- readFile "input"
  let numbers = convertInput input
   in print $ result numbers

convertInput :: String -> [Integer]
convertInput input = map (\x -> read x :: Integer) $ lines input

compute :: Integer -> [Integer] -> Maybe Integer
compute _ [] = Nothing
compute value (x : rest)
  | value + x == 2020 = Just $ value * x
  | otherwise = compute value rest

result :: [Integer] -> Integer
result (nb : numbers) =
  case compute nb numbers of
    Nothing -> result numbers
    Just x -> x
