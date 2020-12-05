import qualified Data.Text as T
import qualified Data.Text.Read as TR

goodPassword (minNb, maxNb, c, pwd) = cNb >= minNb && cNb <= maxNb
  where
    cNb = T.count (T.pack [c]) pwd

isBoundary '-' = True
isBoundary ' ' = True
isBoundary ':' = True
isBoundary _ = False

convert :: [T.Text] -> Maybe (Int, Int, Char, T.Text)
convert [minStr, maxStr, letter, pwd] =
  case (TR.decimal minStr, TR.decimal maxStr, T.head letter) of
    (Right (minNb, _), Right (maxNb, _), c) -> Just (minNb, maxNb, c, pwd)
    (_, _, _) -> Nothing

makeTestCases :: String -> Maybe [(Int, Int, Char, T.Text)]
makeTestCases =
  mapM (convert . filter (\x -> T.length x /= 0) . T.split isBoundary) . T.lines . T.pack

main = do
  input <- readFile "input"
  case makeTestCases input of
    Just tests -> print $ length $ filter goodPassword tests
    Nothing -> putStrLn "FAIL !"