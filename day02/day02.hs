import qualified Data.Text as T
import qualified Data.Text.Read as TR

goodPassword (minNb, maxNb, c, pwd) = cNb >= minNb && cNb <= maxNb
  where
    cNb = T.count (T.pack [c]) pwd

goodPassword2 (i1, i2, c, pwd) =
  case (goodChar c i1 pwd, goodChar c i2 pwd) of
    (True, False) -> True
    (False, True) -> True
    _ -> False
  where
    goodChar c i pwd = i <= T.length pwd && T.index pwd (i - 1) == c

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
    Just tests -> do
      putStr "First Part: "
      print $ length $ filter goodPassword tests
      putStr "Second Part: "
      print $ length $ filter goodPassword2 tests
    Nothing -> putStrLn "FAIL !"