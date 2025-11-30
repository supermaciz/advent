import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as Seq

type Coordinate = (Int, Int)

type Grid = Seq.Seq C.ByteString

data Square = Tree | Open | WrongCoor deriving (Eq, Show)

makeGrid :: C.ByteString -> Grid
makeGrid input = Seq.fromList $ C.lines input

makeNextCoor :: Coordinate -> Coordinate -> Coordinate
makeNextCoor (x, y) (i, j) = (x + i, y + j)

startCoor :: Coordinate
startCoor = (0, 0)

lineIndex :: C.ByteString -> Int -> Char
lineIndex grid i = C.index grid $ i `mod` C.length grid

squareType :: Grid -> Coordinate -> Square
squareType grid (x, y) =
  case (`lineIndex` x) <$> Seq.lookup y grid of
    Just '#' -> Tree
    Just '.' -> Open
    _ -> WrongCoor

countTrees :: Grid -> Coordinate -> (Coordinate -> Coordinate) -> Integer -> Integer
countTrees grid coor nextCoor nb =
  case squareType grid coor of
    Tree -> countTrees grid (nextCoor coor) nextCoor (nb + 1)
    Open -> countTrees grid (nextCoor coor) nextCoor nb
    WrongCoor -> nb

countTotalTrees :: Grid -> [Coordinate] -> Integer
countTotalTrees grid coors = product $ map countSlopeTrees coors
  where
    countSlopeTrees x = countTrees grid startCoor (makeNextCoor x) 0

main :: IO ()
main = do
  input <- C.readFile "input"
  let grid = makeGrid input
   in do
        putStr "Part 1: "
        print $ countTrees grid startCoor (makeNextCoor (3, 1)) 0
        putStr "\nPart 2: "
        print $ countTotalTrees grid [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
