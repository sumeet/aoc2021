import Control.Monad.Trans.Reader (Reader, ask, asks)
import Data.Char (digitToInt)
import Data.List.Extra ((!?))
import Data.Maybe (fromMaybe, mapMaybe)

get :: Int -> Int -> [[a]] -> Maybe a
get x y grid = do
  row <- grid !? y
  row !? x

adjacentCells :: Int -> Int -> [[Int]] -> [Int]
adjacentCells x y grid = do
  let points =
        [ (x + dx, y + dy)
          | dx <- [-1, 0, 1],
            dy <- [-1, 0, 1],
            (dx, dy) /= (0, 0)
        ]
   in mapMaybe (\(x, y) -> get x y grid) points

withAdjacents :: [[Int]] -> [(Int, [Int])]
withAdjacents grid =
  concat $
    [ [ (cell, adjacentCells x y grid)
        | (x, cell) <- zip [0 ..] row
      ]
      | (y, row) <- zip [0 ..] grid
    ]

main :: IO ()
main = do
  s <- readFile "./input"
  let grid = map (map digitToInt) $ lines s
  print $ sum $ map (\(n, adjs) -> if all (n <) adjs then n + 1 else 0) $ withAdjacents grid