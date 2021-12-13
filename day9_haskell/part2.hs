{-# LANGUAGE TupleSections #-}

import Control.Monad.Trans.Reader (Reader, ask, asks)
import Data.Char (digitToInt)
import Data.List (nub, sort)
import Data.List.Extra ((!?))
import Data.Maybe (fromMaybe, mapMaybe)

get :: Int -> Int -> [[a]] -> Maybe a
get x y grid = do
  row <- grid !? y
  row !? x

basin :: [[Int]] -> Int -> Int -> [(Int, Int)]
basin grid x y = nub $ (x, y) : concatMap (uncurry $ basin grid) nextBasinPoints
  where
    val = (grid !! y) !! x
    nextBasinPoints =
      mapMaybe
        ( \(nextVal, nextPoint) ->
            if nextVal /= 9 && nextVal == val + 1 then Just nextPoint else Nothing
        )
        $ adjacentCells x y grid

adjacentCells :: Int -> Int -> [[Int]] -> [(Int, (Int, Int))]
adjacentCells x y grid = mapMaybe (\(x, y) -> (,(x, y)) <$> get x y grid) points
  where
    points =
      [ (x + dx, y + dy)
        | dx <- [-1, 0, 1],
          dy <- [-1, 0, 1],
          (dx, dy) /= (0, 0)
      ]

withAdjacents :: [[Int]] -> [(Int, [Int], (Int, Int))]
withAdjacents grid =
  concat $
    [ [ (cell, map fst $ adjacentCells x y grid, (x, y))
        | (x, cell) <- zip [0 ..] row
      ]
      | (y, row) <- zip [0 ..] grid
    ]

main :: IO ()
main = do
  s <- readFile "./input"
  let grid = map (map digitToInt) $ lines s
  let lowPoints =
        mapMaybe
          (\(n, adjs, (x, y)) -> if all (n <) adjs then Just (x, y) else Nothing)
          $ withAdjacents grid
  let basinSizes = map (length . uncurry (basin grid)) lowPoints
  print $ sort basinSizes

--print $ product $ take 3 $ reverse $ sort basinSizes