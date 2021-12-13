{-# LANGUAGE TupleSections #-}

import Control.Monad.Trans.Reader (Reader, ask, asks)
import Data.Char (digitToInt)
import Data.List (nub, partition, sort)
import Data.List.Extra ((!?))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (fromList, member)

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
            if nextVal /= 9 && nextVal > val then Just nextPoint else Nothing
        )
        $ adjacentCells x y grid

adjacentCells :: Int -> Int -> [[Int]] -> [(Int, (Int, Int))]
adjacentCells x y grid = mapMaybe (\(x, y) -> (,(x, y)) <$> get x y grid) points
  where
    points =
      [(x + dx, y + dy) | (dx, dy) <- [(-1, 0), (0, 1), (1, 0), (0, -1)]]

withAdjacents :: [[Int]] -> [(Int, [Int], (Int, Int))]
withAdjacents grid =
  concat $
    [ [ (cell, map fst $ adjacentCells x y grid, (x, y))
        | (x, cell) <- zip [0 ..] row
      ]
      | (y, row) <- zip [0 ..] grid
    ]

mergeBasins :: [[(Int, Int)]] -> [[(Int, Int)]]
mergeBasins (thisBasin : basins) = nub (thisBasin ++ concat hasCommon) : mergeBasins notHasCommon
  where
    (hasCommon, notHasCommon) = partition (any (`member` thisBasinSet)) basins
    thisBasinSet = fromList thisBasin
mergeBasins [] = []

main :: IO ()
main = do
  s <- readFile "./input"
  let grid = map (map digitToInt) $ lines s
  let lowPoints =
        mapMaybe
          (\(n, adjs, (x, y)) -> if all (n <) adjs then Just (x, y) else Nothing)
          $ withAdjacents grid
  let basinPointss = map (uncurry (basin grid)) lowPoints
  let basinSizes = map length basinPointss
  print basinSizes
  print $ product $ take 3 $ reverse $ sort basinSizes