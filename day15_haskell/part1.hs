{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (first)
import Data.Char (digitToInt)
import Data.List (sortOn)
import Data.List.Extra ((!?))
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Debug.Trace (trace, traceShowId)

type Cur = ((Int, Int), Int)

get :: Int -> Int -> [[a]] -> Maybe a
get x y grid = (!? x) =<< grid !? y

neighbors :: [[Int]] -> Int -> Int -> [(Int, (Int, Int))]
neighbors grid x y = mapMaybe (\(x, y) -> (,(x, y)) <$> get x y grid) points
  where
    points =
      [(x + dx, y + dy) | (dx, dy) <- [(-1, 0), (0, 1), (1, 0), (0, -1)]]

nextPaths :: [[Int]] -> (Int, (Int, Int)) -> [(Int, (Int, Int))]
nextPaths grid (risk, pos) = map (first (risk +)) $ uncurry (neighbors grid) pos

isComplete :: [[Int]] -> (Int, Int) -> Bool
isComplete grid (x, y) = y + 1 == length grid && x + 1 == length (head grid)

-- score :: [[Int]] -> (Int, (Int, Int)) -> Float
-- score grid (risk, (x, y)) = (2.0 * fromIntegral (distanceFromEnd grid (x, y)) :: Float) + fromIntegral risk

score :: [[Int]] -> (Int, (Int, Int)) -> (Int, Int)
score grid (risk, (x, y)) = (risk, distanceFromEnd grid (x, y))

--score grid (risk, (x, y)) = risk * max 1 (distanceFromEnd grid (x, y))

distanceFromEnd :: [[Int]] -> (Int, Int) -> Int
distanceFromEnd grid (x, y) =
  abs
    (y - (length grid - 1))
    + abs (x - (length (head grid) - 1))

traceWith :: (a -> String) -> a -> a
traceWith f x = trace (f x) x

appendScore grid = map (\p -> (score grid p, p))

search ::
  [[Int]] ->
  (Int, (Int, Int)) ->
  Either [(Int, (Int, Int))] [(Int, (Int, Int))]
search grid cur =
  if (not . null) completes
    then Right completes
    else Left nexts
  where
    completes = filter (isComplete grid . snd) nexts
    nexts = nextPaths grid cur

searchUntilComplete :: Set.Set (Int, Int) -> [[Int]] -> [(Int, (Int, Int))] -> (Int, (Int, Int))
searchUntilComplete seen grid (q : qs) = case search grid q of
  -- Left nexts -> searchUntilComplete grid $ traceWith (show . appendScore grid) $ sortOn (score grid) $ nexts ++ qs
  Left nexts ->
    searchUntilComplete (Set.insert (snd q) seen) grid $
      sortOn (score grid) $
        filter ((`Set.notMember` seen) . snd) nexts ++ qs
  Right completes -> head $ sortOn (score grid) completes
searchUntilComplete _ _ [] = error "no paths"

main :: IO ()
main = do
  s <- readFile "./input"
  let grid = map (map digitToInt) $ lines s
  print $ searchUntilComplete Set.empty grid [(0, (0, 0))]