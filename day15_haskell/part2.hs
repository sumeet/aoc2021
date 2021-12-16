{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (first)
import Data.Char (digitToInt)
import Data.List (intercalate, sortOn, transpose, zip5)
import Data.List.Extra ((!?))
import Data.Maybe (fromJust, mapMaybe)
import Data.PSQueue (Binding ((:->)))
import qualified Data.PSQueue as PSQ
import qualified Data.Set as Set
import Debug.Trace (trace, traceShowId)

dumpGrid :: [[Int]] -> String
dumpGrid = unlines . map (intercalate "" . map show)

newRisk :: Int -> Int -> Int
newRisk oldRisk factor = ((oldRisk - 1 + factor) `mod` 9) + 1

raise :: Int -> [[Int]] -> [[Int]]
raise factor = map (map (newRisk factor))

hugeGrid :: [[Int]] -> [[Int]]
hugeGrid grid =
  concat
    [ extendGridsRightward [raise 0 grid, raise 1 grid, raise 2 grid, raise 3 grid, raise 4 grid],
      extendGridsRightward [raise 1 grid, raise 2 grid, raise 3 grid, raise 4 grid, raise 5 grid],
      extendGridsRightward [raise 2 grid, raise 3 grid, raise 4 grid, raise 5 grid, raise 6 grid],
      extendGridsRightward [raise 3 grid, raise 4 grid, raise 5 grid, raise 6 grid, raise 7 grid],
      extendGridsRightward [raise 4 grid, raise 5 grid, raise 6 grid, raise 7 grid, raise 8 grid]
    ]

extendGridsRightward :: [[[a]]] -> [[a]]
extendGridsRightward grids = foldl (foldl (zipWith (++))) (head grids) [tail grids]

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

score :: [[Int]] -> (Int, (Int, Int)) -> (Int, Int)
score grid (risk, (x, y)) = (risk, distanceFromEnd grid (x, y))

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

type Q = PSQ.PSQ ((Int, Int), (Int, Int)) (Int, (Int, Int))

getMinNotInSet :: Set.Set (Int, Int) -> Q -> ((Int, (Int, Int)), Q)
getMinNotInSet seen q =
  if pos `Set.notMember` seen
    then (next, nextQ)
    else getMinNotInSet seen nextQ
  where
    (_ :-> next@(risk, pos), nextQ) = fromJust $ PSQ.minView q

insert :: [[Int]] -> Q -> (Int, (Int, Int)) -> Q
insert grid q item@(_, pos) = PSQ.insert (score grid item, pos) item q

searchUntilComplete :: Set.Set (Int, Int) -> [[Int]] -> Q -> (Int, (Int, Int))
searchUntilComplete seen grid q = case search grid next of
  Left nexts ->
    searchUntilComplete (Set.insert pos seen) grid $ foldl (insert grid) nextQ nexts
  Right completes -> head $ sortOn (score grid) completes
  where
    (next@(risk, pos), nextQ) = getMinNotInSet seen q

main :: IO ()
main = do
  s <- readFile "./input"
  let grid = hugeGrid $ map (map digitToInt) $ lines s
  print $ searchUntilComplete Set.empty grid $ insert grid PSQ.empty (0, (0, 0))