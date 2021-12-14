import Data.Char (digitToInt)
import Data.List (intercalate)
import Data.List.Extra ((!?))
import Data.Maybe (catMaybes)
import Debug.Trace (trace, traceShowId)

untilStable :: Eq a => (a -> a) -> a -> a
untilStable f x = if x == x' then x else untilStable f x'
  where
    x' = f x

dumpGrid :: [[Int]] -> String
dumpGrid rows = intercalate "\n" (map (intercalate "") rowStrings) ++ "\n"
  where
    rowStrings = map (map print) rows
    print n = if n == -1 then "-" else show n

neighbors :: [[Int]] -> Int -> Int -> [Int]
neighbors grid x y =
  catMaybes
    [ grid !? (y + dy) >>= (!? (x + dx))
      | dx <- [-1, 0, 1],
        dy <- [-1, 0, 1],
        (dx, dy) /= (0, 0)
    ]

incrementStep :: [[Int]] -> [[Int]]
incrementStep grid = [[cell + 1 | cell <- row] | row <- grid]

flashingStep :: ([[Int]], Int) -> ([[Int]], Int)
flashingStep (grid, prevCount) =
  ( [ [ nextCell x y thisCell
        | (x, thisCell) <- zip [0 ..] row
      ]
      | (y, row) <- zip [0 ..] grid
    ],
    prevCount + length (filter (== -1) $ concat grid)
  )
  where
    nextCell x y thisCell =
      if thisCell `elem` [0, -1]
        then 0
        else
          let nbors = neighbors grid x y
              numFlashingNeighbors = length (filter (== -1) nbors)
              next = thisCell + numFlashingNeighbors
           in if next > 9 then -1 else next

nextGrid :: ([[Int]], Int) -> ([[Int]], Int)
nextGrid (grid, prevCount) =
  let grid' = incrementStep grid
   in untilStable flashingStep (grid', prevCount)

traceWith :: (a -> String) -> a -> a
traceWith f x = trace (f x) x

main :: IO ()
main = do
  s <- readFile "./input"
  let grid = map (map digitToInt) $ lines s
  print $ snd (iterate nextGrid (grid, 0) !! 100)