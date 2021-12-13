import Data.Char (digitToInt)
import Data.List (intercalate)
import Data.List.Extra ((!?))
import Data.Maybe (catMaybes)

dumpGrid :: [[Int]] -> IO ()
dumpGrid rows = do
  let x = map (map show) rows
  putStrLn $ intercalate "\n" $ map (intercalate "") x

neighbors :: [[Int]] -> Int -> Int -> [Int]
neighbors grid x y =
  catMaybes
    [ grid !? (y + dy) >>= (!? (x + dx))
      | dx <- [-1, 0, 1],
        dy <- [-1, 0, 1],
        (dx, dy) /= (0, 0)
    ]

nextGrid :: [[Int]] -> [[Int]]
nextGrid grid =
  [ [ nextCell x y
      | (x, cell) <- zip [0 ..] row
    ]
    | (y, row) <- zip [0 ..] grid
  ]
  where
    nextCell x y =
      let current = (grid !! y) !! x
          next' = current + 1
          next'' = next' + length (filter (== 9) $ neighbors grid x y)
       in if next'' > 9 then 0 else next''

main :: IO ()
main = do
  s <- readFile "./sample"
  let grid = map (map digitToInt) $ lines s
  dumpGrid grid
  putStrLn ""
  dumpGrid $ nextGrid grid
  putStrLn ""
  dumpGrid $ nextGrid $ nextGrid grid