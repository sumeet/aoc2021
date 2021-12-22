{-# LANGUAGE LambdaCase #-}

import Data.Bits (Bits (shiftL))
import Data.List (intercalate)
import Data.List.Extra ((!?))
import Data.Maybe (fromMaybe)

data Px = On | Off deriving (Show)

toBin :: Px -> Int
toBin On = 1
toBin Off = 0

type Algo = [Px]

type Grid = [[Px]]

count :: Grid -> Int
count = sum . concatMap (map toBin)

main :: IO ()
main = do
  inputLines <- lines <$> readFile "./input"
  let algo = parseAlgo $ head inputLines
  let origGrid = parseGrid $ drop 2 inputLines
  let padded = (pad . pad . pad . pad . pad) origGrid
  let enhanced = enhance (enhance padded algo) algo
  putStrLn $ dump $ enhance (enhance padded algo) algo
  -- 4 is hax for subtracting out the corners
  print $ count enhanced - 4

dump :: Grid -> String
dump = intercalate "\n" . map (map dumpCh)

dumpCh :: Px -> Char
dumpCh On = '#'
dumpCh Off = '.'

enhance :: Grid -> Algo -> Grid
enhance grid algo =
  map
    ( \(y, row) ->
        map (\x -> calc $ nbors x y) row
    )
    gridIndices
  where
    gridIndices = zip [0 ..] $ replicate numRows [0 .. rowLength - 1]
    numRows = length grid
    rowLength = length $ head grid
    nbors = neighbors grid
    calc = calcEnhance algo

calcEnhance :: Algo -> [Px] -> Px
calcEnhance algo lookup = algo !! toDec lookup

pad :: Grid -> Grid
pad grid = [paddedRow] ++ map padRow grid ++ [paddedRow]
  where
    paddedRow = replicate rowLen Off
    rowLen = 2 + length (head grid)
    padRow row = [Off] ++ row ++ [Off]

neighbors :: Grid -> Int -> Int -> [Px]
neighbors grid x y =
  map
    ( \(dx, dy) ->
        fromMaybe Off $ (!? (dx + x)) =<< grid !? (dy + y)
    )
    toLook
  where
    toLook =
      [ (-1, -1),
        (0, -1),
        (1, -1),
        (-1, 0),
        (0, 0),
        (1, 0),
        (-1, 1),
        (0, 1),
        (1, 1)
      ]

toDec :: [Px] -> Int
toDec = foldl (\acc px -> (acc `shiftL` 1) + toBin px) 0

parseAlgo :: String -> Algo
parseAlgo = map parsePx

parseGrid :: [String] -> Grid
parseGrid = map $ map parsePx

parsePx :: Char -> Px
parsePx '#' = On
parsePx '.' = Off
parsePx or = error ("unexpected: " ++ [or])