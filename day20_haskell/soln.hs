{-# LANGUAGE LambdaCase #-}

import Data.Bits (Bits (shiftL))
import Data.List (intercalate)
import Data.List.Extra ((!?))
import Data.Maybe (fromMaybe)

data Px = On | Off deriving (Show, Eq)

toBin :: Px -> Int
toBin On = 1
toBin Off = 0

type Algo = [Px]

type Grid = [[Px]]

count :: Grid -> Int
count = sum . concatMap (map toBin)

nEnhancements :: Int -> Algo -> Grid -> Grid
nEnhancements n algo grid = foldl (flip ($)) padded (take n enhances)
  where
    enhances =
      map (`enhance` algo) $
        cycle $
          if head algo == On then [Off, On] else [Off]
    padded = iterate pad grid !! (n + 3)

main :: IO ()
main = do
  inputLines <- lines <$> readFile "./sample"
  let algo = parseAlgo $ head inputLines
  let grid = parseGrid $ drop 2 inputLines
  let part1 = nEnhancements 2 algo grid
  putStrLn "Part 1:"
  print $ count part1
  let part2 = nEnhancements 50 algo grid
  putStrLn "Part 2:"
  print $ count part2

dump :: Grid -> String
dump = intercalate "\n" . map (map dumpCh)

dumpCh :: Px -> Char
dumpCh On = '#'
dumpCh Off = '.'

enhance :: Px -> Algo -> Grid -> Grid
enhance dflt algo grid =
  map
    ( \(y, row) ->
        map (\x -> calc $ nbors x y dflt) row
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

neighbors :: Grid -> Int -> Int -> Px -> [Px]
neighbors grid x y dflt =
  map
    ( \(dx, dy) ->
        fromMaybe dflt $ (!? (dx + x)) =<< grid !? (dy + y)
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