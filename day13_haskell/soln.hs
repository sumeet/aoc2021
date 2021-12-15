{-# LANGUAGE LambdaCase #-}

import Control.Lens (ix, set)
import Data.Either (partitionEithers)
import Data.List.Extra (intercalate, splitOn)
import Data.Maybe (catMaybes)
import Maybes (mapMaybe)

data Fold = FoldX Int | FoldY Int deriving (Show)

data Parsed = Dot Int Int | Fold Fold deriving (Show)

dumpGrid :: [[Bool]] -> String
dumpGrid = intercalate "\n" . map (map toChar)
  where
    toChar b = if b then '#' else '.'

superimpose :: [[Bool]] -> [[Bool]] -> [[Bool]]
superimpose = zipWith $ zipWith (||)

two :: [a] -> (a, a)
two [x, y] = (x, y)
two _ = error "expectd 2-element tuple"

update :: Int -> Int -> a -> [[a]] -> [[a]]
update x y = set (ix y . ix x)

fold :: Fold -> [[Bool]] -> [[Bool]]
fold (FoldY y) = foldY y
fold (FoldX x) = foldX x

foldY :: Int -> [[Bool]] -> [[Bool]]
foldY y grid = superimpose top $ reverse bottom
  where
    (top, bottom) = splitAt y grid

foldX :: Int -> [[Bool]] -> [[Bool]]
foldX x grid = superimpose left $ map reverse right
  where
    (left, right) = unzip $ map (splitAt x) grid

parse :: String -> Parsed
parse s
  | ',' `elem` s = uncurry Dot $ two $ map read $ splitOn "," s
  | foldOver == 'x' = Fold $ FoldX foldNum
  | otherwise = Fold $ FoldY foldNum
  where
    foldOver = s !! 11
    foldNum = read $ drop 13 s

separate :: [Parsed] -> ([(Int, Int)], [Fold])
separate =
  partitionEithers
    . map (\case (Dot x y) -> Left (x, y); (Fold f) -> Right f)

main :: IO ()
main = do
  s <- readFile "./input"
  let parsed = map parse $ filter (not . null) $ lines s
  let (dots, folds) = separate parsed
  let maxX = maximum $ map fst dots
  let maxY = maximum $ map snd dots
  let emptyGrid = replicate (maxY + 1) $ replicate (maxX + 1) False
  let startGrid = foldl (\nextGrid (x, y) -> update x y True nextGrid) emptyGrid dots
  let folded = fold (head folds) startGrid
  --putStrLn $ dumpGrid folded
  print $ sum $ concatMap (map fromEnum) folded
  pure ()