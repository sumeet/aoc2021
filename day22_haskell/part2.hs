{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (find, findIndex, sort)
import Data.List.Extra (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShowId)

data Grid = Grid {instructions :: [Instruction], totalVolume :: Int}

data Range3D = Range3D
  { xRange :: (Int, Int),
    yRange :: (Int, Int),
    zRange :: (Int, Int)
  }
  deriving (Show)

volume :: Range3D -> Int
volume (Range3D (x1, x2) (y1, y2) (z1, z2)) = abs (x2 - x1) * abs (y2 - y1) * abs (z2 - z1)

data Instruction = Instruction {onOrOff :: Bool, range :: Range3D} deriving (Show)

rangeIntersect :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
rangeIntersect (x1, x2) (y1, y2)
  | x1 > y2 || x2 < y1 = Nothing
  | otherwise = Just (max x1 y1, min x2 y2)

intersect3D :: Range3D -> Range3D -> Maybe Range3D
intersect3D (Range3D (x1, x2) (y1, y2) (z1, z2)) (Range3D (x3, x4) (y3, y4) (z3, z4)) = do
  let xRange = rangeIntersect (x1, x2) (x3, x4)
  let yRange = rangeIntersect (y1, y2) (y3, y4)
  let zRange = rangeIntersect (z1, z2) (z3, z4)
  Range3D <$> xRange <*> yRange <*> zRange

runInstruction :: Grid -> Instruction -> Grid
runInstruction Grid {instructions, totalVolume} inst@Instruction {onOrOff = True, range} =
  Grid
    { instructions = instructions ++ [inst],
      totalVolume = totalVolume + volumeDiff
    }
  where
    volumeDiff =
      foldl
        ( \acc prevInst@Instruction {onOrOff = prevOnOrOff, range = prevRange} ->
            if prevOnOrOff then acc - maybe 0 volume (intersect3D range prevRange) else acc
        )
        (volume range)
        instructions
runInstruction Grid {instructions, totalVolume} inst@Instruction {onOrOff = False, range} = undefined

parseRangeString :: String -> (Int, Int)
parseRangeString s = (a, b)
  where
    [a, b] = sort $ map read $ splitOn ".." $ drop 2 s

parseLine :: String -> Instruction
parseLine s =
  Instruction
    { onOrOff = onOrOff,
      range = Range3D {xRange = x, yRange = y, zRange = z}
    }
  where
    onOrOff = head (words s) == "on"
    [x, y, z] = map parseRangeString ranges
    ranges = splitOn "," $ words s !! 1

main :: IO ()
main = do
  s <- readFile "smallsample2"
  let ls = lines s
  let instructions = map parseLine ls
  -- let grid = foldl runInstruction [] instructions
  -- let numOn = sum $ Map.filter (== True) grid
  -- print numOn
  print ""