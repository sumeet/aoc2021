{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (sort)
import Data.List.Extra (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShowId)

type Grid = Map (Int, Int, Int) Bool

data Instruction = Instruction
  { onOrOff :: Bool,
    xRange :: (Int, Int),
    yRange :: (Int, Int),
    zRange :: (Int, Int)
  }
  deriving (Show)

rangeIntersect :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
rangeIntersect (x1, x2) (y1, y2)
  | x1 > y2 || x2 < y1 = Nothing
  | otherwise = Just (max x1 y1, min x2 y2)

runInstruction :: Grid -> Instruction -> Grid
runInstruction g Instruction {onOrOff, xRange, yRange, zRange} =
  foldl
    ( \acc (x, y, z) ->
        Map.insert (x, y, z) onOrOff acc
    )
    g
    [ (x, y, z)
      | x <- [fst xRange .. snd xRange],
        y <- [fst yRange .. snd yRange],
        z <- [fst zRange .. snd zRange]
    ]

parseRangeString :: String -> Maybe (Int, Int)
parseRangeString s = rangeIntersect (-50, 50) (a, b)
  where
    [a, b] = sort $ map read $ splitOn ".." $ drop 2 s

parseLine :: String -> Maybe Instruction
parseLine s =
  ( \[x, y, z] ->
      Instruction
        { onOrOff = onOrOff,
          xRange = x,
          yRange = y,
          zRange = z
        }
  )
    <$> xyz
  where
    onOrOff = head (words s) == "on"
    xyz = mapM parseRangeString ranges
    ranges = splitOn "," $ words s !! 1

main :: IO ()
main = do
  s <- readFile "input"
  let ls = lines s
  let instructions = mapMaybe parseLine ls
  let grid = foldl runInstruction Map.empty instructions
  let numOn = Map.size $ Map.filter (== True) grid
  print numOn