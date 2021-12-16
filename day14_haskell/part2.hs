{-# LANGUAGE LambdaCase #-}

import Data.List.Extra (splitOn)
import Data.Map (Map, fromList, fromListWith, toList, (!?))
import GhcPlugins (only)
import Test.LeanCheck.Stats (counts)

type Rules = Map (Char, Char) Char

type Pairs = Map (Char, Char) Int

two :: [a] -> (a, a)
two [x, y] = (x, y)
two _ = error "expected 2-element tuple"

parseTemplate :: String -> Pairs
parseTemplate = fromList . counts . windows

parseRules :: [String] -> Rules
parseRules ss = fromList $ map parseRule ss
  where
    parseRule s =
      let (els, to) = two $ splitOn " -> " s
       in (two els, only to)

windows :: [a] -> [(a, a)]
windows (x : xs@(x' : _)) = (x, x') : windows xs
windows _ = []

doSubs :: Rules -> Pairs -> Pairs
doSubs rules pairs =
  fromListWith (+) $
    concatMap
      ( \(pair@(a, b), count) -> case rules !? pair of
          Nothing -> [(pair, count)]
          Just s -> [((a, s), count), ((s, b), count)]
      )
      $ toList pairs

halfRoundUp :: Int -> Int
halfRoundUp x = if even x then quot else quot + 1
  where
    quot = x `div` 2

sumEmUp :: Pairs -> Map Char Int
sumEmUp pairs =
  fromListWith (+) $
    concatMap (\((a, b), count) -> [(a, count), (b, count)]) $ toList pairs

main :: IO ()
main = do
  inputLines <- lines <$> readFile "./input"
  let pairs = parseTemplate $ head inputLines
  let rules = parseRules $ drop 2 inputLines
  let result = iterate (doSubs rules) pairs !! 40
  let elCounts = map snd $ toList $ sumEmUp result
  -- TODO (hahahaha)
  -- idk why we have to divide by 2, but it works ¯\_(ツ)_/¯
  print $ halfRoundUp (maximum elCounts) - halfRoundUp (minimum elCounts)