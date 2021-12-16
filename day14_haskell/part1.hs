{-# LANGUAGE LambdaCase #-}

import Data.List.Extra (splitOn)
import Data.Map (Map, fromList, (!?))
import GhcPlugins (only)
import Test.LeanCheck.Stats (counts)

type Rules = Map (Char, Char) Char

two :: [a] -> (a, a)
two [x, y] = (x, y)
two _ = error "expected 2-element tuple"

parseRules :: [String] -> Rules
parseRules ss = fromList $ map parseRule ss
  where
    parseRule s =
      let (els, to) = two $ splitOn " -> " s
       in (two els, only to)

windows :: [a] -> [(a, a)]
windows (x : xs@(x' : _)) = (x, x') : windows xs
windows _ = []

doSubs :: Rules -> String -> String
doSubs rules template =
  concatMap
    ( \t@(a, b) -> case rules !? t of
        Nothing -> [a]
        Just s -> [a, s]
    )
    (windows template)
    ++ [last template]

main :: IO ()
main = do
  inputLines <- lines <$> readFile "./sample"
  let template = head inputLines
  let rules = parseRules $ drop 2 inputLines
  let result = iterate (doSubs rules) template !! 10
  let elCounts = map snd $ counts result
  print $ maximum elCounts - minimum elCounts