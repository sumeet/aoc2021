{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad (foldM)
import Data.Either (lefts)
import Data.List (find)

parse :: [Char] -> Char -> Either Int [Char]
parse stack c@'(' = Right $ c : stack
parse stack c@'[' = Right $ c : stack
parse stack c@'{' = Right $ c : stack
parse stack c@'<' = Right $ c : stack
parse stack@(top : rest) c = case c of
  ')' -> if top == '(' then Right rest else Left 3
  ']' -> if top == '[' then Right rest else Left 57
  '}' -> if top == '{' then Right rest else Left 1197
  '>' -> if top == '<' then Right rest else Left 25137
  c -> error $ "unreachable! " ++ [c]
parse badstack badc = error $ "unreachable! " ++ show badstack ++ show badc

main :: IO ()
main = do
  s <- readFile "./input"
  let codes = lines s
  let scoreCode code = foldM parse [] code
  print $ sum $ lefts $ map scoreCode codes