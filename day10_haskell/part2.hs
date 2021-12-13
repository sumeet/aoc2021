import Control.Monad (foldM)
import Data.Either (lefts, rights)
import Data.List (find, sort)

scoreClosing :: String -> Int
scoreClosing = foldl (\acc c -> (acc * 5) + scoreOne c) 0
  where
    scoreOne c = case c of
      ')' -> 1
      ']' -> 2
      '}' -> 3
      '>' -> 4
      bad -> error $ "wtf, tried to score " ++ [bad]

close :: Char -> Char
close c = case c of
  '(' -> ')'
  '[' -> ']'
  '<' -> '>'
  '{' -> '}'
  bad -> error $ "wtf, tried to close " ++ [bad]

closeOpenStack :: String -> String
closeOpenStack = map close . reverse

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
  let scoreCode = foldM parse []
  let allScores = sort $ map (scoreClosing . map close) $ rights $ map scoreCode codes
  print $ allScores !! (length allScores `div` 2)