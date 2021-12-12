import Data.List (intersperse)
import Data.List.Split (splitOn)

countIt :: String -> Bool
countIt n = length n `elem` [8, 4, 3, 7, 2]

main :: IO ()
main = do
  s <- readFile "./input"
  let digits = map ((!! 1) . splitOn " | ") $ lines s
  let digits' = concatMap (splitOn " ") digits
  print $ sum $ map (fromEnum . countIt) digits'