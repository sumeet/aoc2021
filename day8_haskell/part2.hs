import Data.List (find, intersperse)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Set (Set, delete, difference, empty, fromList, member, notMember, toList, union)
import Debug.Trace (traceShowId)
import Util (only)

countIt :: String -> Bool
countIt n = length n `elem` [4, 3, 7, 2]

unscrambleConfig :: [Set Char] -> [Set Char]
unscrambleConfig config = [one, two, three, four, five, six, seven, eight, nine, zero]
  where
    -- 1 len(2) =>     c     f
    one = fromJust $ find ((== 2) . length) config
    -- 4 len(4) =>   b c d   f
    four = fromJust $ find ((== 4) . length) config
    -- 7 len(3) => a   c     f
    seven = fromJust $ find ((== 3) . length) config
    -- 8 len(7) => a b c d e f g
    eight = fromJust $ find ((== 7) . length) config
    a = only $ toList $ seven `difference` one
    bOrD = four `difference` one
    -- all length 6s have b but not d
    allSixes = filter ((== 6) . length) config
    b = only $ filter (\c -> all (c `member`) allSixes) (toList bOrD)
    d = only $ toList $ delete b bOrD
    zero = only $ filter (d `notMember`) allSixes

    -- not done yet
    two = fromList [b]
    three = fromList [d]
    five = empty
    six = empty
    nine = empty

parseLine :: String -> ([Set Char], [Set Char])
parseLine line = (toSets config, toSets output)
  where
    toSets = map fromList . splitOn " "
    [config, output] = splitOn " | " line

--
--
-- 1 len(2) =>     c     f
-- 7 len(3) => a   c     f
-- 4 len(4) =>   b c d   f
-- 8 len(7) => a b c d e f g
--
-- 2 len(5) => a   c d e   g
-- 3 len(5) => a   c d   f g
-- 5 len(5) => a b   d   f g
-- 0 len(6) => a b c   e f g
-- 6 len(6) => a b   d e f g
-- 9 len(6) => a b c d   f g

main :: IO ()
main = do
  s <- readFile "./sample"
  let messages = map parseLine $ lines s
  print $ map (unscrambleConfig . fst) messages