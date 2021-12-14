import Data.Char (isLower, isUpper)
import Data.List (intercalate, nub, partition, sortOn)
import Data.List.Extra (anySame, splitOn)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set, fromList, toList)
import Util (singleton)

type CaveMap = Map String [String]

to2Tuple :: [a] -> (a, a)
to2Tuple [x, y] = (x, y)
to2Tuple _ = error "expected 2-element list"

type NextDests = CaveMap -> [String] -> Maybe [String]

nextDestsPart1 :: NextDests
nextDestsPart1 caveMap prevDests =
  filter
    ( \dest ->
        isUpper (head dest) || dest `notElem` prevDests
    )
    <$> (caveMap !? prev)
  where
    prev = last prevDests

nextDestsPart2 :: NextDests
nextDestsPart2 caveMap prevDests =
  filter
    ( \dest ->
        isUpper (head dest)
          || if containsLowercaseDup
            then dest `notElem` prevDests
            else timesInPrev dest < 2
    )
    <$> (caveMap !? prev)
  where
    containsLowercaseDup = anySame $ filter (isLower . head) prevDests
    timesInPrev d = length $ filter (== d) prevDests
    prev = last prevDests

parseCaveMap :: String -> CaveMap
parseCaveMap s =
  Map.fromListWith (++) $
    concatMap
      ( \(src, dest) ->
          [(src, [dest]) | src /= "end" && dest /= "start"]
            ++ ([(dest, [src]) | dest /= "end" && src /= "start"])
      )
      paths
  where
    paths = map (to2Tuple . splitOn "-") $ lines s

allPaths :: CaveMap -> NextDests -> [[String]]
allPaths caveMap nextDests =
  fst $
    until
      (\(_, ongoing) -> null ongoing)
      ( \(ended, ongoing) ->
          let (nextEnded, nextOngoing) =
                partition
                  (\(_, nexts) -> isNothing nexts)
                  $ map (\path -> (path, nextDests caveMap path)) ongoing
           in ( ended ++ map fst nextEnded,
                concatMap
                  ( \(dests, finals) ->
                      map ((dests ++) . singleton) (fromJust finals)
                  )
                  nextOngoing
              )
      )
      ([], [["start"]])

dumpPaths :: [[String]] -> String
dumpPaths paths = intercalate "\n" (map (intercalate ",") paths) ++ "\n"

main :: IO ()
main = do
  s <- readFile "./input"
  let caveMap = parseCaveMap s
  putStrLn "Part 1:"
  print $ length $ allPaths caveMap nextDestsPart1
  putStrLn "Part 2:"
  print $ length $ allPaths caveMap nextDestsPart2
