import Data.Char (isUpper)
import Data.List (nub, partition)
import Data.List.Extra (splitOn)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import Util (singleton)

type CaveMap = Map String [String]

to2Tuple :: [a] -> (a, a)
to2Tuple [x, y] = (x, y)
to2Tuple _ = error "expected 2-element list"

nextDests :: CaveMap -> [String] -> Maybe [String]
nextDests caveMap prevDests =
  filter
    ( \dest ->
        isUpper (head dest) || dest `notElem` prevDests
    )
    <$> (caveMap !? prev)
  where
    prev = last prevDests

parseCaveMap :: String -> CaveMap
parseCaveMap s =
  Map.fromListWith (++) $
    concatMap
      ( \(src, dest) ->
          [(src, [dest]) | src /= "end"]
            ++ ([(dest, [src]) | dest /= "end"])
      )
      paths
  where
    paths = map (to2Tuple . splitOn "-") $ lines s

allPaths :: CaveMap -> [[String]]
allPaths caveMap =
  fst $
    until
      (\(_, ongoing) -> null ongoing)
      ( \(ended, ongoing) ->
          let nextPaths = map (\path -> (path, next path)) ongoing
              (nextEnded, nextOngoing) =
                partition
                  (\(_, nexts) -> isNothing nexts)
                  $ map (\path -> (path, next path)) ongoing
           in ( ended ++ map fst nextEnded,
                concatMap
                  ( \(dests, finals) ->
                      map ((dests ++) . singleton) (fromJust finals)
                  )
                  nextOngoing
              )
      )
      ([], [["start"]])
  where
    next = nextDests caveMap

main :: IO ()
main = do
  s <- readFile "./input"
  let caveMap = parseCaveMap s
  print $ length $ nub $ allPaths caveMap
