{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing)
import Debug.Trace (trace, traceShowId)
import Maybes (firstJusts)

maxBoardPos :: Int
maxBoardPos = 10

scoreIsWinning :: Int -> Bool
scoreIsWinning = (>= 21)

newtype BoardPos = BoardPos Int deriving (Show, Ord, Eq)

score :: BoardPos -> Int
score (BoardPos n) = n

newtype Dice = Dice Int deriving (Show)

data Player = P1 | P2 deriving (Eq, Show, Ord)

type GameFrequencies = Map Game Int

data Game = Game
  { p1Pos :: BoardPos,
    p1Score :: Int,
    p2Pos :: BoardPos,
    p2Score :: Int,
    turn :: Player
  }
  deriving (Show, Ord, Eq)

t :: Show a => String -> a -> a
t s x = trace (s ++ ": " ++ show x) x

winner :: Game -> Maybe Player
winner Game {p1Score, p2Score}
  | scoreIsWinning p1Score = Just P1
  | scoreIsWinning p2Score = Just P2
  | otherwise = Nothing

newGame :: BoardPos -> BoardPos -> Game
newGame p1Start p2Start =
  Game
    { p1Pos = p1Start,
      p1Score = 0,
      p2Pos = p2Start,
      p2Score = 0,
      turn = P1
    }

updateFreqs :: Map Game Int -> Map Game Int
updateFreqs gameFreqs = Map.unionWith (+) next finished
  where
    next =
      Map.fromListWith (+) $
        concatMap (\(game, freq) -> [(nextGame, freq) | nextGame <- doTurn game]) $
          Map.toList ongoing
    (ongoing, finished) = Map.partitionWithKey (\g _ -> isNothing $ winner g) gameFreqs

doTurn :: Game -> [Game]
doTurn game =
  [ doOneTurn (x + y + z) game
    | x <- [1, 2, 3],
      y <- [1, 2, 3],
      z <- [1, 2, 3]
  ]

doOneTurn :: Int -> Game -> Game
doOneTurn
  numAdvance
  Game
    { p1Pos,
      p1Score,
      p2Pos,
      p2Score,
      turn
    } =
    Game
      { p1Pos = nextP1Pos,
        p1Score = nextP1Score,
        p2Pos = nextP2Pos,
        p2Score = nextP2Score,
        turn = nextTurn
      }
    where
      (nextTurn, (nextP1Pos, nextP1Score), (nextP2Pos, nextP2Score)) =
        case turn of
          P1 -> (P2, next p1Pos p1Score, (p2Pos, p2Score))
          P2 -> (P1, (p1Pos, p1Score), next p2Pos p2Score)
      next curPos curScore =
        let nextPos = advance numAdvance curPos
         in (nextPos, curScore + score nextPos)

advance :: Int -> BoardPos -> BoardPos
advance n (BoardPos cur) = BoardPos $ ((cur - 1 + n) `mod` maxBoardPos) + 1

untilStable :: Eq a => (a -> a) -> a -> a
untilStable f x = if x == x' then x else untilStable f x'
  where
    x' = f x

main :: IO ()
main = do
  -- let (p1Start, p2Start) = (BoardPos 4, BoardPos 8) -- sample
  let (p1Start, p2Start) = (BoardPos 8, BoardPos 5) -- input
  let init = Map.fromList [(newGame p1Start p2Start, 1)]
  let done = untilStable updateFreqs init
  let (p1Wins, p2Wins) = Map.partitionWithKey (\g _ -> winner g == Just P1) done
  let p1Sum = sum $ Map.elems p1Wins
  let p2Sum = sum $ Map.elems p2Wins
  print $ max p1Sum p2Sum