{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace, traceShowId)
import Maybes (firstJusts)

maxDice :: Int
maxDice = 100

maxBoardPos :: Int
maxBoardPos = 10

scoreIsWinning :: Int -> Bool
scoreIsWinning = (>= 1000)

newtype BoardPos = BoardPos Int deriving (Show)

score :: BoardPos -> Int
score (BoardPos n) = n

newtype Dice = Dice Int deriving (Show)

data Player = P1 | P2 deriving (Eq, Show)

data Game = Game
  { p1Pos :: BoardPos,
    p1Score :: Int,
    p2Pos :: BoardPos,
    p2Score :: Int,
    dice :: Dice,
    rollCount :: Int,
    turn :: Player
  }
  deriving (Show)

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
      dice = Dice 1,
      rollCount = 0,
      turn = P1
    }

doTurn :: Game -> Game
doTurn
  Game
    { p1Pos,
      p1Score,
      p2Pos,
      p2Score,
      dice,
      rollCount,
      turn
    } =
    Game
      { p1Pos = nextP1Pos,
        p1Score = nextP1Score,
        p2Pos = nextP2Pos,
        p2Score = nextP2Score,
        turn = nextTurn,
        rollCount = rollCount + 3,
        dice = nextDice
      }
    where
      (nextTurn, (nextP1Pos, nextP1Score), (nextP2Pos, nextP2Score)) =
        case turn of
          P1 -> (P2, next p1Pos p1Score, (p2Pos, p2Score))
          P2 -> (P1, (p1Pos, p1Score), next p2Pos p2Score)
      next curPos curScore =
        let nextPos = advance numAdvance curPos
         in (nextPos, curScore + score nextPos)
      (numAdvance, nextDice) = roll dice

advance :: Int -> BoardPos -> BoardPos
advance n (BoardPos cur) = BoardPos $ ((cur - 1 + n) `mod` maxBoardPos) + 1

toD :: Int -> Int
toD = (+ 1) . (`mod` maxDice)

roll :: Dice -> (Int, Dice)
roll (Dice n) =
  ( sum $ map addToDice [0, 1, 2],
    Dice $ addToDice 3
  )
  where
    addToDice m = ((m - 1) + n `mod` maxDice) + 1

losingScore :: Game -> Int
losingScore g@Game {p1Score, p2Score} = case fromJust $ winner g of
  P1 -> p2Score
  P2 -> p1Score

main :: IO ()
main = do
  -- let (p1Start, p2Start) = (BoardPos 4, BoardPos 8) -- sample
  let (p1Start, p2Start) = (BoardPos 8, BoardPos 5) -- input
  let startingGame = newGame p1Start p2Start
  let (endGame, wonPlayer) =
        fromJust $
          firstJusts $
            map (\g -> (g,) <$> winner g) $ iterate doTurn startingGame
  print $ losingScore endGame * rollCount endGame