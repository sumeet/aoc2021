{-# LANGUAGE PatternSynonyms #-}

import Control.Monad (foldM, replicateM)
import Data.Bifunctor (first, second)
import Data.Char (digitToInt, isDigit)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)

data Pair = Scalar Int | Pair Pair Pair deriving (Show)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just a) b = Just a
orElse _ b = b

dump :: Pair -> String
dump (Scalar n) = show n
dump (Pair a b) = "[" ++ dump a ++ "," ++ dump b ++ "]"

parseLine :: String -> Pair
parseLine s = case parse s of
  (p, "") -> p
  (p, rem) -> error ("some was remainining: " ++ rem)

parse :: String -> (Pair, String)
parse s@(c : cs)
  | isDigit c = first Scalar $ parseNumber s
  | otherwise = first (uncurry Pair) $ parsePair s
parse [] = error "nothing to consume"

parsePair :: String -> ((Pair, Pair), String)
parsePair s =
  let (a, rest) = parse $ consume '[' s
      (b, rest') = second (consume ']') $ parse $ consume ',' rest
   in ((a, b), rest')

consume :: Char -> String -> String
consume c (c' : cs)
  | c == c' = cs
  | otherwise = error "not a match"
consume c [] = error "nothing to consume"

parseNumber :: String -> (Int, String)
parseNumber = first (foldl (\acc n -> acc * 10 + n) 0) . fromJust . many parseDigit

many :: (String -> Maybe (a, String)) -> String -> Maybe ([a], String)
many f s = case f s of
  Nothing -> Nothing
  Just (a, s') -> case many f s' of
    Nothing -> Just ([a], s')
    Just (as, s'') -> Just (a : as, s'')

parseDigit :: String -> Maybe (Int, String)
parseDigit (c : cs)
  | isDigit c = Just (digitToInt c, cs)
  | otherwise = Nothing
parseDigit [] = Nothing

type Reduction a = Either a a

pattern Reduced :: a -> Reduction a
pattern Reduced a = Left a

pattern Unmodified :: a -> Reduction a
pattern Unmodified a = Right a

{-# COMPLETE Reduced, Unmodified #-}

reduce :: Int -> Pair -> Reduction Pair
reduce _ (Scalar a) = Unmodified $ Scalar a
reduce n (Pair a b) = case nextReduce a of
  Reduced a' -> Reduced $ Pair a' b
  Unmodified a' -> Pair a' <$> nextReduce b
  where
    nextReduce el =
      fromMaybe
        (reduce (n + 1) el)
        $ Reduced <$> tryExplode n el `orElse` trySplit el

tryExplode :: Int -> Pair -> Maybe Pair
tryExplode _ (Scalar _) = Nothing
tryExplode n (Pair a b)
  | n < 3 = Nothing

trySplit :: Pair -> Maybe Pair
trySplit = error "not implemented"

main :: IO ()
main = do
  pairss <- map parseLine . lines <$> readFile "./sample"
  putStrLn $ unlines $ map dump pairss