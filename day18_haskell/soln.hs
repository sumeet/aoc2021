{-# LANGUAGE PatternSynonyms #-}

import Control.Monad (foldM, replicateM)
import Data.Bifunctor (first, second)
import Data.Char (digitToInt, isDigit)
import Data.Either.Extra (fromLeft')
import Data.Maybe (fromJust)

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

data Reduced a = Exploded (a, Int, Int) | Split (a, Int) | Contained a deriving (Show)

instance Functor Reduced where
  fmap f (Exploded (a, n, m)) = Exploded (f a, n, m)
  fmap f (Split (a, n)) = Split (f a, n)
  fmap f (Contained a) = Contained $ f a

type ReductionMaybe a = Either (Reduced a) a

pattern Reduced :: Reduced a -> ReductionMaybe a
pattern Reduced a = Left a

pattern Unmodified :: a -> ReductionMaybe a
pattern Unmodified a = Right a

{-# COMPLETE Reduced, Unmodified #-}

applyReductionL :: Reduced Pair -> Pair -> Reduced Pair
applyReductionL (Exploded (a, _, m)) (Scalar b') =
  Contained $ Pair a $ Scalar (b' + m)
applyReductionL (Exploded (a, n, m)) (Pair ba bb) =
  Pair a <$> applyReductionL (Exploded (ba, n, m)) bb
applyReductionL (Split (a, n)) b = Contained $ Pair a b
applyReductionL (Contained a) b = Contained $ Pair a b

applyReductionR :: Pair -> Reduced Pair -> Reduced Pair
applyReductionR (Scalar a') (Exploded (b, n, _)) =
  Contained $ Pair (Scalar (a' + n)) b
applyReductionR (Pair aa ab) (Exploded (b, n, m)) =
  flip Pair b <$> applyReductionR aa (Exploded (ab, n, m))
applyReductionR a (Split (b, n)) = Contained $ Pair a b
applyReductionR a (Contained b) = Contained $ Pair a b

reduce :: Int -> Pair -> ReductionMaybe Pair
reduce _ (Scalar a) = Unmodified $ Scalar a
reduce n (Pair a b) = case nextReduce a of
  Reduced reduced -> Reduced $ applyReductionL reduced b
  Unmodified a' ->
    ( case nextReduce b of
        Reduced reduced -> Reduced $ applyReductionR a' reduced
        Unmodified b' -> Unmodified (Pair a' b')
    )
  where
    nextReduce el =
      maybe
        (reduce (n + 1) el)
        Reduced
        (tryExplode n el `orElse` trySplit el)

tryExplode :: Int -> Pair -> Maybe (Reduced Pair)
tryExplode _ (Scalar _) = Nothing
tryExplode n (Pair (Scalar a) (Scalar b))
  | n < 3 = Nothing
  | otherwise = Just $ Exploded (Scalar 0, a, b)
tryExplode n _ = Nothing

trySplit :: Pair -> Maybe (Reduced Pair)
trySplit _ = Nothing

fromReduced :: ReductionMaybe a -> a
fromReduced (Unmodified a) = a
fromReduced (Reduced (Contained a)) = a
fromReduced (Reduced (Exploded (a, n, m))) = a
fromReduced (Reduced (Split (a, n))) = a

-- main :: IO ()
-- main = do
--   pairss <- map parseLine . lines <$> readFile "./sample"
--   putStrLn $ unlines $ map dump pairss

main :: IO ()
main = do
  let pair = parseLine "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
  putStrLn $ dump pair
  putStrLn $ dump $ fromReduced $ reduce 0 pair