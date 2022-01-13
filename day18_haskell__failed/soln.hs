{-# LANGUAGE PatternSynonyms #-}

import Control.Monad (foldM, replicateM)
import Data.Bifunctor (first, second)
import Data.Char (digitToInt, isDigit)
import Data.Either.Extra (fromLeft')
import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShowId)

data Pair = Scalar Int | Pair Pair Pair deriving (Show, Eq)

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

data Reduced a = Exploded (a, Maybe Int, Maybe Int) | Contained a deriving (Show)

instance Functor Reduced where
  fmap f (Exploded (a, n, m)) = Exploded (f a, n, m)
  fmap f (Contained a) = Contained $ f a

comb :: Reduced Pair -> Reduced Pair -> Reduced Pair
comb (Contained a) (Contained b) = Contained $ Pair a b
comb (Contained a) (Exploded (b, n, m)) = Exploded (Pair a b, m, n)
comb (Exploded (a, n, m)) (Contained b) = Exploded (Pair a b, n, m)
comb (Exploded (a, an, am)) (Exploded (b, bn, bm)) = Exploded (Pair a b, an, bm) -- not sure if this is correct

type ReductionMaybe a = Either (Reduced a) a

pattern Reduced :: Reduced a -> ReductionMaybe a
pattern Reduced a = Left a

pattern Unmodified :: a -> ReductionMaybe a
pattern Unmodified a = Right a

{-# COMPLETE Reduced, Unmodified #-}

applyReductionL :: Reduced Pair -> Pair -> Reduced Pair
applyReductionL (Exploded (a, Nothing, Just m)) (Scalar b) =
  Contained $ Pair a $ Scalar (b + m)
applyReductionL (Exploded (a, n, Just m)) (Scalar b) =
  Exploded (Pair a (Scalar (b + m)), n, Nothing)
applyReductionL (Exploded (a, Just n, Nothing)) (Scalar b) =
  Exploded (Pair a $ Scalar b, Just n, Nothing)
applyReductionL (Exploded (a, Nothing, Nothing)) b = Contained $ Pair a b
applyReductionL (Exploded (a, n, m)) (Pair ba bb) =
  comb (Exploded (a, n, Nothing)) $ applyReductionR ba (Exploded (bb, m, Nothing))
applyReductionL (Contained a) b = Contained $ Pair a b

applyReductionR :: Pair -> Reduced Pair -> Reduced Pair
applyReductionR (Scalar a) (Exploded (b, Just n, Nothing)) =
  Contained $ Pair (Scalar $ a + n) b
applyReductionR (Scalar a) (Exploded (b, Just n, Just m)) =
  Exploded (Pair (Scalar $ a + n) b, Nothing, Just m)
applyReductionR (Scalar a) (Exploded (b, Nothing, Just m)) =
  Exploded (Pair (Scalar a) b, Nothing, Just m)
applyReductionR a (Exploded (b, Nothing, Nothing)) = Contained $ Pair a b
applyReductionR (Pair aa ab) (Exploded (b, n, m)) =
  comb (applyReductionL (Exploded (aa, Nothing, n)) ab) (Exploded (b, m, Nothing))
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
  | otherwise = Just $ Exploded (Scalar 0, Just a, Just b)
tryExplode n _ = Nothing

divRoundUp :: Int -> Int -> Int
divRoundUp a b = (a + b - 1) `div` b

trySplit :: Pair -> Maybe (Reduced Pair)
trySplit (Scalar a)
  | a >= 10 = Just $ Contained $ Pair (Scalar (a `div` 2)) $ Scalar (a `divRoundUp` 2)
  | otherwise = Nothing
trySplit _ = Nothing

fromReduced :: ReductionMaybe a -> a
fromReduced (Unmodified a) = a
fromReduced (Reduced (Contained a)) = a
fromReduced (Reduced (Exploded (a, n, m))) = error "not reduced"

traceWith :: (a -> String) -> a -> a
traceWith f x = trace (f x) x

t :: Show a => String -> a -> a
t s = traceWith (\x -> s ++ ": " ++ show x)

untilStable :: Eq a => (a -> a) -> a -> a
untilStable f x = if x == x' then x else untilStable f x'
  where
    x' = f x

addApplyReductions :: Pair -> Pair -> Pair
addApplyReductions a b =
  untilStable
    (fromReduced . reduce 0 . traceWith dump)
    $ Pair a b

main :: IO ()
main = do
  pairss <- map parseLine . lines <$> readFile "./sample"
  putStrLn $ dump $ foldl1 addApplyReductions pairss

-- main :: IO ()
-- main = do
--   let pair = parseLine "[7,[6,[5,[4,[3,2]]]]]"
--   putStrLn $ dump pair
--   print $ reduce 0 pair
--   putStrLn $ dump $ fromReduced $ reduce 0 pair