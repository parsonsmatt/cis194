{-# LANGUAGE FlexibleInstances #-}
module JoinList where
import Data.List
import Data.Monoid
import Scrabble
import Buffer
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

single :: a -> JoinList Size a
single = Single (Size 1)

-- Exercise #1:
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2


tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- Exercise #2:

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ x Empty = Nothing
indexJ x (Single s a)
    | x == 0    = Just a
    | otherwise = Nothing
indexJ x (Append m l r)
    | x > intFromSized m = Nothing
    | x < intFromTag l   = indexJ x l
    | otherwise          = indexJ (x - intFromTag l) r


intFromSized :: Sized a => a -> Int
intFromSized = getSize . size


intFromTag :: (Monoid m, Sized m) => JoinList m a -> Int
intFromTag = intFromSized . tag


dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ x (Single m a) 
    | x > 0     = Empty
    | otherwise = Single m a
dropJ x (Append m l r)
    | x > intFromSized m = Empty
    | x < sl             = dropJ x l +++ r
    | otherwise          = dropJ (x - sl) r 
    where sl = intFromTag l


takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ x (Single m a)
    | x < 0     = Single m a
    | otherwise = Empty
takeJ x (Append m l r)
    | x < 0     = Append m l r
    | x < sl    = takeJ x l
    | otherwise = l +++ takeJ (x - sl) r
    where sl = intFromTag l


scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str


getSzInt :: JoinList (Score, Size) String -> Int 
getSzInt = fromIntegral . getSize . snd . tag


instance Buffer (JoinList (Score, Size) String) where
    toString Empty          = ""
    toString (Single m a)   = a
    toString (Append m l r) = toString l ++ toString r

    fromString "" = Empty
    fromString str = foldl1 (+++) . map makeSingle $ lines str
          where makeSingle s = Single (scoreString s, Size 1) s

    line x _ | x < 0  = Nothing
    line _ Empty      = Nothing
    line x (Single _ a) 
        | x == 0    = Just a
        | otherwise = Nothing
    line x (Append (sc, sz) l r)
        | x > getSize sz = Nothing
        | x < szl = line x l
        | otherwise      = line (x - szl) r
        where szl = getSzInt l

    replaceLine x str b
        | x < 0          = b
        | x > numLines b = b
        | otherwise      = go x str b
        where go _ _ Empty = Empty
              go _ str (Single m a) = Single m str 
              go x str (Append (_, sz) l r)
                  | x < szl   = go x str l +++ r
                  | otherwise = l +++ go (x - szl) str r
                  where szl = getSzInt l

    numLines Empty = 0
    numLines (Single _ _) = 1
    numLines (Append (_, sz) _ _) = getSize sz

    value Empty = 0
    value (Single (sc, si) _) = fromIntegral sc
    value (Append (sc, _) _ _) = fromIntegral sc
