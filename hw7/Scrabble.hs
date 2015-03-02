{-# LANGUAGE TypeSynonymInstances #-}
module Scrabble where
import Data.Monoid
import Data.Char (toLower)
import Data.List (foldl')

type Score = Integer

instance Monoid Score where
    mempty = 0
    mappend = (+)


score :: Char -> Score
score a
  | a == 'a' ||
    a == 'e' ||
    a == 'i' ||
    a == 'l' ||
    a == 'n' ||
    a == 'o' ||
    a == 'r' ||
    a == 's' || 
    a == 't' ||
    a == 'u'    = 1
  | a == 'd' ||
    a == 'g'    = 2
  | a == 'b' || 
    a == 'c' ||
    a == 'm' ||
    a == 'p'    = 3
  | a == 'f' ||
    a == 'h' ||
    a == 'v' ||
    a == 'w' || 
    a == 'y'    = 4
  | a == 'k'    = 5
  | a == 'j' ||
    a == 'x'    = 8
  | a == 'q' ||
    a == 'z'    = 10
  | otherwise   = 0


scoreString :: String -> Score
scoreString = foldl' (\acc x -> acc <> score x) 0 . map toLower 
