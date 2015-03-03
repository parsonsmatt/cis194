module Main where

import Scrabble
import Sized
import JoinList
import Editor

main = runEditor editor $ Single (0 :: Score, 1 :: Size) "an buffer"
