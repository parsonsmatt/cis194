{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

{- Ok, what?
 - oneOrMore first fmaps (:) over the parser p. This creates a 
 - value of type Parser (a -> [a]) which might be represented as
 - Parser { runParser = Just (x : , rest) }. Then oneOrMore applies
 - the 'function in a context' to the Parser that is a result of 
 - zeroOrMore p.
 -
 - zeroOrMore p calls oneOrMore p. If the parser succeeds, then the
 - result is of the same type, and might be thought of as:
 -      Parser { runParser = Just (x : y : , rest) }
 - At some point, oneOrMore will fail. In this case, zeroOrMore
 - ignores the Nothing value, and instead returns `pure []` which
 - lifts [] into the Parser context, which would result in a final
 - value of:
 -      Parser { runParser = Just (x : y : z : [], rest) }
 - Which of course translates into 
 -      Parser { runParser = Just ([x,y,z], rest) }
 -  Which is precisely what we're looking for!
 -}

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show
