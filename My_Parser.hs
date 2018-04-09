module My_Parser where

import Data.Char
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) inp = p inp

instance Functor Parser where
    fmap = liftM
 
instance Applicative Parser where
    pure a = Parser (\inp -> [(a, inp)]) {- move the definition of `return` from the `Monad` instance here -}
    (<*>) = ap

instance Monad Parser where
--return a = Parser (\inp -> [(a, inp)])
  return = pure
  p >>= f = Parser (\inp -> case parse p inp of
                              [(a, out)] -> parse (f a) out
                              []         -> [])

failure :: Parser a
failure = Parser (\_ -> [])

item :: Parser Char
item = Parser (\inp -> case inp of 
                         []   -> []
                         x:xs -> [(x,xs)])

myparser :: Parser (Char, Char)
myparser = do 
  c1 <- item
  item
  c2 <- item
  return (c1, c2)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\inp -> case parse p inp of
  []        -> parse q inp
  [(a,out)] -> [(a,out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else failure

digit = sat isDigit
lower = sat isLower
upper = sat isUpper
letter = sat isLetter

char :: Char -> Parser Char
char c = sat (==c)


string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

many :: Parser a -> Parser [a]
many p = (many1 p) +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do
 x <- p
 xs <- many p
 return (x:xs)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

symbol :: String -> Parser String
symbol xs = do 
  space
  string xs
  space
  return xs