
module Parser where

import Data.Char 
import Control.Monad 

-- The type of parser 
newtype Parser a = Parser (String -> [(a, String)])

-- Apply a parser
apply :: Parser a -> String -> [(a, String)]
apply (Parser f) s = f s

parse :: Parser a -> String -> a
parse m s = one [x | (x,t) <- apply m s, t == ""]
	where
	one []					= error "no parse"
	one [x]					= x
	one xs | length xs > 1  = error "ambiguous parse"

instance Monad Parser where 
	return x = Parser (\s -> [(x, s)])
	m >>= k  = Parser (\s -> 
				 [ (y, u) |
				   (x, t) <- apply m s,
				   (y, u) <- apply (k x) t ])

instance MonadPlus Parser where 
	mzero		= Parser (\s -> [])
	mplus m n   = Parser (\s -> apply m s ++ apply n s)

char :: Parser Char 
char = Parser f
	where
	f []    = []
	f (c:s) = [(c, s)]

spot :: (Char -> Bool) -> Parser Char 
spot p = do { c <- char; guard (p c); return c }

token :: Char -> Parser Char
token c = spot (== c)

match :: String -> Parser String
match []      = return []
match (x:xs)  = do {
					y  <- token x; 
					ys <- match xs;
					return (y:ys)
				 }

star :: Parser a -> Parser [a]
star p = plus p `mplus` return []

plus :: Parser a -> Parser [a]
plus p = do 
	x <- p
	xs <- star p
	return (x:xs)
