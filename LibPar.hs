module LibPar (
    Parser, Error,
    oops, throw, try, (<|>), anychar, like, char, anyOf, noneOf,
    string, keyword, many1, many, many1SepBy, manySepBy, between, parse,
    (<$>), (<*>), (<*), (*>)) where

import Control.Applicative (Applicative(..), (<$>))
import Data.Char

data Error = Error     String
           | Exception String

newtype State c a = State { run :: [c] -> Either Error ([c], a) }

type Parser a = State Char a

instance Functor (State c) where
    fmap f m = do
        x <- m
        return (f x)

instance Applicative (State c) where
    pure  = return
    (<*>) f x = do
        f' <- f
        x' <- x
        return (f' x')

instance Monad (State c) where
    return a = State $ \s -> Right (s, a)
    (>>=) x f = State $ \s -> case run x s of
        Left a -> Left a
        Right (s', a) -> run (f a) s'

oops :: String -> Parser a
oops desc = State $ \_ -> Left (Error desc)

throw :: String -> Parser a
throw desc = State $ \_ -> Left (Exception desc)

try :: Parser a -> Parser (Maybe a)
try p = State $ \s -> case run p s of
    Left (Exception e) -> Left (Exception e)
    Left (Error _)     -> Right (s, Nothing)
    Right (s', a)      -> Right (s', Just a)

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q  = State $ \s  -> case run p s of
    Left (Error _)     -> run q s
    Left (Exception e) -> Left (Exception e)
    Right (s', a)      -> Right (s', a)

anychar :: Parser Char
anychar = State $ \s -> case s of
    []     -> Left (Error "eof")
    (x:xs) -> Right (xs, x)

like :: (Char -> Bool) -> String -> Parser Char
like p desc = do
    c <- anychar
    if p c
    then return c
    else oops $ "expected " ++ desc ++ ", got " ++ show c

char :: Char -> Parser Char
char x = like (== x) (show x)

anyOf :: String -> Parser Char
anyOf s = like (`elem` s) $ "something in " ++ show s

noneOf :: String -> Parser Char
noneOf s = like (not . (`elem` s)) $ "something not in " ++ show s

string :: String -> Parser String
string [] = return []
string (x:xs) = (:) <$> char x <*> string xs

keyword :: String -> Parser String
keyword [] = return []
keyword (x:xs) = (:) <$> char (toUpper x) <|> char (toLower x) <*> keyword xs

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1SepBy :: Parser a -> Parser b -> Parser [a]
many1SepBy p s = (:) <$> p <*> many (s >> p)

manySepBy :: Parser a -> Parser b -> Parser [a]
manySepBy p s = many1SepBy p s <|> return []

between :: Parser b -> Parser a -> Parser b' -> Parser a
between p q r = p *> q <* r

parse :: Parser a -> String -> a
parse p s = case run p s of
    Right ("", a) -> a
    Right (as, _) -> error $ "unexpected " ++ as
    Left (Error err)     -> error err
    Left (Exception err) -> error err
