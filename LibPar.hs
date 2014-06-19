module LibPar where

data Error = Error     String
           | Exception String

newtype State c a = State { run :: [c] -> ([c], Either Error a) }

type Parser a = State Char a

instance Monad (State c) where
    return a = State $ \s -> (s, Right a)
    (>>=) x f = State $ \s -> case run x s of
        (s', Left a)   -> (s', Left a)
        (s', Right a)  -> run (f a) s'

instance Functor (State c) where
    fmap f m = do
        x <- m
        return (f x)

oops :: String -> Parser a
oops desc = State $ \s -> (s, Left (Error desc))

throw :: String -> Parser a
throw desc = State $ \s -> (s, Left (Exception desc))

try :: Parser a -> Parser (Maybe a)
try p = State $ \s -> case run p s of
    (_, Left (Exception e)) -> (s, Left (Exception e))
    (_, Left (Error _))     -> (s, Right Nothing)
    (s', Right a)           -> (s', Right (Just a))

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q  = State $ \s -> case run p s of
    (_, Left (Error _))     -> run q s
    (_, Left (Exception e)) -> (s, Left (Exception e))
    (s', Right a)           -> (s', Right a)

anychar :: Parser Char
anychar = State $ \s -> case s of
    []     -> ([], Left (Error "eof"))
    (x:xs) -> (xs, Right x)

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
string (x:xs) = do
    char x
    string xs
    return (x:xs)

many1 :: Parser a -> Parser [a]
many1 p = do
    x <- p
    xs <- many p
    return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1SepBy :: Parser a -> Parser b -> Parser [a]
many1SepBy p s = do
    x <- p
    xs <- many (s >> p)
    return (x:xs)

manySepBy :: Parser a -> Parser b -> Parser [a]
manySepBy p s = many1SepBy p s <|> return []

between :: Parser b -> Parser a -> Parser b' -> Parser a
between p q r = p >> q >>= \x -> r >> return x

parse :: Parser a -> String -> a
parse p s = case run p s of
    ("", Right a) -> a
    (as, Right _) -> error $ "unexpected " ++ as
    (_, Left (Error err))     -> error err
    (_, Left (Exception err)) -> error err
