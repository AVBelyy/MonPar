{-# LANGUAGE RankNTypes, UnicodeSyntax, InstanceSigs, FlexibleContexts #-}
module MonPar where

import Prelude hiding (Maybe(..), Either(..))

data Hole = Hole

hole = undefined

type Error = String

--------------------------

newtype State s a = State { runState :: s -> (s, a) }
newtype Parser c a = Parser
    { runPar :: [c] -> Maybe ([c], a) }

instance Monad (Parser c) where
  return a = Parser $ \s -> Just (s, a)
  x >>= f = Parser $ \s -> case runPar x s of
    Nothing -> Nothing
    Just (s', a) -> runPar (f a) s'



instance Monad (State s) where
    return :: a -> State s a
    return a = State $ \s -> (s, a)
    (>>=) :: State s a -> (a -> State s b) -> State s b
    (>>=) x f = State $ \s -> let (s', a) = (runState x s) in runState (f a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> (s, ())

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (f s, ())

foo = do
  x <- get
  modify (+ 1)
  put x

foo' = get >>= (\x -> modify (+1) >>= (\_ -> put x))

--------------------------

data Either a b = Left a | Right b deriving Show
data Maybe a = Just a | Nothing deriving Show

instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  (Just a) >>= f = f a

type UserName = String
type UserId = Integer

getUserId :: UserName -> Maybe UserId
getUserId = undefined

getUserMoney :: UserId -> Maybe Integer
getUserMoney = undefined

getUserMoneyByName n = do
  id <- getUserId n
  getUserMoney id

--------------------------

-- Comment this block
--{-

--class Stream c a | c -> a where
--  next :: c -> Maybe (a, c)

newtype Monstupar c a = Monstupar
  { runParser :: [c] -> Either ([c], Error) ([c], a) }

instance Monad (Monstupar c) where
  return :: a -> Monstupar c a
  return a = Monstupar $ \s -> Right (s, a)
  (>>=) :: Monstupar c a -> (a -> Monstupar c b) -> Monstupar c b
  (>>=) x f = Monstupar $ \s -> case runParser x s of
    Left l -> Left l
    Right (s', a) -> runParser (f a) s'

oops :: String -> Monstupar c a
oops desc = Monstupar $ \s -> Left (s, desc)

eof :: Monstupar c ()
eof = Monstupar $ \s -> case s of
  [] -> Right ([], ())
  (a:as) -> Left (s, "not eof")

anychar :: Monstupar c c
anychar = Monstupar $ \s -> case s of
  [] -> Left ([], "eof")
  (a:as) -> Right (as, a)

try :: Monstupar c a -> Monstupar c (Maybe a)
try p = Monstupar $ \s -> case runParser p s of
  Left (s', e) -> Right (s, Nothing)
  Right (s', a) -> Right (s', Just a)

(<|>) :: Monstupar c a -> Monstupar c a -> Monstupar c a
p <|> s = do
  x <- try p
  case x of
    Just a -> return a
    Nothing -> s

like :: Show c => (c -> Bool) -> Error -> Monstupar c c
like p err = do
  c <- anychar
  if p c
     then return c
     else oops $ "expected " ++ err ++ " got " ++ show c

parse :: Monstupar Char a -> [Char] -> a
parse p s = case runParser p s of
  Left  (_, e) -> error e
  Right ("", a) -> a
  Right (as, a) -> error as

---}

--------------------------

char :: (Show c, Eq c) => c -> Monstupar c c
char x = like (== x) $ show x

anyOf :: (Show c, Eq c) => [c] -> Monstupar c c
anyOf s = like (`elem` s) $ "something in " ++ show s

space = anyOf " \t"
spaces = many space
lexem p = do
  x <- p
  spaces
  return x

noneOf :: (Show c, Eq c) => [c] -> Monstupar c c
noneOf s = like (not . (`elem` s)) $ "something not in " ++ show s

string :: (Show c, Eq c) => [c] -> Monstupar c [c]
string [] = return []
string (a:as) = do
  char a
  string as
  return (a:as)

many1 :: Monstupar c a -> Monstupar c [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

many :: Monstupar c a -> Monstupar c [a]
many p = many1 p <|> return []

many1SepBy p s = do
  x <- p
  xs <- many (s >> p)
  return (x:xs)

between :: Monstupar c a -> Monstupar c b -> Monstupar c a' -> Monstupar c b
between o m c = do
    o
    x <- m
    c
    return x

parseInteger :: Monstupar Char Integer
parseInteger = do
    is <- many1 (anyOf "0123456789")
    return $ read is

--------------------------

instance Functor (Monstupar c) where
  fmap f m = m >>= (\x -> return (f x))

--------------------------

data Arith = Atom Integer
           | Sum Arith Arith
           | Mul Arith Arith
           deriving Show

-- P -> M + P
-- M -> AT * M
-- AT -> A | (P)

parseAtomic :: Monstupar Char Arith
parseAtomic = do
    xs <- parseInteger
    return (Atom xs)

parseSum :: Monstupar Char Arith
parseSum = fmap (foldr1 Sum) $ many1SepBy parseMul (lexem $ char '+')

parseMul :: Monstupar Char Arith
parseMul = fmap (foldr1 Mul) $ many1SepBy (parseAtomic <|> between (lexem $ char '(') parseSum (lexem $ char ')')) (lexem $ char '*')

eval :: Arith -> Integer
eval (Atom i) = i
eval (Sum x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

c :: String -> Integer
c = eval . parse parseSum

--------------------------

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Monad (Cont r) where
  return :: a -> Cont r a
  return a = Cont $ \g -> g a
  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  (>>=) x f = Cont $ \g -> runCont x (\a -> runCont (f a) g)

callcc :: ((a -> Cont r a) -> Cont r a) -> Cont r a
callcc f = Cont $ \g -> runCont (f (\a -> Cont $ \_ -> g a)) g

bar :: Cont r Int
bar = return 1

bar' :: Cont r Int
bar' = callcc $ \e -> do
  x <- return 1
  y <- return 2
  e 1
  return (x + y)

runContState f = runCont f id
rcs = runContState

--------------------------

-- Uncomment this block
{-

newtype Monstupar c a = Monstupar { runParser :: forall r . [c]
                                              -> ([c] -> Error -> r)
                                              -> ([c] -> a -> r)
                                              -> r }
instance Monad (Monstupar c) where
  return a = Monstupar $ \s err ok -> ok s a
  ma >>= f = Monstupar $ \s err ok -> runParser ma s err (\s' a -> runParser (f a) s' err ok)

oops :: String -> Monstupar c a
oops desc = Monstupar $ \s err ok -> err s desc

eof :: Monstupar c ()
eof = Monstupar $ \s err ok -> case s of
  [] -> ok [] ()
  (a:as) -> err s "not eof"


anychar :: Monstupar c c
anychar = Monstupar $ \s err ok -> case s of
  [] -> err [] "eof"
  (a:as) -> ok as a

(<|>) :: Monstupar c a -> Monstupar c a -> Monstupar c a
p <|> t = Monstupar $ \s err ok -> runParser p s (\_ e -> runParser t s err ok) ok

like :: Show c => (c -> Bool) -> Error -> Monstupar c c
like p err = do
  c <- anychar
  if p c
     then return c
     else oops $ "expected " ++ err ++ " got " ++ show c

parse p s = runParser p s (\s e -> error e) (\s a -> case s of
                                                [] -> a
                                                as -> error as)

-}

-- Everything should work still
