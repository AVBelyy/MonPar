module Main where

import Control.Applicative (Applicative(..))

import LibPar

--------------
--  Datatypes
--------------

type EBNF = [Operator]

data Operator = Operator String Expression
                deriving Show

data Expression = ID String
                | Chain String
                | Group Expression
                | Many Expression
                | Try Expression
                | Term [Expression]
                | Or [Expression]
                deriving Show

------------
-- Alphabet
------------

quote = char '\''
orelse = char '|'
lpar = char '('
rpar = char ')'
lcbracket = char '{'
rcbracket = char '}'
lsbracket = char '['
rsbracket = char ']'
dot = char '.'

assignment = string "::="
letter = anyOf (['a'..'z'] ++ ['0'..'9'] ++ "_")
space = anyOf " \t\n"
symbol = escape <|> noneOf "'"

escapeChar = anyOf ('"' : "nt'\\")
escape = (\c -> head (read ("\"\\" ++ [c] ++ "\"") :: String)) <$> (char '\\' >> escapeChar)

-----------
-- Grammar
-----------

ebnf = between spaces (manySepBy operator spaces) spaces

operator = Operator <$> word <* (spaces >> assignment >> spaces) <*> expression <* (spaces >> dot)

expression = Or <$> many1SepBy term (spaces >> orelse >> spaces)

term = Term <$> many1SepBy factor spaces

factor = identifier <|> chain <|> group_factor <|> many_factor <|> try_factor

identifier = ID <$> word

chain = Chain <$> between quote symbols quote

group_factor = Group <$> between (lpar >> spaces) expression (spaces >> rpar)

many_factor = Many <$> between (lcbracket >> spaces) expression (spaces >> rcbracket)

try_factor = Try <$> between (lsbracket >> spaces) expression (spaces >> rsbracket)

word = many1 letter

symbols = many symbol

spaces = many space

-------------
-- Generator
-------------

data GenContext a = Ctx (String -> GenContext a) | Leaf a

type ContextStorage a = String -> (GenContext a -> GenContext a)

instance Functor GenContext where
    fmap f (Leaf p) = Leaf (f p) -- homomorphism law
    fmap f (Ctx  p) = Ctx  (\s -> f <$> p s) -- typechecks

instance Applicative GenContext where
    pure = Leaf
    (<*>) (Leaf f) (Leaf p) = Leaf (f p) -- homomorphism law
    (<*>) (Leaf f) (Ctx  p) = Ctx  (\s -> Leaf f <*> p s) -- fmap law
    (<*>) (Ctx  f) (Leaf p) = Ctx  (\s -> Leaf ($ p) <*> f s) -- interchange law
    (<*>) (Ctx  f) (Ctx  p) = Ctx  (\s -> f s <*> p s) -- my guess

genFromExpr :: Expression -> GenContext (Parser ()) -> GenContext (Parser ())
genFromExpr (ID    x) (Ctx p)  = Leaf $ get (p x)
genFromExpr (Chain x) _        = Leaf $ string x >> return ()
genFromExpr (Group x) p        = genFromExpr x p
genFromExpr (Many  x) (Ctx p)  = Ctx  $ \_ -> (\y -> many y >> return ()) <$> genFromExpr x (Ctx p)
genFromExpr (Try   x) (Ctx p)  = Ctx  $ \_ -> (\y -> y <|> return ()) <$> genFromExpr x (Ctx p)
genFromExpr (Or   xs) (Ctx p)  = Ctx  $ \_ -> foldl1 (\x y -> (<|>) <$> x <*> y) (map (\x -> genFromExpr x (Ctx p)) xs)
genFromExpr (Term xs) (Ctx p)  = Ctx  $ \_ -> foldl1 (\x y -> (>>) <$> x <*> y) (map (\x -> genFromExpr x (Ctx p)) xs)
genFromExpr _         (Leaf _) = error "no ctx"

genFromOperator :: Operator -> ContextStorage (Parser ()) -> ContextStorage (Parser ())
genFromOperator (Operator x y) ctx = \s -> if s == x then genFromExpr y else ctx s

emptyCtx :: ContextStorage (Parser ())
emptyCtx s = error $ "rule '" ++ s ++ "' was not declared"

genFromEBNF :: EBNF -> ContextStorage (Parser ())
genFromEBNF []     = emptyCtx
genFromEBNF (x:xs) = genFromOperator x (genFromEBNF xs)

-- some magic
getFrom :: ContextStorage (Parser ()) -> String -> GenContext (Parser ())
getFrom ctx s = ctx s (Ctx (getFrom ctx))

get :: GenContext (Parser ()) -> Parser ()
get (Leaf a) = a
get (Ctx  a) = get (a "")

getRule :: ContextStorage (Parser ()) -> String -> Parser ()
getRule ctx s = get (getFrom ctx s)

--------
-- Main
--------

main = do
    input <- readFile "grammar.txt"
    text <- readFile "text.txt"
    let grammar = parse ebnf input
    let ctx = genFromEBNF grammar
    let parser = getRule ctx "main"
    putStrLn "Parsed grammar from grammar.txt:"
    print $ grammar
    putStrLn "\nResult of parsing text.txt with this grammar:"
    print $ parse parser text
