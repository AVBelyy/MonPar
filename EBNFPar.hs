module Main where

import LibPar

--------------
--  Datatypes
--------------

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
letter = anyOf ['a'..'z']
space = anyOf " \t\n"
symbol = noneOf "'"

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

--------
-- Main
--------

main = do
    input <- readFile "grammar.txt"
    print $ parse ebnf input
