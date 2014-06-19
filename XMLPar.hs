module Main where

import LibPar

-------------
-- Datatypes
-------------

data Error = Error     String
           | Exception String

type ParamKey = String

data ParamValue = XMLInteger Integer
                | XMLString  String
                  deriving Show

data Param = Param ParamKey ParamValue
             deriving Show

type Dictionary = [Param]

data Tag  = Tag String Dictionary
            deriving Show

data Node = TextNode String
          | TagNode  Tag [Node]
            deriving Show

type XML = [Node]

-----------
-- Grammar
-----------

letter   = anyOf $ ['a'..'z'] ++ ['A'..'Z']
digit    = anyOf ['0'..'9']
space    = anyOf " \t"

lbracket = char '<'
rbracket = char '>'
slash    = char '/'
squote   = char '\''
dquote   = char '"'
equals   = char '='

lt       = string "&lt;" >> return '<'
gt       = string "&gt;" >> return '>'

symbol   = letter <|> digit
quote    = squote <|> dquote

word     = letter >>= \x -> symbols >>= \xs -> return (x:xs)

number   = many1 digit

symbols  = many symbol
spaces   = many space

text :: Parser Node
text = many1 (lt <|> gt <|> noneOf "<>") >>= return . TextNode

tag_name :: Parser String
tag_name = word

tag_param_string :: Parser ParamValue
tag_param_string = do
    x <- quote
    s <- many (noneOf [x])
    char x
    return $ XMLString s

tag_param_integer :: Parser ParamValue
tag_param_integer = number >>= return . XMLInteger . read

tag_param_value :: Parser ParamValue
tag_param_value = tag_param_string <|> tag_param_integer

tag_param_key :: Parser String
tag_param_key = word

tag_param :: Parser Param
tag_param = do
    x <- tag_param_key
    spaces >> equals >> spaces
    y <- tag_param_value
    return $ Param x y

open_tag :: Parser Tag
open_tag = do
    lbracket >> spaces
    x <- tag_name
    spaces
    xs <- manySepBy tag_param spaces
    spaces >> rbracket
    return $ Tag x xs

open_close_tag :: Parser Tag
open_close_tag = do
    lbracket >> spaces
    x <- tag_name
    spaces
    xs <- manySepBy tag_param spaces
    spaces >> slash >> rbracket
    return $ Tag x xs

close_tag :: String -> Parser ()
close_tag s = do
    lbracket >> slash >> spaces
    string s
    spaces >> rbracket
    return ()

tag_with_body :: Parser Node
tag_with_body = do
    Tag x xs <- open_tag
    y <- nodes
    close_tag x <|> throw ("unmatched <" ++ x ++ ">")
    return $ TagNode (Tag x xs) y

tag_without_body :: Parser Node
tag_without_body = do
    x <- open_close_tag
    return $ TagNode x []

tag :: Parser Node
tag = tag_without_body <|> tag_with_body

node :: Parser Node
node = tag <|> text

nodes :: Parser [Node]
nodes = many node

xml :: Parser XML
xml = nodes

--------
-- Main
--------

main = do
    input <- readFile "input.txt"
    print $ parse xml input
