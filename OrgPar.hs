module OrgPar where

import LibPar

-------------
-- Datatypes
-------------

data TextTree = Node String [TextTree]
                deriving Show

-----------
-- Grammar
-----------

-- extend if you like
symbol = anyOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ,."

asterisk = char '*'
space = char ' '

header = many1 symbol >>= \s -> char '\n' >> return s

-- header_prefix parser generator
header_prefix_asterisks 1 = asterisk >> return ()
header_prefix_asterisks n = asterisk >> (header_prefix_asterisks (n - 1))

header_prefix_spaces 1 = return ()
header_prefix_spaces n = space >> (header_prefix_spaces (n - 1))

header_prefix n = header_prefix_asterisks n <|> header_prefix_spaces n

line n = header_prefix n >> header

node :: Int -> State Char TextTree
node lvl = do
    title <- try (line lvl)
    case title of
        Nothing -> oops ""
        Just a  -> do
            children <- nodes (lvl + 1)
            return $ Node a children

nodes = many . node

text = nodes 1

--------
-- Main
--------

countnodes :: [TextTree] -> Int
countnodes (Node _ a : b) = countnodes a + countnodes b + 1
countnodes []             = 0

main :: IO ()
main = do
    input <- readFile "input.txt"
    let rootNodes = parse text input
    print $ Node "" rootNodes
    print $ countnodes rootNodes
