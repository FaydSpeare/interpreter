module Parser where

import Text.Printf

import Token

type Prec = Int
type Operator = String
type Value = String
type Name = String

data Node = IntLiteral Value
          | Identifier Name
          | InfixExpression Node Operator Node
          | PrefixExpression Operator Node
          | Undefined deriving (Show)

nodeString :: Node -> String
nodeString (IntLiteral v) = v
nodeString (Identifier n) = n
nodeString Undefined = "undefined"
nodeString (PrefixExpression o n) = printf "(%s%s)" o (nodeString n)
nodeString (InfixExpression l o r) = printf "(%s %s %s)" (nodeString l) o (nodeString r)

prefixPrec = 3

prec :: TokenType -> Int
prec LBRAC = 1000
prec DORKA = 1
prec MUL = 2
prec DIV = 3
prec MINUS = 1
prec PLUS = 1
prec _ = 0

parseMinusPrefix :: [Token] -> (Node, [Token])
parseMinusPrefix tokens = (PrefixExpression "-" node, rest) 
    where (node, rest) = parseExpression prefixPrec tokens

parseGroupedExp :: [Token] -> (Node, [Token])
parseGroupedExp tokens = (if valid then node else Undefined, rest)
    where (node, (Token t _):rest) = parseExpression 0 tokens
          valid = t == RBRAC

parsePrefixExpression :: [Token] -> (Node, [Token])
parsePrefixExpression ((Token t literal):tokens) = case t of
    INT   -> (IntLiteral literal, tokens)
    IDENT -> (Identifier literal, tokens)
    LBRAC -> parseGroupedExp tokens
    MINUS -> parseMinusPrefix tokens
    _     -> (Undefined, tokens)

parseInfixExpression :: [Token] -> Node -> (Node, [Token])
parseInfixExpression ((Token t literal):tokens) left = (node, rest)
        where (right, rest) = parseExpression (prec t) tokens
              node         = InfixExpression left literal right

recurseInfix :: Prec -> [Token] -> Node -> (Node, [Token])
recurseInfix p tokens@((Token t l):_) left
    | p >= prec t = (left, tokens)
    | otherwise   = recurseInfix p rest newLeft
        where (newLeft, rest) = parseInfixExpression tokens left

parseExpression :: Prec -> [Token] -> (Node, [Token])
parseExpression p tokens = recurseInfix p rest left
    where (left, rest) = parsePrefixExpression tokens

parseRoot :: [Token] -> Node
parseRoot [] = Undefined
parseRoot [Token EOF _] = Undefined
parseRoot tokens = fst . parseExpression 0 . filter (\(Token t l) -> t /= SPACE) $ tokens










