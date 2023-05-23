module Parser where

import Ast
import Token

type Prec = Int

prefixPrec = 3

prec :: TokenType -> Int
prec LBRAC = 1000
prec POW = 5
prec DORKA = 4
prec MUL = 3
prec DIV = 3
prec MINUS = 2
prec PLUS = 2
prec ASSIGN = 1
prec _ = 0

parseMinusPrefix :: [Token] -> (Node, [Token])
parseMinusPrefix tokens = (PrefixExpression "-" node, rest) 
    where (node, rest) = parseExpression prefixPrec tokens

parseSqrtPrefix :: [Token] -> (Node, [Token])
parseSqrtPrefix tokens = (PrefixExpression "$" node, rest) 
    where (node, rest) = parseExpression prefixPrec tokens

parseGroupedExp :: [Token] -> (Node, [Token])
parseGroupedExp tokens = (if valid then node else Undefined, rest)
    where (node, (Token t _):rest) = parseExpression 0 tokens
          valid = t == RBRAC

parsePrefixExpression :: [Token] -> (Node, [Token])
parsePrefixExpression ((Token t literal):tokens) = case t of
    DORKA -> parseSqrtPrefix tokens
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

parseRoot :: [Token] -> [Node]
parseRoot [] = []
parseRoot [Token EOF _] = []
parseRoot ((Token SEMIC _):tokens) = parseRoot tokens
parseRoot tokens = node : parseRoot rest 
    where tokens' = filter (\(Token t l) -> t /= SPACE) tokens
          (node, rest) = parseExpression 0 tokens'









