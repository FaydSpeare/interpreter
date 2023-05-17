module Token where

type Literal = String
data Token = Token TokenType Literal deriving (Show, Eq)
data TokenType = INT
               | IDENT
               | ASSIGN
               | MUL
               | PLUS
               | UNDEFINED
               | SPACE
               | INVALID
               | EOF deriving (Show, Eq)


