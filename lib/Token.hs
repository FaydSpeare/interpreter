module Token where

type Literal = String
data Token = Token TokenType Literal deriving (Show, Eq)
data TokenType = INT
               | IDENT
               | ASSIGN
               | MINUS 
               | DIV
               | MUL
               | DORKA
               | PLUS
               | LBRAC
               | RBRAC
               | SEMIC
               | POW
               | UNDEFINED
               | SPACE
               | INVALID
               | EOF deriving (Show, Eq)


