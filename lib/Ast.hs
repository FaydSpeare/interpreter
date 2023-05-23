module Ast where

import Text.Printf

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
