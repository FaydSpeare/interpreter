module Evaluator where

import Ast

data EValue = StrLit String | IntLit Int deriving (Eq, Show)
type ValueMap = [(EValue, EValue)]
type EvalState = (EValue, ValueMap)

extract :: EValue -> String
extract (StrLit s) = s
extract (IntLit v) = show v

setValue :: ValueMap -> Node -> EValue -> ValueMap
setValue vs (Identifier n) v = (StrLit n, v):vs

getValue :: ValueMap -> String -> EValue
getValue [] s = StrLit s
getValue (((StrLit a), v):vs) s = if a == s then v else getValue vs s

evalMinus :: EValue -> EValue
evalMinus (IntLit a) = IntLit (-a)
evalMinus _ = IntLit 0

evalSqrt :: EValue -> EValue
evalSqrt (IntLit a) = IntLit $ (floor . sqrt . fromIntegral) a

evalPrefix :: ValueMap -> Operator -> Node -> EvalState
evalPrefix vm o r 
    | o == "$" = (evalSqrt rval, nvm)
    | o == "-" = (evalMinus rval, nvm)
        where (rval, nvm) = eval vm r

evalMul :: EValue -> EValue -> EValue
evalMul (IntLit a) (IntLit b) = IntLit (a * b)
evalMul _ _ = IntLit 0

evalAdd :: EValue -> EValue -> EValue
evalAdd (IntLit a) (IntLit b) = IntLit (a + b)
evalAdd _ _ = IntLit 0


evalSub :: EValue -> EValue -> EValue
evalSub (IntLit a) (IntLit b) = IntLit (a - b)
evalSub _ _ = IntLit 0

evalDiv :: EValue -> EValue -> EValue
evalDiv (IntLit a) (IntLit b) = IntLit (a `div` b)
evalDiv _ _ = IntLit 0

evalPow :: EValue -> EValue -> EValue
evalPow (IntLit a) (IntLit b) = IntLit $ a ^ b
evalPow _ _ = IntLit 0

evalInfix :: ValueMap -> Node -> Operator -> Node -> EvalState
evalInfix vm l o r
    | o == "=" = (rval, setValue vm l rval)
    | o == "x" = (evalMul lval rval, nvm)
    | o == "/" = (evalDiv lval rval, nvm) 
    | o == "-" = (evalSub lval rval, nvm)
    | o == "+" = (evalAdd lval rval, nvm)
    | o == "^" = (evalPow lval rval, nvm)
        where (lval, lvm) = eval vm l
              (rval, nvm) = eval lvm r

eval :: ValueMap -> Node -> EvalState
eval vm (IntLiteral v) = (IntLit (read v :: Int), vm)
eval vm (Identifier n) = (getValue vm n, vm)
eval vm (InfixExpression l o r) = evalInfix vm l o r
eval vm (PrefixExpression o r) = evalPrefix vm o r

evalRoot :: ValueMap -> [Node] -> EvalState
evalRoot vm [n] = eval vm n
evalRoot vm (n:nodes) = evalRoot nvm nodes
    where (_, nvm) = eval vm n
