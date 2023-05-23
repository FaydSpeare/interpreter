module Lexer where

import Data.Char (digitToInt)
import Token

letters = '_':['a'..'z']
numbers = ['0'..'9']

isDigit :: Char -> Bool
isDigit c = c `elem` numbers

isLetter :: Char -> Bool
isLetter c = c `elem` letters

isValidIdent :: Char -> Bool
isValidIdent c = isDigit c || isLetter c

parseIdent :: String -> (Token, Int)
parseIdent str =
    let
        ident = takeWhile isValidIdent str
        len = length ident
    in (Token IDENT ident, len)

parseInt :: String -> (Token, Int)
parseInt str =
    let 
        int = takeWhile isDigit str
        len = length int
    in (Token INT int, len)

parseToken :: String -> (Token, Int)
parseToken string
    | c == ' '   = (Token SPACE s, 1)
    | c == '-'   = (Token MINUS s, 1)
    | c == '+'   = (Token PLUS s, 1)
    | c == '/'   = (Token DIV s, 1)
    | c == 'x'   = (Token MUL s, 1)
    | c == '$'   = (Token DORKA s, 1)
    | c == '='   = (Token ASSIGN s, 1)
    | c == '('   = (Token LBRAC s, 1)
    | c == ')'   = (Token RBRAC s, 1)
    | c == ';'   = (Token SEMIC s, 1)
    | c == '^'   = (Token POW s, 1)
    | isDigit c  = parseInt string 
    | isLetter c = parseIdent string 
    | otherwise  = (Token UNDEFINED s, 1)
        where c = head string
              s = [c]

parseTokens :: String -> [Token]
parseTokens string 
    | null string = [Token EOF ""]
    | otherwise          = token : parseTokens nextString
        where (token, len) = parseToken string
              nextString   = drop len string





























