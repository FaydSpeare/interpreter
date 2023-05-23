module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lexer
import Token

main = defaultMain tests

testParseIdent :: String -> String -> Assertion
testParseIdent a b = assertEqual (a++b) a' b'
    where (a', _) = parseIdent a
          b'      = Token IDENT b

tests :: TestTree
tests = testGroup "Lexer tests"
    [ testCase "parse identifier" $
        testParseIdent "var" "var"
    , testCase "parse identifier"  $
        testParseIdent "varA" "varA" 
    ]
