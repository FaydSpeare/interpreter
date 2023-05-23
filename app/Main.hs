module Main where

import System.IO
import Control.Monad (forever)
import Data.List

import Ast
import Lexer
import Parser
import Evaluator

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "---------------------------------------"
    putStrLn " Welcome to the interpreter."
    putStrLn " Enter something and see what happens!"
    putStrLn "---------------------------------------"
    inputLoop

inputLoop :: IO ()
inputLoop = forever $ do
    putStr ">> "
    a <- getLines
    let tokens = parseTokens . intercalate ";" $ a
    let nodes = parseRoot tokens
    let (value, vm) = evalRoot [] nodes
    putStrLn ""
    mapM_ print tokens
    putStrLn ""
    mapM_ (putStrLn . nodeString) nodes
    putStrLn ""
    putStrLn $ "= " ++ extract value
    -- print vm
    putStrLn ""

getLines :: IO [String]
getLines = do 
    x <- getLine
    if x == "" || last x == ';'
        then return [x]
        else do 
            putStr ".. "
            xs <- getLines
            return (x:xs)
