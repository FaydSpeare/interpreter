module Main where

import System.IO
import Control.Monad (forever)

import Lexer

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
    a <- getLine
    mapM_ (putStrLn . show) $ parseTokens a
    putStrLn ""
