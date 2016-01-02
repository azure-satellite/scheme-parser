module Main where

import Parser
import System.Environment

main :: IO ()
main = getArgs >>= putStrLn . readExpr . head
