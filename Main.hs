module Main where

import System.Environment

import Function
import Parser

runProgram :: [String] -> [Int] -> Int
runProgram defs vs = evaluate (resolveProgram (parseFunctionDefs defs)) vs

main :: IO ()
main = do
  path <- getArgs
  text <- readFile $ head path
  let l = lines text
  let p = runProgram l
  params <- getLine
  let i = p (parseParams params)
  putStrLn ("Output is " ++ show i)
