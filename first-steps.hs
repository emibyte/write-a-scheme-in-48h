module Main where

import System.Environment

main :: IO ()
main = exerciseThree
-- main = do
--   args <- getArgs
--   putStrLn ("Hello, " ++ args !! 0)

exerciseOne :: IO()
exerciseOne = do
  args <- getArgs
  putStrLn(args !! 0 ++ args !! 1)

exerciseTwo :: IO ()
exerciseTwo = do
  args <- getArgs
  let num1 = read (args !! 0) :: Int
  let num2 = read (args !! 1) :: Int
  putStrLn $ show $ num1 + num2

exerciseThree :: IO ()
exerciseThree = do
  putStrLn "What's ur name?"
  input <- getLine
  putStrLn ("Hello, " ++ input)

