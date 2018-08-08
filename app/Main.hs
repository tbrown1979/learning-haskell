module Main where

import Lib

main :: IO ()
main = getPhoto >>= (putStrLn . show . take 5)
