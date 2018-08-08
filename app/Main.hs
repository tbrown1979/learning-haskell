module Main where

import Lib

main :: IO ()
main = getAllPhotos >>= (putStrLn . show . take 5)
