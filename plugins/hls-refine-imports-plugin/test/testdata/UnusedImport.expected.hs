module Main where

import A
import D
import F
import G ()
import Data.List (intercalate)

main :: IO ()
main = putStrLn 
     $ "hello " 
    <> intercalate ", " [b1, c1, e1, f1, g1]
