module Main where

import H
import E ( e2 )
import Data.List (intercalate)

main :: IO ()
main = putStrLn 
     $ "hello " 
    <> intercalate ", " [h, e2]
