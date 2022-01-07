module Main where

import Lib
import System.IO
import Data.List.Split

main :: IO ()
main =  
  do
    c <- readFile "day7.txt"
    let nums  = map (read :: String -> Int) (splitOn "," c)
    let hi    = foldr max 0 nums
    --let res = foldr (min) (maxBound :: Int) (map (f1 nums) [0..hi])
    let res = foldr (min) (maxBound :: Int) (map (f2 nums) [0..hi])
    putStrLn $ show res

f1 ns val = foldr (\i -> (+) (abs (i - val)) ) 0 ns

d x y = let n = abs (x-y) in div (n * (n+1))  2

f2 ns val = foldr (\i -> (+) (d val i)) 0 ns

