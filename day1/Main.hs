module Main where

import Lib
import System.IO

main :: IO ()
main =  
  do
    contents <- readFile "day1.txt"
    let nums = map (read::String->Int) (lines contents)
    let res1 = countInc nums 0
    putStrLn (show res1)

countInc [] x = x
countInc (a:b:c:d:xs) x = 
  let x' = if d > a then (x+1) else x in
  countInc (b:c:d:xs) x'
countInc _ x = x


