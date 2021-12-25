module Main where

import Lib
import System.IO

main :: IO ()
main =  
  do
    contents <- readFile "day2.txt"
    let dirs = map split (lines contents)
    let (xAxis, depth, _) = foldr scan (0,0,0) (reverse dirs) 
    putStrLn (show (depth*xAxis))

scan :: ([Char],Integer) -> (Integer, Integer, Integer) 
                                    -> (Integer, Integer, Integer)
scan (d,x) (i,j,aim) = 
  case d of
    "forward" ->  (i+x, (j + (aim*x)), aim)
    "up"      ->  (i, j, aim-x)
    "down"    ->  (i, j, aim+x)
    _         ->  (i,j,aim)

split :: [Char] -> ([Char], Integer)
split s = 
  let lst = words s in 
  case lst of
    (x:y:[]) -> (x, (read y::Integer))
    _ -> ([],-1)

