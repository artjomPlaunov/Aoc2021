module Main where

import Lib
import System.IO
import Data.List.Split

main :: IO ()
main =  
  do
    c <- readFile "day6.txt"
    let nums  = map (read :: String -> Int) (splitOn "," c)
    let m = parse nums
    let res = nx day m 10000
    putStrLn (show (foldr (+) 0 res))

-- Simulate one day of fish life-cycle.
day m = 
  let carry = head m in
  let aux m n = case  n of 
                      8 -> set m 8 carry
                      6 -> aux (set m 6 (carry + (get m 7))) (n+1)  
                      i -> aux (set m i (get m (i+1))) (n+1)
  in aux m 0
  
-- nx applies function "f" to input "i", n times. On each application, the 
-- result "r" becomes the new "i" for the next function application. 
-- The final "r" result is returned.
nx f i n = 
  if n == 0 
  then i 
  else let r = f i in nx f r (n-1)

get ls i = 
  if i == 0 
  then head ls
  else get (tail ls) (i-1)

set ls x val = 
  let aux ls res x =  if x == 0 
                      then
                        (reverse res) ++ [val] ++ (tail ls) 
                      else
                        aux (tail ls) ((head ls):res) (x-1)
  in aux ls [] x

ins ls x = 
  let aux ls res x =  if x == 0 
                      then 
                        (reverse res) ++ [(head ls)+1] ++ (tail ls)
                      else
                        aux (tail ls) ((head ls):res) (x-1)
  in (aux ls [] x)

parse nums = 
  let aux nums res =  case  nums of 
                            []      ->  res
                            (n:ns)  ->  let _res = ins res n in aux ns _res in
  aux nums [0,0,0,0,0,0,0,0,0,0]


      --let _res = ins res n 
                  --in aux ns _res 
