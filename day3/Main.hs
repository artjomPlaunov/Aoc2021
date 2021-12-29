module Main where

import Lib
import System.IO
--import Data.Bits
import Data.Char (digitToInt)
import Data.List (foldl')

-- Convert binary string to Int
toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

joiner :: [Integer] -> String
joiner = concatMap show

-- If bit is set, add one to res, otherwise subtract one.
addBit c res = if c == '0' then (res-1) else (res+1)

-- Return 1 or 0 based on evaluation of cmp n.
setBit cmp n = if cmp n then 1 else 0

fixedPtFilter lst bit n = 
  if length lst == 1 then head lst else
  let ones = foldr (\i -> if (i!!n) == '1' then (+1) else (+0)) 0 lst in
  let zeroes = (length lst) - ones in
  let filterBit = case  bit of 
                        1 ->  if ones >= zeroes 
                              then '1'
                              else '0' 
                        0 ->  if ones < zeroes 
                              then '1'
                              else '0' in
                        
  let _lst = filter (\i -> (i!!n) == filterBit) lst in 
  fixedPtFilter _lst bit (n+1)



main :: IO ()
main =  
  do
    contents <- readFile "day3.txt"
    -- List of Bit Strings
    let lst = lines contents
    -- Bit counters
    let counts = foldr (zipWith addBit) (take 12 (repeat 0)) lst
    let gamma = toDec $ joiner (map (setBit (>0)) counts)
    let epsilon = toDec $ joiner (map (setBit (<0)) counts)
    let oxygen = toDec (fixedPtFilter lst 1 0)
    let co2    = toDec (fixedPtFilter lst 0 0)
    putStrLn (show $ oxygen * co2)

