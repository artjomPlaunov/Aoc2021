module Main where

import Lib
import System.IO
import Text.Regex.Posix 
import qualified Data.Map.Strict as Map
import Data.Map (Map())

type IntTup = (Integer,Integer)

matchP :: String -> (IntTup, IntTup)
matchP s = 
  let (p1,_,p2) = s   =~ (" -> ") :: (String,String,String) in
  let (x1,_,y1) = p1  =~ (",")    :: (String,String,String) in
  let (x2,_,y2) = p2  =~ (",")    :: (String,String,String) in
  ( ((read x1 :: Integer), (read y1 :: Integer)),
    ((read x2 :: Integer), (read y2 :: Integer))
  ) 

-- Return (min,max)
minMax x y = 
  if x < y then (x,y) else (y,x)

genPts :: IntTup ->  IntTup -> [IntTup]
genPts (x1,y1) (x2,y2) = 
  if  x1 == x2 
  then  
    let (min,max) = minMax y1 y2 in 
    [ (x1,y) | y <- [min..max] ]
  else 
    if y1 == y2 
    then
      let (min,max) = minMax x1 x2 in 
      [ (x,y1) | x <- [min..max] ]
    else
      if ((x1 > x2) && (y1 > y2)) || ((x1 < x2) && (y1 < y2)) 
      then
        let (minX,maxX) = minMax x1 x2 in
        let (minY,maxY) = minMax y1 y2 in
        let cX = [ (x,0) | x <- [minX..maxX] ] in
        let cY = [ (0,y) | y <- [minY..maxY] ] in
        zipWith (\(x,_) (_,y) -> (x,y)) cX cY
      else
        let (minX,maxX) = minMax x1 x2 in
        let (minY,maxY) = minMax y1 y2 in
        let cX = [ (x,0) | x <- [minX..maxX] ] in
        let cY = reverse [ (0,y) | y <- [minY..maxY] ] in
        zipWith (\(x,_) (_,y) -> (x,y)) cX cY


insPt :: IntTup -> Map IntTup Integer -> Map IntTup Integer
insPt (x,y) m = 
  if Map.member (x,y) m 
  then
    let count = Map.findWithDefault 0 (x,y) m in
    Map.insert (x,y) (count+1) m
  else
    Map.insert (x,y) (1) m

insPts :: IntTup -> IntTup -> Map IntTup Integer -> Map IntTup Integer
insPts p1 p2 m = 
  let ps = genPts p1 p2 in 
  let aux ps m  = case  ps of
                        []    ->  m
                        h:t   ->  aux t (insPt h m)  in 
  aux ps m

isNonDiag :: (IntTup, IntTup) -> Bool
isNonDiag ((x1,y1),(x2,y2)) = (x1 == x2) || (y1 == y2)

main :: IO ()
main =  
  do
    contents <- readFile "day5.txt"
    let pts = map matchP (lines contents)
    --let pts = filter (isNonDiag) _pts
    let m = (Map.fromList []) :: (Map IntTup Integer)
    let m' = foldr (\(p1,p2) -> insPts p1 p2) m pts
    let res1 = foldr (\((_,_),c) -> if c > 1 then (+1) else (+0)) 
                      0 (Map.toList m')
    putStrLn (show res1)




