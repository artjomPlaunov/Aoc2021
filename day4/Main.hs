module Main where

import Lib
import System.IO
import Data.List.Split
import Data.List
import Data.Char

rotl = transpose . map reverse

splitRead delim row = map (\i -> read i::Integer) (splitOn delim row)

genRow s = map (\i -> read i::Integer) (wordsBy isSpace s)

genBoards lst = 
  let aux lst res = case  lst of 
                          []  ->  res
                          _   ->  let board = map (genRow) (take 5 lst)
                                  in  aux (drop 5 lst) (board:res)
  in aux lst []

markNums rows n = map (\i -> map (\x -> if x == n then -1 else x) i) rows

checkRow r = if (length (filter (\i -> i /= -1) r)) == 0 then True else False

checkRows b = foldr (||) False (map checkRow b)

checkBingoBoard board = (checkRows board) || (checkRows (rotl board))

checkBingoAll boards = 
  case  boards of
        []    ->  (False, (head boards))
        (h:t)  ->  if (checkBingoBoard h) then (True, h) else (checkBingoAll t)

_playBingo boards nums = 
  case  nums of
        []    ->  (head boards,(-1))
        (h:t) ->  let b = map (\r -> markNums r h) boards in
                  let (bingo,winner) = checkBingoAll b in
                  if bingo then (winner, h) else _playBingo b t

playBingo boards nums = 
  if (length boards) == 1 then (boards, nums) else
  let b = map (\r -> markNums r (head nums)) boards in
  let res = filter (\i -> (not (checkBingoBoard i))) b in
  playBingo res (tail nums)


sumBoard winner = foldr (+) 0 (map (sum) winner)

main :: IO ()
main =  
  do
    contents <- readFile "day4.txt"
    let lst = lines contents
    let nums = splitRead "," (head lst)
    let input = filter (\i -> i /= "") (tail lst)
    let boards = genBoards input
    --let (winner, num) = playBingo boards nums
    let (loser, _nums) = playBingo boards nums
    let (winner, num) = _playBingo loser _nums
    putStrLn (show $ (sumBoard (map (filter (\i -> i /= -1)) winner)) * num)












