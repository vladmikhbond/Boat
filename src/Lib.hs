{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib (mainBoat) where

import Data.List ( (\\), sort, isPrefixOf)


-- состояние - упоряд. множ. объектов, включая лодку, на левом берегу (множ на правом = разность)
type State = [Char]
type Hist  = [State]

boat : goods = "_cgv"
invalidStates = map sort ["gv", "cg", "cgv", "_c", "_v", "_"]

-- переходы: если лодка на лев берегу, удалить пустую лодку или лодку с любым объектом
--           иначе добавить пустую лодку или лодку с любым объектом с правого берега
nextStates :: State -> [State]
nextStates state = let
   thisBank = state \\ [boat]
   otherBank = goods \\ state
   states = if boat `elem` state
    then thisBank : [ thisBank \\ [x] | x <- thisBank]
    else (boat : state) : [boat : x : state | x <- otherBank]
 in
   map sort states

isValid :: State -> Bool
isValid state = state `notElem` invalidStates && 
   all (`elem` "_cgv") state

variants :: Hist -> [Hist]
variants hist = do
   let nexts = filter isValid (nextStates $ head hist)
   next <- filter (`notElem` hist) nexts
   if null next
     then return $ reverse ("" : hist)
     else variants (next : hist)

solve x = variants [sort x]

-- shipping ["_cgv","cv","_cv","c","_cg","g","_g",""]
shipping :: Hist -> Hist
shipping hist = tail $ zipWith g hist ("" : hist)
 where
    g x y = if length x <= length y
       then '>' : (y \\ x)
       else '<' : (x \\ y)

showBoat :: String -> IO()
showBoat (c : _ : cs) = putStrLn  $ "\\_"++ g ++"_/ " ++ [c]
    where g = if null cs then "_" else cs 


mainBoat :: State -> IO ()
mainBoat x | (not . isValid . sort) x = putStrLn "impossible"
mainBoat x = let
   solve x = variants [sort x] 
   hist = (shipping . head . solve) x
 in do
   mapM_ showBoat hist

