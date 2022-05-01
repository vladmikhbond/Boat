{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib (makeTrafficSt, isValid, State, Boat, mcgv, man, goods) where

import Data.List ( (\\), sort)

-- состояние - упоряд. множ. объектов, включая лодку, на левом берегу (множ на правом = разность)
type State = [Char]
type Hist  = [State]
type Boat  = [Char]

mcgv = "_cgv"
man : goods = mcgv
invalidStates = map sort ["gv", "cg", "cgv", "_c", "_v", "_"]

-- переходы: если лодка на лев берегу, удалить пустую лодку или лодку с любым объектом
-- если на правом, добавить пустую лодку или лодку с любым объектом с правого берега
getNextStates :: State -> [State]
getNextStates state = let
   thisBank = state \\ [man]
   otherBank = goods \\ state
   states = if man `elem` state
    then thisBank : [ thisBank \\ [x] | x <- thisBank]
    else (man : state) : [man : x : state | x <- otherBank]
 in
   map sort states

isValid :: State -> Bool
isValid state = state `notElem` invalidStates && 
   all (`elem` mcgv) state

doVariants :: Hist -> [Hist]
doVariants hist = do
   let nexts = filter isValid (getNextStates $ head hist)
   next <- filter (`notElem` hist) nexts
   if null next
     then return $ reverse ("" : hist)
     else doVariants (next : hist)

-- makeTraffic ["_cgv", "cv",  "_cv",   "c",  "_cg",  "g", "_g", ""]
--        ==>  [">_g" , "<_",  ">_v", "<_g",  ">_c", "<_", ">_g"]
makeTraffic :: Hist -> [Boat]
makeTraffic hist = tail $ zipWith f hist ("" : hist)
 where
   f x y | length x < length y = '>' : (y \\ x)
         | length x > length y = '<' : (x \\ y)
         | otherwise = error "" 
      

makeTrafficSt :: State -> [(State, Boat)]
makeTrafficSt initState = zip hist1 (makeTraffic hist1)
  where
   hist1 = head (doVariants [initState])

