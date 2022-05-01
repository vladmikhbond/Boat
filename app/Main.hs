module Main where

import Lib
import System.IO
import Control.Monad (unless)
import Control.Concurrent
import Data.List (sort, (\\))
import Consul

main :: IO ()
main = do
   putStr $ "q - quite >"++ gray ++"    " ++ mcgv ++ back 8 ++ norm
   hFlush stdout
   init <- getLine
   unless ('q' `elem` init) (do
      let init' = if null init then mcgv else sort init
      if not (isValid init') 
       then putStrLn "impossible"
       else showTraffic (makeTrafficSt init') 
      main)

showTraffic :: [(State, Boat)] -> IO ()
showTraffic traffic = do
   putStr hideCur
   putStr clrscr
   mapM_ showRoute traffic
   putStrLn showCur

(bL, bR) = (5, 44) 

--          ("_cgv", ">_g")
showRoute :: (State, Boat) -> IO()
showRoute (state, boat) = do
   -- перед погрузкой
   drawGoods onLeft onRight 
   showBoat "_" dir (if dir == '>' then bL else bR)
   pause
   -- после погрузки
   drawGoods (onLeft \\ load) (onRight \\ load)
   showBoat load dir (if dir == '>' then bL else bR)
   pause
   -- проход лодки
   mapM_ (showBoat load dir) dist
   -- перед разгрузкой
   pause
   -- после разгрузки
   if dir == '<' 
    then drawGoods (onLeft ++ load) (onRight \\ load)
    else drawGoods (onLeft \\ load) (onRight ++ load)
   showBoat "_" dir (if dir == '<' then bL else bR)

 where
   (dir : _ : load) = boat
   drawGoods l r = putStr $ 
      rc 1 1  ++ take 5 (l ++ "      ") ++
      rc 1 (bR + 6) ++ take 5 (r ++ "      ")  
   pause =  threadDelay 500000   
   onLeft = state \\ [man]
   onRight = goods \\ onLeft
   dist = if dir == '>' then [bL..bR] else [bR,(pred bR)..bL]
   

showBoat :: 
   String    -- load
   -> Char   -- dir: '>' , '<' 
   -> Int    -- x-coord
   -> IO ()
showBoat load dir col  = do
   let d = if dir == '>' then -1 else 1
   putStr $ rc 1 (col + d) ++ "     " ++ rc 1 col ++ "\\___/" ++ back 3 ++ load
   hFlush stdout
   threadDelay 50000
    