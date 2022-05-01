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

showTraffic y = do
   putStr hideCur
   putStr clrscr
   mapM_ showRoute y
   putStrLn showCur
 

--          ("_cgv", ">_g")
showRoute :: (State, Boat) -> IO()
showRoute (onBank, boat) = do
   -- перед погрузкой
   drawL onBank'
   drawR onRight 
   threadDelay 1000000
   -- перед проходом
   drawL onLeft
   showBoat "_" d (if d == '>' then 6 else 44)
   -- проход лодки
   mapM_ (showBoat load d) way
   -- после прохода
   drawR onRight
   showBoat load d (if d == '<' then 6 else 44)
   -- после разгрузки
   drawR $ onRight ++ load
   showBoat "_" d (if d == '<' then 6 else 44)
   threadDelay 1000000
 where
   drawL x = putStr $ rc 1 1  ++ take 5 (x ++ "      ")
   drawR x = putStr $ rc 1 50 ++ take 5 (x ++ "      ")  
   (d : _ : load) = boat
   onBank' = onBank \\ ['_']
   way = if d == '>' then [6..44] else [44,43..6]
   onLeft = onBank \\ ('_' : load)
   onRight = ("cgv" \\ onLeft) \\ load

showBoat :: String -> Char -> Int -> IO ()
showBoat load dir col  = do
   putStr $ rc 1 (col + d) ++ "     " ++ rc 1 col ++ "\\___/" ++ back 3 ++ load
   hFlush stdout
   threadDelay 50000
 where
    d = if dir == '>' then -1 else 1

