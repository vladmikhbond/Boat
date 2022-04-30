module Main where

import Lib
import System.IO
import Control.Monad (unless)
import Control.Concurrent
import Data.List (sort, (\\))
import Consul

main :: IO ()
main = do
   putStr $ "q - quite >"++ gray ++"    _cgv" ++ back 8 ++ norm
   hFlush stdout
   init <- getLine
   unless ('q' `elem` init) (do
      let init' = if null init then "_cgv" else sort init
      if not (isValid init') 
       then putStrLn "impossible"
       else showCargo (makeCargo init') 
      main)

showCargo y = do
   putStr hideCur
   putStr clrscr
   mapM_ showSeries y
   putStrLn showCur
 

--            ("_cgv", ">_g")
showSeries :: (String, String) -> IO()
showSeries (onBank, boatStr) = do
   -- перед погрузкой
   putStr $ rc 1 1  ++ take 5 (onBank' ++ "      ") ++
            rc 1 50 ++ take 5 (onRight ++ "      ")
   threadDelay 1000000
   -- перед проходом
   putStr $ rc 1 1  ++ take 5 (onLeft ++ "      ")
   showBoat "_" d (if d == '>' then 6 else 44)
   -- проход лодки
   mapM_ (showBoat load d) way
   -- после прохода
   putStr $ rc 1 50 ++ take 5 (onRight ++ "      ")
   showBoat load d (if d == '<' then 6 else 44)
   -- после разгрузки
   putStr $ rc 1 50 ++ take 5 (onRight ++ load ++ "      ")
   showBoat "_" d (if d == '<' then 6 else 44)
   threadDelay 1000000
 where
   (d : _ : load) = boatStr
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

