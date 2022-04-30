module Main where

import Lib
import System.IO
import Control.Monad (when)
import Control.Concurrent
import Data.List (sort, (\\))
import Consul

main :: IO ()
main = do
   putStr $ "q - quite,  \"_cgv\" as default >"++ gray ++"_cgv" ++ back 4 ++ norm
   hFlush stdout
   s <- getLine
   when ('q' `notElem` s) (main1 s)

-- main1 :: String -> IO ()
main1 s = let 
   s' = if null s then "_cgv" else s
 in do
   showFilm s'
   main



showFilm :: State -> IO ()
showFilm initState | (not . isValid . sort) initState = 
   putStrLn "impossible"

showFilm initState = let
   solve state = variants [sort state] 
   fstHist = (head . solve) initState
   movie = shipping fstHist
 in do
   putStr hideCur 
   putStr clrscr
   mapM_ showSeries (zip fstHist movie)
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
    
