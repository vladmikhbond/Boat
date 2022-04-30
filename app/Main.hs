module Main where

import Lib
import System.IO
import Control.Monad (when)
import Control.Concurrent
import Data.List (sort)
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
   mapM_ showSeries movie

showSeries :: String -> IO()  -- "<_cgv"
showSeries (c : _ : cs) = do
   mapM_ (showCadr load) way
   threadDelay 500000
 where 
   way = if c == '>' then [10..50] else [50,49..10] 
   load = if null cs then "_" else cs 
   
showCadr load col  = do
   putStr clrscr
   putStr $ "|||||||||" ++ rc 0 55 ++ "||||||||||"
   --putStrLn  $ rc 0 col ++ "\\_"++ load ++"_/"
   putStr $ rc 0 col ++ "\\___/"
   putStrLn $ back 3 ++ load
   --hFlush stdout
   threadDelay 50000
