module Main where

import Lib (mainBoat)
import System.IO

main = do
   putStr "\"_cgv\" as default > "
   hFlush stdout
   s <- getLine
   if null s
      then mainBoat "_cgv"
      else mainBoat s



