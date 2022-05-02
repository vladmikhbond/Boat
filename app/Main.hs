module Main where

import Lib
import System.IO ( hFlush, stdout)
import Control.Monad (unless)
import Control.Concurrent ( threadDelay )
import Data.List (sort, (\\))
import Consul
import System.Console.Haskeline

import Control.Monad.IO.Class


repl :: [Char] -> InputT IO [Char]
repl cs = do
  minput <- getInputChar ""
  
  case minput of
    Nothing -> return cs
    Just '.' -> return cs
    Just c ->  do 
       liftIO (putStr (rc 1 (length cs + 2))) 
       repl (c : cs)   

ma = do
   putStr clrscr
   xx <- runInputT defaultSettings (repl "")
   putStr xx

main = do
   putStrLn "What are on the left bank?"
   putStr $ green ++ "c" ++ norm ++ "-cabbage, " 
         ++ blue ++ "g" ++ norm ++ "-goat, "
         ++ red ++ "w" ++ norm ++ "-wolf, b-boat, q-quite >"
         ++ gray ++"    " ++ "bcgv" ++ back 8 ++ norm
   hFlush stdout
   init' <- getLine
   let init = map (\x -> if x == 'b' then '_' else x) init'

   unless ('q' `elem` init) (do
      let init' = if null init then bcgv else sort init
      if not (isValid init')
       then putStrLn "impossible"
       else drowTraffic (makeTrafficSt init')
      main)

drowTraffic :: [(State, Boat)] -> IO ()
drowTraffic traffic = do
   putStr hideCur
   putStr clrscr
   mapM_ drowRoute traffic
   putStrLn showCur

(bL, bR) = (4, 34)

--          ("_cgv", ">_g")
drowRoute :: (State, Boat) -> IO()
drowRoute (state, boat) = do
   -- перед погрузкой
   drawBanks onLeft onRight
   drowBoat "_" dir (if dir == '>' then bL else bR)
   pause
   -- после погрузки
   drawBanks (onLeft \\ load) (onRight \\ load)
   drowBoat load dir (if dir == '>' then bL else bR)
   pause
   -- проход лодки
   mapM_ (drowBoat load dir) dist
   -- перед разгрузкой
   pause
   -- после разгрузки
   if dir == '<'
    then drawBanks (onLeft ++ load) (onRight \\ load)
    else drawBanks (onLeft \\ load) (onRight ++ load)
   drowBoat "_" dir (if dir == '<' then bL else bR)
 where
   (dir : _ : load) = boat
   pause =  threadDelay 500000
   onLeft = state \\ [man]
   onRight = goods \\ onLeft
   dist = if dir == '>' then [bL..bR] else [bR,(pred bR)..bL]


drowBoat ::
   String    -- load
   -> Char   -- dir: '>' , '<' 
   -> Int    -- x-coord
   -> IO ()
drowBoat load dir col  = do
   let d = if dir == '>' then -1 else 1
   putStr $ rc 1 (col + d) ++ "     " ++ rc 1 col ++ "\\___/" ++ back 3 ++ showColored load
   hFlush stdout
   threadDelay 50000

drawBanks l r = putStr $ showColored $ l' ++ r'
 where
   l' = rc 1 1        ++ take 5 (l ++ "      ")
   r' = rc 1 (bR + 6) ++ take 5 (r ++ "      ")


showColored cs = concatMap showC cs ++ norm
 where
   showC 'c' = green ++ "c"
   showC 'g' = blue ++ "g"
   showC 'w' = red ++ "w"
   showC x = [x]



