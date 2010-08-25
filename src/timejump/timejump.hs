import Control.Concurrent
import Control.Monad
import Data.Time
import System.Cmd
import System.Environment
import System.Posix.Signals
import System.Posix.Unistd

-- Sense when the system clock changes by more then n seconds and
-- run a command when that happens.

main = do
  args <- getArgs
  if args == [] 
    then do
      putStrLn "Usage: timejump <leap_seconds> <command>"
      return 1
    else do
      let leap = read $ head args 
      let comd = unwords $ tail args
      senseLeap leap comd
      return 0

  where 

    senseLeap n command = 
        senseLeap' (fromRational (toRational $ n + 0.001)) (round (n * 1000000.0)) command

    senseLeap' :: NominalDiffTime -- Offset
               -> Int             -- Sleep time
               -> String          -- Command
               -> IO ()
    senseLeap' off sleep command = do

         start <- getCurrentTime 
         usleep sleep
         end   <- getCurrentTime

         let diff = abs (diffUTCTime end start)

         when (diff > off) $ system command >> return ()
         senseLeap' off sleep command
