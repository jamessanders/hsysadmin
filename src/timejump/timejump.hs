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
    senseLeap n command = do
         start <- getCurrentTime 
         usleep (n * 1000)
         end <- getCurrentTime
         let diff = abs (diffUTCTime end start)
         when (diff > fromIntegral (n + 1)) $ system command >> return ()
         senseLeap n command