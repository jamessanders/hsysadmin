{-# LANGUAGE RankNTypes #-}
-- Chronos v0.2.8
-- Author: James Sanders <jimmyjazz14@gmail.com>

import Data.Time 
import Data.List
import Data.Char
import System.Cmd
import System.Exit
import System.Locale
import System.Environment
import System.Posix.Signals
import Control.Concurrent

version :: String
version = "v0.2.8"

-- | Get time now
getTimeNow :: IO LocalTime
getTimeNow = zonedTimeToLocalTime `fmap` getZonedTime

-- | Get day of the week
getWeekDay :: IO String
getWeekDay = formatTime defaultTimeLocale "%a" `fmap` getZonedTime
  
-- | Get Tomarrow
getTomarrow :: IO Day
getTomarrow = do 
  tn <- getCurrentTime
  tz <- getCurrentTimeZone 
  return . localDay . utcToLocalTime tz  $
         tn { utctDay = addDays 1 $ utctDay tn }

runCmd :: [String] -> IO ExitCode
runCmd = system . intercalate " " 

runCmd' :: [String] -> IO (Maybe ExitCode)
runCmd' x = Just `fmap` runCmd x

daily :: String -> IO (Maybe ExitCode) -> IO ()
daily arg fn = do
  timeNow <- getTimeNow -- get time now

  -- split arguments
  let args = split ',' arg

  -- parse the arguments in tod
  let parsed = map (\t->let [h,m] = split ':' t in TimeOfDay (read h) (read m) 0) args

  -- map times to today and then sort them
  let times = sort . map (LocalTime (localDay timeNow)) $ parsed

  -- get only times that occur in the future
  let future = dropWhile (< timeNow) (sort times)
  tz <- getCurrentTimeZone -- get current timezone

  -- if all times are in the past add 24hrs to the first time in the list...
  final <- case future of
             [] -> getTomarrow >>= \x -> return $ (head times){ localDay = x }
             otherwise -> return . head $ future

  -- convert the upcoming date and current time to POSIX seconds
  let next = (localTimeToUTC tz) final
  let now  = (localTimeToUTC tz) timeNow

  -- find the differences of the two times
  let fsleep = diffUTCTime next now

  -- print fsleep
  -- putStrLn $ "Will run command next at "++(show $ utcToLocalTime tz $ addUTCTime fsleep now)

  -- sleep then run the command
  threadDelay (ceiling fsleep * 1000000) >> fn >> threadDelay 1000000
  
  -- start it all over again
  daily arg fn

runWeekly :: [String] -> IO ExitCode -> IO (Maybe ExitCode)
runWeekly days fn = do
  x <- getWeekDay 
  if x `elem` days 
   then Just `fmap` fn 
   else return Nothing

-- | The interval command (just sleep for i seconds then run cmd)
sleepNfork i cmd = do
  threadDelay (i * 1000000)
  res <- runCmd cmd
  case res of
    ExitSuccess -> sleepNfork i cmd
    otherwise   -> exitFailure  

-- | Handle the SIGALRM signal
wakeHandler :: [String] -> IO ()
wakeHandler cmd = runCmd cmd >> return ()

-- | Install the handler for SIGALRM
setWakeupSignal :: [String] -> IO Handler
setWakeupSignal cmd = installHandler sigALRM (Catch (wakeHandler cmd)) Nothing

usage :: IO ()
usage = ps "Usage: chronos <sub-command> [options] <command-to-run>" >>
        ps "Sub Commands:" >>
        ps "               daily    <times-of-day>" >>
        ps "               weekly   <days-of-week> <times-of-day>" >>
        ps "               interval <interval-to-run-at>\n\n" >>
        ps "<times-of-day> should be in the format HH:MM (12:00,16:00)">>
        ps "<days-of-week> should be in one of the abbrivated formats: (Sun,Mon,Tue,Wed,Thu,Fri,Sat)"
        where ps = putStrLn

failMessage :: forall a. a -> IO ()
failMessage _ = usage

main :: IO ()
main = do
  args <- getArgs
  if null args then usage else makecall args
  where makecall args = 
          case head args of
            "daily"   -> if length args < 3
                          then usage 
                          else catch (setWakeupSignal (drop 2 args) >> 
                                      daily (args !! 1) (runCmd' (drop 2 args))) failMessage
            "weekly"  -> if length args < 4
                          then usage
                          else catch (setWakeupSignal (drop 3 args) >> 
                                      daily (args !! 2) 
                                            (runWeekly 
                                             (map capitalize $ split ',' $ args !! 1) 
                                             (runCmd (drop 3 args))
                                            )
                                     ) failMessage
            "interval"-> catch (setWakeupSignal (drop 2 args) >> 
                                sleepNfork (read $ args !! 1) (drop 2 args)) failMessage
            "version" -> putStrLn ("Version: "++version)
            _         -> usage
  


------------------------------------------------------------------------

-- | split a string into many smaller string at delimiter
split :: Char -> String -> [String]
split delim s
    | [] == rest = [token]
    | otherwise = token : split delim (tail rest)
    where (token,rest) = span (/=delim) s

-- | capitalize a string
capitalize :: String -> String
capitalize x = (:) (toUpper . head $ x) (strToLower . tail $ x)

-- | make string all lowercase
strToLower :: String -> String
strToLower = map toLower

-- | make string all uppercase
strToUpper :: String -> String
strToUpper = map toUpper
