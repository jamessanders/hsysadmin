import Control.Exception (bracket)
import Control.Monad
import Data.String.Utils
import System.Directory
import System.Environment
import System.FilePath
import System.Find
import System.Process
import System.Exit
import System.IO
import Text.Printf
import Text.Regex.Posix.Wrap

main :: IO ()
main = do
  args <- getArgs
  when (length args < 3) (usage >> exitFailure)
  let ra = reverse . take 3 . reverse $ args
  let rx = head ra
  let rt = ra !! 1
  let path = ra !! 2
  ------------------------------------------------------------------------
  files <- find path doesFileExist
  dirs  <- find path doesDirectoryExist
  mapM_ (\(f,n)->doReplace rx rt n f) (zip files [1..])
  mapM_ (\(f,n)->doReplace rx rt n f) (zip dirs  [1..])

numReplace :: [String] -> String -> String
numReplace ls str = replaceNumbered ls str 1
                    where
                      replaceNumbered [] str' _ = str'
                      replaceNumbered (x:xs) str' n = 
                          replace ("%" ++ show n) x (replaceNumbered xs str' (n+1))

doReplace regex replacement n fpath = do
  let fname = takeFileName  fpath
  let dname = takeDirectory fpath
  let checkName = fname =~ regex
  when checkName $
     do let found   = fname =~ regex :: [[String]]
        let rep     = numReplace (tail $ head found) $ replace "%n" (show n) replacement
        let rnamed  = dname </> replace (head $ head found) rep fname
        askPermission 
          (printf "Move '%s' to '%s'?" fpath rnamed) 
          (do fe <- doesFileExist rnamed
              de <- doesDirectoryExist rnamed
              isDir <- doesDirectoryExist fpath
              if fe || de 
                 then putStrLn " - File exist, skipping." 
                 else do putStrLn $ printf "%s -> %s" fpath rnamed
                         if isDir 
                            then renameDirectory fpath rnamed
                            else renameFile fpath rnamed) 
          (putStrLn " - Skipping")
     
getChar' :: IO Char
getChar' = bracket 
              (system "stty raw"  >> system "stty -echo")
              (\_-> system "stty sane" >> system "stty echo")
              (\_-> getChar)


askPermission :: String -> IO () -> IO () -> IO ()
askPermission str ifyes ifno = do
  putStrLn (str ++ " (y/n)")
  answer <- getChar'
  if (answer `notElem` "yn")
     then do putStrLn "Answer must be 'y' or 'n'." 
             askPermission str ifyes ifno
     else if (answer == 'y') then ifyes else ifno 


usage = do 
  progname <- getProgName
  hPutStrLn stderr 
    $ printf "Usage: %s <find_regex> <replacement> <filepath>" progname