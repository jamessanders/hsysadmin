import Data.String.Utils
import System.Directory
import System.Environment
import System.FilePath
import System.Find
import System.Process
import Text.Printf
import Text.Regex.Posix
import Text.Regex.Posix.Wrap

main = do
  args <- getArgs
  let ra = reverse . take 3 . reverse $ args
  let rx = ra !! 0
  let rt = ra !! 1
  let path = ra !! 2
  putStrLn $ printf " + Replace '%s' with '%s' in '%s'" rx rt path
  ------------------------------------------------------------------------
  files <- find path doesFileExist
  dirs  <- find path doesDirectoryExist
  mapM (doReplace rx rt) files
  mapM (doReplace rx rt) dirs

doReplace regex replacement fpath = do
  let fname = takeFileName  fpath
  let dname = takeDirectory fpath
  let checkName = fname =~ regex
  if checkName 
     then do
       let found  = fname =~ regex
       let rnamed = dname </> replace found replacement fname
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
     else return ()


askPermission str ifyes ifno = do
  putStrLn (str ++ " (y/n)")
  system "stty raw"
  system "stty -echo"
  answer <- getChar
  system "stty sane"
  system "stty echo"
  if (not $ answer `elem` "yn") 
    then do putStrLn "Answer must be 'y' or 'n'." 
            askPermission str ifyes ifno
    else if answer == 'y'
           then ifyes
           else ifno
  
  
