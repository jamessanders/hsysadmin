import Control.Monad.State
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Network.HTTP
import Network.URI
import System.Environment
import System.Environment
import System.Exit
import Text.HTML.TagSoup
import qualified Control.Exception as E
import qualified Data.Map as M
------------------------------------------------------------------------

instance Ord URI where
    compare a b = compare (show a) (show b)

httpGet' :: URI -> IO (Maybe (Response String))
httpGet' uri = do
  if uriScheme uri == "https:" then
      return $ Just $ Response (2,0,0) "" [] "" 
      else do 
          xresp <- (simpleHTTP (buildRequest uri))
          case xresp of
            (Right resp) ->
                case rspCode resp of
                  (3,0,1) -> followRedirect resp
                  _       -> return (Just resp)
            (Left _) -> return Nothing
    where followRedirect r = do
                let (Just a) = findHeader HdrLocation r
                let (Just b) = parseURI a
                httpGet' b

withPage :: (URI -> Maybe (Response String) -> Maybe a ) -> URI -> IO (Maybe a)
withPage fn a = catch (httpGet' a >>= return . fn a) (\_-> return Nothing)


getPage = withPage aux
    where aux u (Just a) = Just (u,a)
          aux u Nothing  = Nothing

buildRequest :: URI -> Request String
buildRequest uri = Request{ rqURI = uri,
                            rqMethod = GET,
                            rqHeaders = [Header HdrUserAgent "Spidey 0.2"],
                            rqBody = "" }

------------------------------------------------------------------------

getAnchorHrefsFromPage :: Response String -> [String]
getAnchorHrefsFromPage = foldl parseLinks [] . parseTags . rspBody
    where parseLinks a (TagOpen "a" b) = case getHrefs b of
                                           (Just c) -> c:a
                                           Nothing  -> a
          parseLinks a _ = a
          getHrefs x = lookup "href" x 

getBrowsableLinksFromPage :: Response String -> [String]
getBrowsableLinksFromPage = nub . map removeHashes . getAnchorHrefsFromPage
                         where removeHashes = takeWhile (/= '#')

------------------------------------------------------------------------

parseLinkToURI = fullParse
    where fullParse link | isRelativeReference link = parseRelativeReference link
                         | isAbsoluteURI       link = parseAbsoluteURI link
                         | otherwise                = parseURI link
                            
getLocalLinks :: URI -> Response String -> [URI]
getLocalLinks base rsp = foldl onlyLocal [] . 
                         catMaybes . map combine . 
                         catMaybes . map parseLinkToURI . 
                         getBrowsableLinksFromPage $ rsp
    where combine a = relativeTo a base
          onlyLocal a b = if uriAuthority base == uriAuthority b then b:a else a

filterHTTPS = filter (\x->uriScheme x /= "https:") 

------------------------------------------------------------------------

utterFail = (0,0,0)

diff x1 x2 = reverse $ foldl (\a b-> if b `elem` x1 then a else b:a) [] x2

addToMap v m k = case M.lookup k m of
                   (Just a) -> M.update (\_-> Just (v:a)) k m
                   Nothing  -> M.insert k [v] m

crawlSite :: [URI] -> StateT (M.Map URI [URI], [(URI,ResponseCode)]) IO ()
crawlSite [] = lift $ putStrLn "Done"
crawlSite (u:xs) = do
  lift $ putStrLn (" + Reading Page: " ++ (show u))
  (m,st) <- get
  page   <- lift $ getPage u
  case page of 
    Nothing -> put (m,(u,utterFail):st) >> crawlSite xs
    (Just (uri,rsp)) -> let links = diff ((map fst st) ++ xs) 
                                         (filterHTTPS $ getLocalLinks uri rsp)
                            nm = foldl (addToMap u) m links in
                        put (nm,((u,rspCode rsp):st)) >> crawlSite (xs ++ links)
          
crawlUrl url = do 
  case parseURI url of
    Nothing -> putStrLn "Not a valid url"
    Just u  -> do
                 (m,crawl) <- execStateT (crawlSite [u]) (M.empty,[])
                 putStrLn $ "Crawled "++(show $ length crawl)++" pages."
                 if any failed crawl then do
                                       putStrLn "" 
                                       putStrLn $ replicate 72 '-' 
                                       putStrLn " - Possible broken links found!" 
                                       putStrLn $ replicate 72 '-' 
                                       putStrLn "" 
                                       mapM_ (printResult m) crawl
                                       putStrLn "" 
                                       exitFailure
                   else do
                     putStrLn " + No broken links found"
                     exitSuccess
        where printResult m a@(u,r) | failed a = (putStrLn $ (printR r) ++ ": "++ (show u)) 
                                           >> putStrLn (showRefs u m) 
                                           >> putStrLn ""
                              | otherwise = return ()
              showRefs u m = let refs = fromMaybe [] (M.lookup u m) in 
                             intercalate "\n" $ map (\x-> "   |- found at: "++(show x)) refs
              printR (a,b,c) = show a ++ show b ++ show c
              failed (_,(2,_,_)) = False
              failed (_,(3,_,_)) = False
              failed (_,_)       = True

             
main = getArgs >>= \x -> crawlUrl (x !! 0)

