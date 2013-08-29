module DownloaderTests where 

import Test.HUnit
import Downloader

loadContent a b c d k isJustOk = do
  mimg <- downloadHubble a b c d k  
  case mimg of
    Just _ -> return isJustOk
    Nothing -> return $ not isJustOk

downloads = ["Load Existing content" ~: loadContent 0 5 0 1 'a' @? "Load Existing content Failed" 
             ,"Load NonExisting content" ~ loadConten 0 7 0 8 'a' @? "Load NonExisting content Failed"]