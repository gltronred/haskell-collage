module DownloaderTests where 

import Test.HUnit
import Downloader

loadContent a b c d k isJustOk = do
  mimg <- downloadHubble a b c d k  
  case mimg of
    Just _ -> return isJustOk
    Nothing -> return $ not isJustOk

downloadsTests = ["Load Existing content" ~: loadContent 0 5 0 1 'a' True@? "Load Existing content Failed" 
             ,"Load NonExisting content" ~: loadContent 0 7 0 8 'a' False@? "Load NonExisting content Failed"]