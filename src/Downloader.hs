module Downloader where

import  Network.HTTP.Conduit.Downloader
import qualified Data.ByteString as B
import Data.Maybe (fromJust,isJust)
import qualified Control.Exception  as E 
import Control.Monad

downloadHubble :: Int -> Int -> Int -> Int -> Char-> IO (Maybe B.ByteString)

downloadHubble a b c d k= (fmap Just$urlGetContents ("http://www.spacetelescope.org/static/archives/images/large/heic"++show a++show b++show c++show d++""++[k]++".jpg")) `E.catch` handler
                      where
                        handler exc = do
                          return Nothing `const` (exc :: E.SomeException)


main = do
   forM_ [(a,b,c,d,k)|a<-[0,1,9],b<-[0..9],c<-[0..9],d<-[0..9],k<-['a'..'c']] $ \(a,b,c,d,k) -> do
     mf <- downloadHubble a b c d k 
     putStrLn $ show $ isJust mf
     case mf of
       Just f -> do
         putStr "# "
         B.writeFile ("images/img-"++show n++".jpg") f
         return ()
         --putStrLn("images/img-"++show n++".jpg")
       Nothing ->putStr("! ")