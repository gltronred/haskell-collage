module Utils (loadFile) where

import Graphics.GD.ByteString

loadFile :: FilePath -> IO Image
loadFile path = let
  extension = takeExtension path
  in case extension of
    "jpg" -> loadJpegFile path
    "jpeg" -> loadJpegFile path
    "png" -> loadPngFile path
    "gif" -> loadGifFile path
    _ -> error "Unknown file format"