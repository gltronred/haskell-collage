module LoadFileTests where

import qualified Control.Exception as E (SomeException(..), catch)
import Test.HUnit

import Utils

errResult :: Bool -> E.SomeException -> IO Bool
errResult x e = return x

testLoad file = (loadFile file >> return True)`E.catch`errResult False

loadFileTests = [ "Load JPG" ~: testLoad "test/images/load-file.jpg" @? "JPG load failed"
                , "Load JPEG" ~: testLoad "test/images/load-file.jpeg" @? "JPEG load failed"
                , "Load PNG" ~: testLoad "test/images/load-file.png" @? "PNG load failed"
                , "Load GIF" ~: testLoad "test/images/load-file.gif" @? "GIF load failed"
                , "Load unknown" ~: ((loadFile "test/images/load-file.unknown" >> return False) `E.catch` errResult True) @? "Unknown file loaded (should fail)" ]


