module LoadFileTests where

import Control.Exception (catch)
import Utils

loadFileTests = [ "Load JPG" ~: loadFile "test/images/load-file.jpg" @?= ()
                , "Load JPEG" ~: loadFile "test/images/load-file.jpeg" ~?= ()
                , "Load PNG"  ~: loadFile "test/images/load-file.png" @?= ()
                , "Load GIF" ~: loadFile "test/images/load-file.gif" @?= ()
                , "Load unknown (should fail)" ~: (loadFile "test/images/load-file.unknown" >> return False `catch` \e -> return True) @?=True ]


