module ColorAnalysisTests where

import Test.HUnit

import Utils
import ColorAnalysis

--img = loadFile "test\images\singlecolorimages\000.jpg"
--tests include any opacity
getColorTests = [ "Pure white" ~: getcolor (rgb 0 0 0) @?= 0
    , "Pure white with opacity" ~: getcolor (rgba 0 0 0 0) @?= 0
    , "Pure blue" ~: getcolor (rgb 0 0 255) @?= 3
    , "Pure blue with opacity" ~: getcolor (rgba 0 0 255 0) @?= 3
    , "Pure green" ~: getcolor (rgb 0 255 0) @?= 12
    , "Pure green with opacity" ~: getcolor (rgba 0 0 255 0) @?= 12
    , "Pure red" ~: getcolor (rgba 255 0 0 0) @?= 48
    , "Pure black" ~: getcolor (rgba 255 255 255 0) @?= 63
    , "Dark gray" ~: getcolor (rgba 170 170 170 0) @?= 42
    , "Light gray" ~: getcolor (rgba 85 85 85 0) @?= 21 ]
makeGraphTests =