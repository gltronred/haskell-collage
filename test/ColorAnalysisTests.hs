module ColorAnalysisTests where

import Test.HUnit
import Graphics.GD

import Utils
import ColorAnalysis

zeroes=replicate 62 0
--img = loadFile "test\images\singlecolorimages\000.jpg"
getColorTests = [ "Pure white" ~: getcolor (rgb 0 0 0) @?= 0
    , "Pure white with opacity" ~: getcolor (rgba 0 0 0 0) @?= 0
    , "Pure blue" ~: getcolor (rgb 0 0 255) @?= 3
    , "Pure blue with opacity" ~: getcolor (rgba 0 0 255 0) @?= 3
    , "Pure green" ~: getcolor (rgb 0 255 0) @?= 12
    , "Pure green with opacity" ~: getcolor (rgba 0 255 0 0) @?= 12
    , "Pure red" ~: getcolor (rgb 255 0 0) @?= 48
    , "Pure red with opacity" ~: getcolor (rgba 255 0 0 0) @?= 48
    , "Pure black" ~: getcolor (rgba 255 255 255 0) @?= 63
    , "Dark gray" ~: getcolor (rgba 170 170 170 0) @?= 42
    , "Light gray" ~: getcolor (rgba 85 85 85 0) @?= 21 ]

    
makeGraphTests = [ "[63,0,0,63]" ~: makegraph [63,0,0,63] @?= [2]++zeroes++[2])
    , "[0..63]" ~: makegraph [0..63] @?= replicate 64 1
    , "replicate 100 1" ~: makegraph (replicate 100 1) @?= 0:100:zeroes ]
    