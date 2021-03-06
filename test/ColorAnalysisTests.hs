module ColorAnalysisTests where

import Test.HUnit
import Graphics.GD
import System.FilePath

import Utils
import ColorAnalysis

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

    
makeGraphTests = [ "[63,0,0,63]" ~: makegraph [63,0,0,63] @?= [2]++replicate 62 0++[2]
    , "[0..63]" ~: makegraph [0..63] @?= replicate 64 1
    , "replicate 100 1" ~: makegraph (replicate 100 1) @?= 0:100:replicate 62 0 
    , "[0..63]++[0..63]" ~: makegraph ([0..63]++[0..63]) @?= replicate 64 2 
    , "replicate 100 1 ++ replicate 100 63" ~: makegraph (replicate 100 1 ++ replicate 100 63) @?= 0:100:replicate 61 0 ++ [100 ]]
    
colorAnalysisTests = ["[10]++replicate 62 0++[10]" ~: coloranalysis ([10]++replicate 62 0++[10]) @?= Nothing
    , "[10]++replicate 63 0" ~: fmap toRGBA(coloranalysis ([10]++replicate 63 0)) @?=Just(0,0,0,0)
    , "replicate 63 0++[10]" ~: fmap toRGBA(coloranalysis (replicate 63 0++[10])) @?=Just(255,255,255,0) ]
    
testAverage :: FilePath->Color->IO Bool
testAverage path color=do
     img<-loadJpegFile $ "test/images/singlecolorimages" </> path <.> "jpg"
     colorimg<-averagecolor img
     return$ colorimg==color

averageColorTests = ["black pictute" ~: testAverage "0-0-0" (rgba 0 0 0 0)@?"fail"
		  , "0-170-0"~: testAverage "0-170-0" (rgba 0 170 0 0)@?"fail"
		  , "85-0-0"~: testAverage "85-0-0" (rgba 85 0 0 0)@?"fail"]
    
toColorTests = ["black" ~: tocolor 0 @?= rgba 0 0 0 0 
	     , "white" ~: tocolor 63 @?= rgba 255 255 255 0
	     , "red" ~: tocolor 48 @?= rgba 255 0 0 0 
	     , "green" ~: tocolor 12 @?= rgba 0 255 0 0
	     , "blue" ~: tocolor 3 @?= rgba 0 0 255 0
	     , "light gray" ~: tocolor 42@?= rgba 170 170 170 0 
	     , "dark gray" ~: tocolor 21@?=rgba 85 85 85 0]
	     