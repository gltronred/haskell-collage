module ColorAnalysisTests where

import Test.HUnit

import Utils
import ColorAnalysis

--img = loadFile "test\images\singlecolorimages\000.jpg"

getColorTests = [ "Pure white" ~: getcolor (rgb 0 0 0) @?= 0
	, "Pure white with opacity" ~: getcolor (rgba 0 0 0 0) @?= 0]