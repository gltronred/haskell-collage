
module ConverterTo64ColourTests where

import Graphics.GD.ByteString
import Test.HUnit
import ConverterTo64Colour
import Utils


roundingTests = [ "<43" ~: rounding 23 @?= 0
	, ">=43 && < 128" ~: rounding 127 @?= 85
	, ">=128 && < 213" ~: rounding 128 @?= 170
	, ">=213" ~: rounding 248 @?= = 255 ]
	
Color clr <- rgba (23, 127, 128, 0)
convertTests = [ "color conversion" ~: convert clr @?= rgba (0, 85, 170, 0) ]
	
image = loadFile test/images/for-conversion.png
converterTests = [ "image conversion" ~: converter image @?= test/images/converted.png ]