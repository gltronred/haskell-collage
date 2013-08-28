import Utils

image = loadFile src/img.jpeg

-- всё изображение
converter :: Image -> IO Image
converter image = do
	pixelColor <- getPixel image currentPoint -- тип pixelColor - Color
	setPixel currentPoint (convert pixelColor)

-- один пиксель
convert :: Color -> Color
convert pixelColor = do
	pixelColor <- toRGBA pixelColor
	pixelColor (r g b a) <- rgb (pixelColor (rounding r rounding g rounding b a))
        
rounding :: Int -> Int -- или case
ronding num | num < 43 = 0
	| num >=43 && num < 128 = 85
	| num >=128 && num < 213 = 170
	| num >=213 = 255

currentPoint :: ([Int], [Int])
currentPoint <- Point ([0..fst Size image], [0..snd Size image])
