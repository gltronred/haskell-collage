import Graphics.GD.ByteString
import Control.Monad
import Utils

-- всё изображение
converter :: Image -> IO Image
converter image = do
        (w,h) <- imageSize image
        forM_ [(i,j)|i<-[0..w], j<-[0..h]] $ \cur -> do
        	let currentPoint = cur
		pixelColor <- getPixel image currentPoint -- тип pixelColor - Color
		setPixel currentPoint (convert pixelColor)

-- один пиксель
convert :: Color -> Color
convert pixelColor = let
	(r,g,b,a) = toRGBA pixelColor
	in rgb (rounding r) (rounding g) (rounding b) a
        
rounding :: Int -> Int -- или case
rounding num | num < 43 = 0
	| num >=43 && num < 128 = 85
	| num >=128 && num < 213 = 170
	| num >=213 = 255
