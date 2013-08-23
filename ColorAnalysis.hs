module ColorAnalysis where
import Graphics.GD.ByteString
analysis::Image->(Point,Point)->[RectRegion]
analysis img part= do
	