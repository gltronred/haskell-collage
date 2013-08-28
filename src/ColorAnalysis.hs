module ColorAnalysis where
import Graphics.GD
import Data.Digits
import Data.Bits
import Data.List
import Algorithm.Square
--making list of regions
colorseq::Image->(Point,Point)->IO [RectRegion]
colorseq img part=do
    colorlist<-mapM (\(a,b)->getPixel (a,b) img) $ zip [fst$fst$part..fst$snd$part] [snd$fst$part..snd$snd$part]--geting list of colors in picture
    let colorlist2=map getcolor colorlist --transforming [Color] to [Int[0..63] ]
    let graph=makegraph colorlist2 --transforming colorlist2 to [Int[0..63]], which is color graph
    let maxcolor=coloranalysis graph
    case maxcolor of
        Just f->return [RectRegion (fst$part) (snd$part) f]
        Nothing -> do
            size<-imageSize img
            let wid=fst size
            let hei=snd size
            let halfwid=fst size `div` 2
            let halfhei=snd size `div` 2
            let p0=((0,0),(halfwid-1,halfhei-1))
            let p1=((halfwid,0),(wid,halfhei-1))
            let p2=((0,halfhei),(halfwid-1,hei))
            let p3=((halfwid,halfhei),(wid,hei))
			-- fmap concat $ mapM (colorseq img) [p0,p1,p2,p3]
            if (wid*hei<75*75) 
                then do
                    color<-averagecolor img
                    return [RectRegion (fst$part) (snd$part) color]
                else mapM (colorseq img) [p0,p1,p2,p3] >>= return . concat			
--transform color to into Int[0..63]
getcolor::Color->Int
getcolor col=let
    get0th (a,_,_,_) = a
    get1th (_,a,_,_) = a
    get2th (_,_,a,_) = a
    rgb=toRGBA col
    r=div (get0th rgb) 85
    g=div (get1th rgb) 85
    b=div (get2th rgb) 85
    r1=if length (digits 2 r)<2 then 0:(digits 2 r) else (digits 2 r)
    g1=if length (digits 2 g)<2 then 0:(digits 2 g) else (digits 2 g)
    b1=if length (digits 2 b)<2 then 0:(digits 2 b) else (digits 2 b)
    in (unDigits 2 (r1++b1++g1))
--making graphics of colors in order of colors from 0 to 63
makegraph::[Int]->[Int]
makegraph col=let
    result= replicate 64 0
    result1= map (\x->length(elemIndices x col)) result
    in result1
-- analysing graphics,returning color, represented as Int[0..63], or Nothing
coloranalysis::[Int]->Maybe Color
coloranalysis col=let
    level=60
    points=fromIntegral (sum col)
    pctg=[(floor(fromIntegral(col!!n)/points*100),n)|n<-[0..63]]
    sortedpctg=reverse(sort pctg)
    maxlist=takeWhile (\x->fst x>level) sortedpctg
    color=snd$maxlist!!0
    result=if null maxlist then Nothing else Just$tocolor color
    in result
--getting average color in image
averagecolor::Image->IO Color
averagecolor img=do
    (wid,hei)<-imageSize img
    colorlist<-mapM (\(a,b)->getPixel (a,b) img) $ zip [0..wid] [0..hei]--geting list of colors in picture
    let colorlist2=map getcolor colorlist --transforming [Color] to [Int[0..63] ]
    let graph=makegraph colorlist2 --transforming colorlist2 to [Int], which is color graph
    let points=fromIntegral(sum graph)
    let pctg=[(floor(FromIntegral(graph!!n)/points*100),n)|n<-[0..63]]
    let result= tocolor(snd(maximum pctg))
    return result
--trasform from Int[0..63] to CInt color
tocolor::Int->Color
tocolor col=let
    b=col.&.3 * 85
    g=(shiftR (col.&.12) 2)*85
    r=(shiftR (col.&.48) 4)*85
    res=rgba r g b 0
    in res