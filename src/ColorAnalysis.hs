module ColorAnalysis where
import Graphics.GD
import Data.Digits
import qualified Data.Sequence as S
colorseq::Image->(Point,Point)->IO [Int]
colorseq img part=do
	colorlist<-mapM (\(a,b)<-getPixel (a,b) img) $ zip [fst$fst$part..snd$fst$part] [fst$snd$part..snd$snd$part]
        let colorlist2=map getcolor colorlist        
        let result=makeresult colorlist2
        return result 
getcolor::Color->Int
getcolor col=let
  get0th (a,_,_,_) = a
  get1th (_,a,_,_) = a
  get2th (_,_,a,_) = a
  rgb=toRGBA c
  r=div(get0th rgb) 85
  g=div(get1th rgb) 85
  b=div(get2th rgb) 85
  r1=if length (digits 2 r)<2 then 0:(digits 2 r) else (digits 2 r)
  g1=if length (digits 2 g)<2 then 0:(digits 2 g) else (digits 2 g)
  b1=if length (digits 2 b)<2 then 0:(digits 2 b) else (digits 2 b)
  in (unDigits 2 (r1++b1++g1))
makeresult::[Int]->[Int]
makeresult col=do
  let result=S.fromList[0|n<-[0..63]]
  
  return result