module ColorAnalysis where
import Graphics.GD
import Data.Digits
import qualified Data.Sequence as S
colorseq::Image->(Point,Point)->[Int]
colorseq img part= do
	let colorlist=[(\x<-getPixel (a,b) img)|a<-[fst$fst$part..snd$fst$part],b<-[fst$snd$part..snd$snd$part]]
        let colorlist2=map getcolor colorlist        
        let result=makeresult colorlist2
        return 2
getcolor::IO Color->Int
getcolor col=do
  c<-col
  let get0th (a,_,_,_) = a
  let get1th (_,a,_,_) = a
  let get2th (_,_,a,_) = a
  let rgb=toRGBA c
  let r=div(get0th rgb) 85
  let g=div(get1th rgb) 85
  let b=div(get2th rgb) 85
  let r1=if length (digits 2 r)<2 then 0:(digits 2 r) else (digits 2 r)
  let g1=if length (digits 2 g)<2 then 0:(digits 2 g) else (digits 2 g)
  let b1=if length (digits 2 b)<2 then 0:(digits 2 b) else (digits 2 b)
  return (unDigits 2 (r1++b1++g1))
makeresult::[Int]->[Int]
makeresult col=do
  let result=S.fromList[0|n<-[0..63]]
  map (S.adjust (+1) result) col
  return result