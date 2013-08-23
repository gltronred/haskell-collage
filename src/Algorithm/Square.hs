module Algorithm.Square where

import Graphics.GD.ByteString

data RectRegion = RectRegion { rrTopLeft :: Point
                             , rrDownRight :: Point
                             , rrColor :: Color
                             } deriving (Eq,Show)

instance Algorithm RectRegion where
  toRegions = undefined
  findSimilar = undefined
  fromRegions = undefined

