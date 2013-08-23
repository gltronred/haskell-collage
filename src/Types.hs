{-# LANGUAGE TypeFamilies #-}

module Types where

import Data.Word
import Graphics.GD.ByteString

class Algorithm region where
  toRegions :: Image -> [region]
  findSimilar :: [Image] -> region -> (region, Int)
  fromRegions :: [(region,Int)] -> Image



