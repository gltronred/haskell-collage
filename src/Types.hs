{-# LANGUAGE TypeFamilies #-}

module Types where

import Data.Word

-- TODO: Change to more appropriate type
type Coord = (Double, Double)
type Color = (Word8, Word8, Word8)
type Point = (Coord, Color)
type Image = [Point]
----------------------------------------

class Algorithm where
  data Region :: *
  data RegionWithImage :: *
  toRegions :: Image -> [Region]
  findSimilar :: [Image] -> Region -> RegionWithImage
  fromRegions :: [RegionWithImage] -> Image



