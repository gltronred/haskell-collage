{-# LANGUAGE TypeFamilies #-}

module Types where

import Data.Word
import Codec.Picture

-- TODO: Change to more appropriate type
type Coord = (Double, Double)
type Color = (Word8, Word8, Word8)
type Point = (Coord, Color)
----------------------------------------

class Algorithm region where
  toRegions :: DynamicImage -> [region]
  findSimilar :: [DynamicImage] -> region -> (region, Int)
  fromRegions :: [(region,Int)] -> DynamicImage



