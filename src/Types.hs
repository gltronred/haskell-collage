{-# LANGUAGE TypeFamilies #-}

module Types where

import Data.Word

-- TODO: Change to more appropriate type
type Coord = (Double, Double)
type Color = (Word8, Word8, Word8)
type Point = (Coord, Color)
type Image = [Point]
----------------------------------------

class Algorithm region where
  toRegions :: Image -> [region]
  findSimilar :: [Image] -> region -> (region, Int)
  fromRegions :: [(region,Int)] -> Image



