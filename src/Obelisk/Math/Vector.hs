module Obelisk.Math.Vector where

import Linear

rotation2 :: Float -> V2 (V2 Float)
rotation2 theta = V2 (V2 (cos theta) (-sin theta))
                     (V2 (sin theta) (cos theta))

vectorAngle :: V2 Float -> Float
vectorAngle (V2 x y)
  | y > 0 = atan2 y x
  | otherwise = 2 * pi + atan2 y x
