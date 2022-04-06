module Obelisk.Math.Homogenous where

import Linear
import Foreign.C.Types

mapAft :: V3 (V3 Float) -> V2 Float -> V2 CInt
mapAft t = dC . (fmap floor . (t !*)) . hC

hC :: (Num a) => V2 a -> V3 a
hC (V2 x y) = V3 x y 1

dC :: (Num a) => V3 a -> V2 a
dC (V3 x y _) = V2 x y

translateT :: (Num a) => a -> a -> V3 (V3 a)
translateT x y = V3 (V3 1 0 x)
                   (V3 0 1 y)
                   (V3 0 0 1)

zoomT :: (Num a) => a -> V3 (V3 a)
zoomT scale = V3 (V3 scale 0 0)
                (V3 0 scale 0)
                (V3 0 0 1)

rotationT :: Float -> V3 (V3 Float)
rotationT theta = V3 (V3 (cos theta) (-sin theta) 0)
                     (V3 (sin theta) (cos theta)  0)
                     (V3  0           0           1)
