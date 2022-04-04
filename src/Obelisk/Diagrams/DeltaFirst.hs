{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (tri)

grid :: Int -> Int -> Diagram B
grid x y = frame <> lattice
  where s = unitSquare # lw thin
        frame = rect (fromIntegral x) (fromIntegral y) # lw thick
        lattice = centerXY . vcat . map hcat . replicate y . replicate x $ s

main = mainWith ((grid 3 3 <> example) :: Diagram B)

ray@(V2 rx ry) = V2 0.5 0.8
dfVert = V2 0 rx
dfHorz = V2 rx 0

drawV v = fromOffsets [v]

example = mconcat
  [ arrowV ray
  , translate (V2 rx 0) $ arrowV dfVert # lc red # lw thick
  , translate (V2 0 0) $ arrowV dfHorz # lc blue # lw thick
  ]
