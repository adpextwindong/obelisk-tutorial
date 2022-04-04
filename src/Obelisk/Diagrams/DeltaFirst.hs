{- LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (tri)

grid :: Int -> Int -> Diagram B
grid x y = frame <> lattice
  where s = unitSquare # lw thin
        frame = rect (fromIntegral x) (fromIntegral y) # lw thick
        lattice = centerXY . vcat . map hcat . replicate y . replicate x $ s

main = mainWith (grid 3 3 :: Diagram B)
