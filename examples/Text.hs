
module Main where

import Diagrams.Prelude
import Diagrams.TwoD.Text
import Geometry
import Diagrams.Backend
import Diagrams.Backend.SVG

example1 :: Diagram V2
example1 = text "Hello world!" # fc black # fontSize 12 <> rect 8 2

eff2 :: Diagram V2
eff2 = text "F" # fc black <> square 1 # lw 0 # fontSize 12
example2 = eff2 # rotateBy (1/7)

eff :: Diagram V2
eff = text "F" # fc black <> square 1 # lw 0 # lc white
rs  = map rotateBy [1/7, 2/7 .. 6/7]
example = hcat . map (eff #) $ rs

main = mainWith SVG example
