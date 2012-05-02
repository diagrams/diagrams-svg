import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine

eff1 :: Diagram SVG R2
eff1 = text "F" # fc black <> square 1 # lw 0
ts  = [ scale (1/2), id, scale 2,    scaleX 2,    scaleY 2
      ,     scale (-1), scaleX (-1), scaleY (-1)
      ]

example1 = hcat . map (eff1 #) $ ts

eff2 :: Diagram SVG R2
eff2 = text "F" # fc black <> square 1 # lw 0 # lc white
example2 = (scaleX 2 `under` rotation (-1/8 :: CircleFrac)) eff2

eff :: Diagram SVG R2
eff = text "F" <> square 1 # lw 0
example = eff
       <> reflectAbout (p2 (0.2,0.2)) (rotateBy (-1/10) unitX) eff

main = defaultMain example1
