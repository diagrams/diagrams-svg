import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine

example1 :: Diagram SVG R2
example1 = text "Hello world!" # fc black <> rect 8 2

eff2 :: Diagram SVG R2
eff2 = text "F" # fc black <> square 1 # lw 0
example2 = eff2 # rotateBy (1/7)

eff :: Diagram SVG R2
eff = text "F" # fc black <> square 1 # lw 0 # lc white
rs  = map rotateBy [1/7, 2/7 .. 6/7]
example = hcat . map (eff #) $ rs

main = defaultMain example
