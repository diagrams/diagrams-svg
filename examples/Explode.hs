import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine


spike :: Trail R2
spike = fromOffsets . map r2 $ [(1,3), (1,-3)]

burst = mconcat . take 13 . iterate (rotateBy (-1/13)) $ spike

colors = cycle [aqua, orange, deeppink, blueviolet, crimson, darkgreen]

example = lw 1.0
        . mconcat
        . zipWith lc colors
        . map stroke . explodeTrail origin
        $ burst

main = defaultMain (pad 1.1 example)