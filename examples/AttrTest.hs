import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

b1 = square 20 # lw 0.002

main = defaultMain (pad 1.1 b1)
