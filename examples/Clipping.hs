import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

example :: Diagram SVG
example = square 3
        # fc green
        # lw 5.0
        # clipBy (square 3.2 # rotateBy (1/10))

main = defaultMain example
