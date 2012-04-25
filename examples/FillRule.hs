import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

loopyStar = fc red
          . mconcat . map (cubicSpline True)
          . pathVertices
          . star (StarSkip 3)
          $ regPoly 7 1
example = loopyStar # fillRule EvenOdd
      ||| strutX 1
      ||| loopyStar # fillRule Winding


main = defaultMain example
