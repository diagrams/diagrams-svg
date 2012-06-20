import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine

text' s t = text t # fontSize s <> strutY (s * 1.3)
example = centerXY $
      text' 10 "Hello" # fc black # italic
  === text' 5 "there"  # fc black # bold # font "freeserif"
  === text' 3 "world"  # fc green

main = defaultMain example
