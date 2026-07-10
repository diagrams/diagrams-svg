
module Main where

import Diagrams.Prelude
import Diagrams.Backend
import Diagrams.TwoD.Text
import Diagrams.Backend.SVG

text' s t = text t # fontSizeL s <> strutY (s * 5.3)
example :: Diagram V2
example = center $
      text' 10 "Hello" # fc black # italic
  === text' 5 "there"  # fc black # bold # font "sans-serif"
  === text' 3 "world"  # fc green

main = mainWith SVG (example <> square 30 # fcA transparent)
