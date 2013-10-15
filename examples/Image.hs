{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude

no = (circle 1 <> hrule 2 # rotateBy (1/8))
   # lw 0.2 # lc red
example = no <> image "phone.png" 1.5 1.5

main = defaultMain (example # centerXY # pad 1.1)
