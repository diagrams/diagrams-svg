{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

stops = mkStops [(gray, 0, 1), (white, 0.5, 1), (purple, 1, 1)]
gradient = mkLinearGradient stops ((-0.5) ^& 0) (0.5 ^& 0) GradPad
sq1 = square 1 # fillTexture  gradient
sq2 = square 1 # lineTexture (gradient & _LG . lGradSpreadMethod .~ GradRepeat
                                       & _LG . lGradStart        .~ (-0.1) ^& 0
                                       & _LG . lGradEnd          .~ 0.1 ^& 0
                             ) # lw ultraThick
sq3 = square 1 # fillTexture (gradient & _LG . lGradSpreadMethod .~ GradReflect
                                       & _LG . lGradStart        .~ (-0.1) ^& 0
                                       & _LG . lGradEnd          .~ 0.1 ^& 0
                             )

example :: Diagram B
example = hsep 0.25 [sq1, sq2, sq3]

main = mainWith $ example # frame 0.2
