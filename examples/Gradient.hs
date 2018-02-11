{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend
import Diagrams.Backend.SVG

stops = [(gray, 0), (white, 0.5), (purple, 1)]
gradient = mkLinearGradient stops (P2 (-0.5) 0) (P2 0.5 0)
sq1 = square 1 # fillTexture  gradient
sq2 = square 1 # lineTexture (gradient & gradientSpreadMethod .~ GradRepeat
                                       & gradientStart        .~ P2 (-0.1) 0
                                       & gradientEnd          .~ P2 0.1 0
                             ) # lw ultraThick
sq3 = square 1 # fillTexture (gradient & gradientSpreadMethod .~ GradReflect
                                       & gradientStart        .~ P2 (-0.1) 0
                                       & gradientEnd          .~ P2 0.1 0
                             )

example :: Diagram V2
example = hsep 0.25 [sq1, sq2, sq3]

main = mainWith SVG $ example # frame 0.2
