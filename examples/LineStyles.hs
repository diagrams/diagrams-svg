import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

path = fromVertices (map p2 [(0,0), (1,0.3), (2,0), (2.2,0.3)]) # lw 0.1
example = centerXY . vcat' with { sep = 0.1 }
          $ map (path #)
            [ lineCap LineCapButt   . lineJoin LineJoinMiter
            , lineCap LineCapRound  . lineJoin LineJoinRound
            , lineCap LineCapSquare . lineJoin LineJoinBevel
            , dashing [0.1,0.2,0.3,0.1] 0
            ]

main = defaultMain (pad 1.1 example)