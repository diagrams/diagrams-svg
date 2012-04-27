import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

example = text "Hello world!" <> rect 8 1

main = defaultMain example
