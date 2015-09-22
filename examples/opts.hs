import qualified Data.Text as T
import qualified Lucid.Svg as LST
import Diagrams.Prelude
import Diagrams.Backend.SVG

svgOpt :: Num n => Options SVG V2 n
svgOpt = SVGOptions {
  _size = mkSizeSpec $ V2 (Just 400) (Just 400),
  _idPrefix = T.empty,
  _svgDefinitions = Nothing,
  _svgAttributes = [LST.preserveAspectRatio_ $ T.pack "xMinYMin"],
  _generateDoctype = True
}

diagram :: Diagram B
diagram = circle 1 # fc orange # lw ultraThick # lc blue # frame 0.2

main :: IO ()
main = renderSVG' "opts.svg" svgOpt diagram
