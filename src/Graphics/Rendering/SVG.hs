module Graphics.Rendering.SVG
    ( Render(..)
    , svgHeader
    , svgFooter
    , renderPath
    , renderEllipse
    , renderText
    , Attribute(..)
    , renderAttrs
    ) where

-- from base
import Data.List (intersperse)

-- from diagrams-lib
import Diagrams.Prelude hiding (Render, Attribute, close, e)
import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Text
import Diagrams.TwoD.Path

-- from blaze-builder
import qualified Blaze.ByteString.Builder as B
import qualified Blaze.ByteString.Builder.Char8 as B8
import qualified Blaze.ByteString.Builder.Html.Utf8 as BH

-- from blaze-textual
import qualified Blaze.Text as BT


newtype Render = R { unR :: T2 -> B.Builder }

instance Monoid Render where
    mempty = builder mempty
    x `mappend` y = R $ \t -> unR x t `mappend` unR y t

builder :: B.Builder -> Render
builder = R . const

sp :: Render
sp = chr ' '

chr :: Char -> Render
chr = builder . B8.fromChar

str :: String -> Render
str = builder . B8.fromString

int :: Int -> Render
int = builder . BT.integral

double :: Double -> Render
double = builder . BT.double

escapedStr :: String -> Render
escapedStr = builder . BH.fromHtmlEscapedString






svgHeader :: Double -> Double -> Render
svgHeader w h =
    str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
        \<svg xmlns=\"http://www.w3.org/2000/svg\" \
             \xmlns:xlink=\"http://www.w3.org/1999/xlink\" \
             \width=\""
 <> double w
 <> str "pt\" height=\""
 <> double h
 <> str "pt\" viewBox=\"0 0 "
 <> int (round w)
 <> sp
 <> int (round h)
 <> str "\" version=\"1.1\"><g\
        \ fill=\"rgb(0,0,0)\"\
        \ fill-opacity=\"0\"\
        \ fill-rule=\"nonzero\"\
        \ font-family=\"Sans\"\
        \ font-size=\"1\"\
        \ font-style=\"normal\"\
        \ opacity=\"1\"\
        \ stroke=\"rgb(0,0,0)\"\
        \ stroke-opacity=\"1\"\
        \ stroke-width=\"0.1\"\
        \ stroke-linecap=\"butt\"\
        \ stroke-linejoin=\"miter\"\
        \ text-anchor=\"middle\"\
        \>"


svgFooter :: Render
svgFooter = str "</g></svg>"


renderPath :: Path R2 -> Render
renderPath p = R $ \t -> unR (renderPath' $ transform (inv t) p) t

renderPath' :: Path R2 -> Render
renderPath' (Path trs) =    str "<path d=\""
                         <> mconcat (map renderTrail trs)
                         <> str "\"/>"
    where
      renderTrail (P (x,y), Trail segs c) =
             chr 'M' <> double x <> sp <> double y
          <> closed (mconcat $ map renderSeg segs)
        where closed = if c then (<> chr 'Z') else id

      renderSeg (Linear (x,0)) = chr 'h' <> double x
      renderSeg (Linear (0,y)) = chr 'v' <> double y
      renderSeg (Linear (x,y)) = chr 'l' <> double x <> sp <> double y
      renderSeg (Cubic (x0,y0) (x1,y1) (x2,y2)) =
          chr 'c'
       <> double x0 <> sp
       <> double y0 <> sp
       <> double x1 <> sp
       <> double y1 <> sp
       <> double x2 <> sp
       <> double y2


renderEllipse :: Ellipse -> Render
renderEllipse (Ellipse t) =
    str "<circle r=\"1\"" <> matrix t <> str "/>"
{-
renderEllipse ellipse =
    let P (cx,cy) = ellipseCenter ellipse
        P (rx,ry) = ellipseScale  ellipse
        Deg angle = convertAngle (ellipseAngle ellipse)
    in    str "<ellipse cx=\"" <> double cx
       <> str "\" cy=\"" <> double cy
       <> str "\" rx=\"" <> double rx
       <> str "\" ry=\"" <> double ry
       <> str "\" transform=\"rotate(" <> double angle
       <> str ")\"/>"
-}


renderText :: Text -> Render
renderText (Text t val) =
    str "<text" <> matrix (t <> reflectionY) <> str ">"
 <> escapedStr val
 <> str "</text>"


data Attribute =
    AFillRule   FillRuleA
  | AFont       Font
  | AFontSize   FontSize
  | AFontSlant  FontSlantA
  | AFontWeight FontWeightA
  | ALineColor  LineColor
  | AFillColor  FillColor
  | AOpacity    Opacity
  | ALineWidth  LineWidth
  | ALineCap    LineCapA
  | ALineJoin   LineJoinA
  | ADashing    DashingA

renderAttrs :: T2 -> [Attribute] -> Render -> Render
renderAttrs t xs r = str "<g" <> matrix t <> mconcat (map f xs) <> chr '>' <> r' <> str "</g>"
    where
      r' = R $ const $ unR r t
      close = chr '"'
      f (AFillRule fr) =
          case getFillRule fr of
            Winding -> mempty -- default -- str " fill-rule=\"nonzero\""
            EvenOdd -> str " fill-rule=\"evenodd\""
      f (AFont o) =
          str " font-family=\"" <> escapedStr (getFont o) <> close
      f (AFontSize s) =
          case getFontSize s of
            1 -> mempty -- default --
            d -> str " font-size=\""<> double d <> close
      f (AFontSlant s) =
          case getFontSlant s of
            FontSlantNormal  -> mempty -- default -- str " font-style=\"normal\""
            FontSlantItalic  -> str " font-style=\"italic\""
            FontSlantOblique -> str " font-style=\"oblique\""
      f (AFontWeight s) =
          case getFontWeight s of
            FontWeightNormal -> mempty -- default -- str " font-weight=\"normal\""
            FontWeightBold   -> str " font-weight=\"bold\""
      f (ALineColor c) = color "stroke" 1 c
      f (AFillColor c) = color "fill"   0 c
      f (AOpacity o) =
          case getOpacity o of
            1 -> mempty -- default --
            d -> str " opacity=\"" <> double d <> close
      f (ALineWidth w) =
          case getLineWidth w of
            0.1 -> mempty -- default --
            d   -> str " stroke-width=\"" <> double d <> close
      f (ALineCap c) =
          case getLineCap c of
            LineCapButt   -> mempty -- default -- str " stroke-linecap=\"butt\""
            LineCapRound  -> str " stroke-linecap=\"round\""
            LineCapSquare -> str " stroke-linecap=\"square\""
      f (ALineJoin j) =
          case getLineJoin j of
            LineJoinMiter -> mempty -- default -- str " stroke-linejoin=\"miter\""
            LineJoinRound -> str " stroke-linejoin=\"round\""
            LineJoinBevel -> str " stroke-linejoin=\"bevel\""
      f (ADashing d) =
          let Dashing lens offset = getDashing d
              lens' = intersperse (chr ',') (map double lens)
          in    str " stroke-dasharray=\"" <> mconcat lens'
             <> str "\" stroke-dashoffset=\"" <> double offset <> close

color :: Color c => String -> Double -> c -> Render
color name defAlpha = \c ->
    let (r',g',b',a) = colorToRGBA c
        t d = round (d * 255)
    in (
        case (t r', t g', t b') of
          (0,0,0) -> mempty -- default --
          (r,g,b) ->   str (' ' : name ++ "=\"rgb(")
                    <> int r <> chr ','
                    <> int g <> chr ','
                    <> int b <> str ")\""
       ) <> (
        if a == defAlpha
        then mempty -- default --
        else str (' ' : name ++ "-opacity=\"") <> double a <> chr '\"'
       )




matrix :: T2 -> Render
matrix t2 = R $ \t1 -> let t = inv t1 <> t2 in unR (m t) t2
    where
      m t = case (apply t (1,0), apply t (0,1), transl t) of
              ((1,0), (0,1), (0,0)) -> mempty
              ((1,0), (0,1), (e,0)) ->    str " transform=\"translate("
                                       <> double e
                                       <> close
              ((1,0), (0,1), (e,f)) ->    str " transform=\"translate("
                                       <> double e <> sp
                                       <> double f
                                       <> close
              ((a,0), (0,d), (0,0)) ->    str " transform=\"scale("
                                       <> double a <> sp
                                       <> double d
                                       <> close
              ((a,b), (c,d), (e,f)) ->    str " transform=\"matrix("
                                       <> double a <> sp
                                       <> double b <> sp
                                       <> double c <> sp
                                       <> double d <> sp
                                       <> double e <> sp
                                       <> double f
                                       <> close
      close = str ")\""
