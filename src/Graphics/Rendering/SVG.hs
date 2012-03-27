{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ViewPatterns, OverloadedStrings, RankNTypes #-}
module Graphics.Rendering.SVG
    ( svgHeader
    , renderPath
    , renderText
    , renderStyles
    ) where

-- from base
import Data.List (intersperse)

-- from diagrams-lib
import Diagrams.Prelude hiding (Render, Attribute, close, e, (<>))
import Diagrams.TwoD.Text

import Text.Blaze.Svg11 ((!), mkPath, m, cr, hr, vr, lr, z)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

svgHeader :: Double -> Double -> S.Svg -> S.Svg
svgHeader w h_ s =  S.docTypeSvg
  ! A.version "1.1"
  ! A.width   (S.toValue w)
  ! A.height  (S.toValue h_)
  ! A.viewbox (S.toValue $ concat . intersperse " " $ map show ([0, 0, round w, round h_] :: [Int])) $
    topLevelGroup $ s

topLevelGroup :: S.Svg -> S.Svg
topLevelGroup = S.g
  ! A.fill "rgb(0,0,0)"
  ! A.fillOpacity "0"
  ! A.fillRule "nonzero"
  ! A.fontFamily "Sans"
  ! A.fontSize "1"
  ! A.fontStyle "normal"
  ! A.opacity "1"
  ! A.stroke "rgb(0,0,0)"
  ! A.strokeOpacity "1"
  ! A.strokeWidth "0.5"
  ! A.strokeLinecap "butt"
  ! A.strokeLinejoin "miter"
  ! A.textAnchor "middle"

renderPath :: Path R2 -> S.Svg
renderPath (Path trs)  = S.path ! A.d makePath
 where
  makePath = mkPath $ mapM_ renderTrail trs

renderTrail :: (P2, Trail R2) -> S.Path
renderTrail (unp2 -> (x,y), Trail segs closed) = do
  m x y
  mapM_ renderSeg segs
  if closed then z else return ()

renderSeg :: Segment R2 -> S.Path
renderSeg (Linear (unr2 -> (x,0))) = hr x
renderSeg (Linear (unr2 -> (0,y))) = vr y
renderSeg (Linear (unr2 -> (x,y))) = lr x y
renderSeg (Cubic  (unr2 -> (x0,y0)) (unr2 -> (x1,y1)) (unr2 -> (x2,y2))) = cr x0 y0 x1 y1 x2 y2

-- FIXME implement
renderText :: Text -> S.Svg
renderText _ = mempty

renderStyles :: forall v. Style v -> S.Attribute
renderStyles s = mconcat . map ($ s) $
  [ renderLineColor
  , renderFillColor
  , renderLineWidth
  ]

renderLineColor :: Style v -> S.Attribute
renderLineColor s =
  (renderAttr A.stroke lineColorRgb) `mappend`
  (renderAttr A.strokeOpacity lineColorOpacity)
 where lineColor_       = getLineColor `fmap` getAttr s
       lineColorRgb     = colorToRgbString `fmap` lineColor_
       lineColorOpacity = colorToOpacity `fmap` lineColor_

renderFillColor :: Style v -> S.Attribute
renderFillColor s =
  (renderAttr A.fill fillColorRgb) `mappend`
  (renderAttr A.fillOpacity fillColorOpacity)
 where fillColor_       = getFillColor `fmap` getAttr s
       fillColorRgb     = colorToRgbString `fmap` fillColor_
       fillColorOpacity = colorToOpacity `fmap` fillColor_


renderLineWidth :: Style v -> S.Attribute
renderLineWidth s = renderAttr A.strokeWidth lineWidth_
 where lineWidth_ = getLineWidth `fmap` getAttr s

-- Render a style attribute if available, empty otherwise
renderAttr :: S.ToValue s => (S.AttributeValue -> S.Attribute)
           -> Maybe s
           -> S.Attribute
renderAttr attr valM = case valM of
  Just val -> attr (S.toValue val)
  Nothing  -> mempty

colorToRgbString :: forall c . Color c => c -> String
colorToRgbString c = concat
  [ "rgb("
  , int r, ","
  , int g, ","
  , int b
  , ")"
  ]
 where int d = show (round (d * 255) :: Int)
       (r,g,b,_) = colorToRGBA c

colorToOpacity :: forall c . Color c => c -> Double
colorToOpacity c = a
 where (_,_,_,a) = colorToRGBA c