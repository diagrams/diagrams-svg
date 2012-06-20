{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ViewPatterns, OverloadedStrings, RankNTypes #-}
module Graphics.Rendering.SVG
    ( svgHeader
    , renderPath
    , renderClip
    , renderClipPathId
    , renderText
    , renderStyles
    ) where

-- from base
import Data.List (intersperse, intercalate)

-- from diagrams-lib
import Diagrams.Prelude hiding (Render, Attribute, close, e, (<>))
import Diagrams.TwoD.Text
import Diagrams.TwoD.Path (getFillRule, getClip)

import Text.Blaze.Svg11 ((!), mkPath, m, cr, hr, vr, lr, z)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

svgHeader :: Double -> Double -> S.Svg -> S.Svg
svgHeader w h_ s =  S.docTypeSvg
  ! A.version "1.1"
  ! A.width    (S.toValue w)
  ! A.height   (S.toValue h_)
  ! A.fontSize "1"
  ! A.viewbox (S.toValue $ concat . intersperse " " $ map show ([0, 0, round w, round h_] :: [Int])) $
     S.g $ s

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


renderClip :: Maybe [Path R2] -> Int -> S.Svg
renderClip Nothing _       = mempty
renderClip (Just pths) id_ = S.clippath ! A.id_ clipPathId $ renderClipPaths
  where renderClipPaths = mapM_ renderPath pths
        clipPathId      = S.toValue $ "myClip" ++ show id_

-- FIXME take alignment into account
renderText :: Text -> S.Svg
renderText (Text tr _ str) =
  S.text_
    ! A.transform transformMatrix
    ! A.dominantBaseline "middle"
    ! A.textAnchor "middle"
    ! A.stroke "none" $
      S.toMarkup str
 where
  t                   = tr `mappend` reflectionY
  (a,b,c,d,e,f)       = getMatrix t
  transformMatrix     =  S.matrix a b c d e f

getMatrix :: Transformation R2 -> (Double, Double, Double, Double, Double, Double)
getMatrix t = (a1,a2,b1,b2,c1,c2)
 where
  (unr2 -> (a1,a2)) = apply t unitX
  (unr2 -> (b1,b2)) = apply t unitY
  (unr2 -> (c1,c2)) = transl t

renderStyles :: forall v. Style v -> S.Attribute
renderStyles s = mconcat . map ($ s) $
  [ renderLineColor
  , renderFillColor
  , renderLineWidth
  , renderLineCap
  , renderLineJoin
  , renderFillRule
  , renderDashing
  , renderOpacity
  , renderFontSize
  ]

renderLineColor :: Style v -> S.Attribute
renderLineColor s =
  (renderAttr A.stroke lineColorRgb) `mappend`
  (renderAttr A.strokeOpacity lineColorOpacity)
 where lineColor_       = getLineColor <$> getAttr s
       lineColorRgb     = colorToRgbString <$> lineColor_
       lineColorOpacity = colorToOpacity <$> lineColor_

renderFillColor :: Style v -> S.Attribute
renderFillColor s =
  (renderAttr A.fill fillColorRgb) `mappend`
  (renderAttr A.fillOpacity fillColorOpacity)
 where fillColor_       = getFillColor <$> getAttr s
       fillColorRgb     = colorToRgbString <$> fillColor_
       fillColorOpacity = colorToOpacity <$> fillColor_


renderOpacity :: Style v -> S.Attribute
renderOpacity s = renderAttr A.opacity opacity_
 where opacity_ = getOpacity <$> getAttr s

renderFillRule :: Style v -> S.Attribute
renderFillRule s = renderAttr A.fillRule fillRule_
  where fillRule_ = (fillRuleToStr . getFillRule) <$> getAttr s
        fillRuleToStr :: FillRule -> String
        fillRuleToStr Winding = "nonzero"
        fillRuleToStr EvenOdd = "evenodd"

renderLineWidth :: Style v -> S.Attribute
renderLineWidth s = renderAttr A.strokeWidth lineWidth_
 where lineWidth_ = getLineWidth <$> getAttr s

renderLineCap :: Style v -> S.Attribute
renderLineCap s = renderAttr A.strokeLinecap lineCap_
  where lineCap_ = (lineCapToStr . getLineCap) <$> getAttr s
        lineCapToStr :: LineCap -> String
        lineCapToStr LineCapButt   = "butt"
        lineCapToStr LineCapRound  = "round"
        lineCapToStr LineCapSquare = "square"

renderLineJoin :: Style v -> S.Attribute
renderLineJoin s = renderAttr A.strokeLinejoin lineJoin_
  where lineJoin_ = (lineJoinToStr . getLineJoin) <$> getAttr s
        lineJoinToStr :: LineJoin -> String
        lineJoinToStr LineJoinMiter = "miter"
        lineJoinToStr LineJoinRound = "round"
        lineJoinToStr LineJoinBevel = "bevel"

renderDashing :: Style v -> S.Attribute
renderDashing s = (renderAttr A.strokeDasharray arr) `mappend`
                  (renderAttr A.strokeDashoffset offset)
 where
  getDasharray  (Dashing a _) = a
  getDashoffset :: Dashing -> Double
  getDashoffset (Dashing _ o) = o
  dashArrayToStr              = intercalate "," . map show
  dashing_                    = getDashing <$> getAttr s
  arr                         = (dashArrayToStr . getDasharray) <$> dashing_
  offset                      = getDashoffset <$> dashing_

renderFontSize :: Style v -> S.Attribute
renderFontSize s = renderAttr A.fontSize fontSize_
 where
  fontSize_ = ((++ "em") . show . getFontSize) <$> getAttr s


renderClipPathId :: Style v -> Int -> S.Attribute
renderClipPathId s id_ = renderAttr A.clipPath clipPathId
 where
  clipPathId :: Maybe String
  clipPathId = case getClip <$> getAttr s of
                 Nothing -> Nothing
                 Just _ -> Just ("url(#myClip" ++ show id_ ++ ")")

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

