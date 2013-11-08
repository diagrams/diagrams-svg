{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ViewPatterns               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.SVG
-- Copyright   :  (c) 2011 diagrams-svg team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Generic tools for generating SVG files.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.SVG
    ( svgHeader
    , renderPath
    , renderClip
    , renderText
    , renderStyles
    , renderTransform
    , renderMiterLimit
    , getMatrix
    , renderFillTextureDefs
    , renderFillTexture
    ) where

-- from base
import           Data.List                   (intercalate, intersperse)

-- from lens
import           Control.Lens                hiding (transform)

-- from diagrams-lib
import           Diagrams.Prelude            hiding (Attribute, Render, e, (<>))
import           Diagrams.TwoD.Path          (getFillRule)
import           Diagrams.TwoD.Text

-- from blaze-svg
import           Text.Blaze.Svg11            (cr, hr, lr, m, mkPath, vr, z, (!))
import qualified Text.Blaze.Svg11            as S
import qualified Text.Blaze.Svg11.Attributes as A

-- | @svgHeader w h defs s@: @w@ width, @h@ height,
--   @defs@ global definitions for defs sections, @s@ actual SVG content.
svgHeader :: Double -> Double -> Maybe S.Svg -> S.Svg -> S.Svg
svgHeader w h_ defines s =  S.docTypeSvg
  ! A.version "1.1"
  ! A.width    (S.toValue w)
  ! A.height   (S.toValue h_)
  ! A.fontSize "1"
  ! A.viewbox (S.toValue $ concat . intersperse " " $ map show ([0, 0, round w, round h_] :: [Int]))
  $ do case defines of
         Nothing -> return ()
         Just defs -> S.defs $ defs
       S.g $ s

renderPath :: Path R2 -> S.Svg
renderPath trs  = S.path ! A.d makePath
 where
  makePath = mkPath $ mapM_ renderTrail (op Path trs)

renderTrail :: Located (Trail R2) -> S.Path
renderTrail (viewLoc -> (unp2 -> (x,y), t)) = flip withLine t $ \l -> do
  m x y
  mapM_ renderSeg (lineSegments l)
  if isLoop t then z else return ()

renderSeg :: Segment Closed R2 -> S.Path
renderSeg (Linear (OffsetClosed (unr2 -> (x,0)))) = hr x
renderSeg (Linear (OffsetClosed (unr2 -> (0,y)))) = vr y
renderSeg (Linear (OffsetClosed (unr2 -> (x,y)))) = lr x y
renderSeg (Cubic  (unr2 -> (x0,y0))
                  (unr2 -> (x1,y1))
                  (OffsetClosed (unr2 -> (x2,y2))))
  = cr x0 y0 x1 y1 x2 y2

renderClip :: Path R2 -> Int -> S.Svg -> S.Svg
renderClip p id_ svg = do
  S.g ! A.clipPath (S.toValue $ "url(#" ++ clipPathId id_ ++ ")") $ do
    S.clippath ! A.id_ (S.toValue $ clipPathId id_) $ renderPath p
    svg
  where clipPathId i = "myClip" ++ show i

renderStop :: GradientStop -> S.Svg
renderStop (c, v, o)
  = S.stop ! A.stopColor (S.toValue (colorToRgbString c))
           ! A.offset (S.toValue (show v))
           ! A.stopOpacity (S.toValue (show o))

spreadMethodStr :: SpreadMethod -> String
spreadMethodStr GradPad      = "pad"
spreadMethodStr GradReflect  = "reflect"
spreadMethodStr GradRepeat   = "repeat"

renderLinearGradient :: LGradient -> Int -> S.Svg
renderLinearGradient g i = S.lineargradient
    ! A.id_ (S.toValue ("gradient" ++ (show i)))
    ! A.x1  (S.toValue (x1' - 0.5))
    ! A.y1  (S.toValue (y1' - 0.5))
    ! A.x2  (S.toValue (x1' - 0.5 + dx))
    ! A.y2  (S.toValue (y1' - 0.5 + dy))
    ! A.gradienttransform (S.toValue matrix)
    ! A.gradientunits "userSpaceOnUse"
    ! A.spreadmethod (S.toValue (spreadMethodStr (g^.lGradSpreadMethod)))
    $ do mconcat $ (map renderStop) (g^.lGradStops)
  where
    matrix = S.matrix a1 a2 b1 b2 c1 c2
    (a1, a2, b1, b2, c1, c2) = getMatrix (g^.lGradTrans)
    (x1', y1') = unp2 (g^.lGradStart)
    (x2', y2') = unp2 (g^.lGradEnd)
    dx = (x2' - x1')
    dy = (y2' - y1')

renderRadialGradient :: RGradient -> Int -> S.Svg
renderRadialGradient g i = S.radialgradient
    ! A.id_ (S.toValue ("gradient" ++ (show i)))
    ! A.gradienttransform (S.toValue matrix)
    ! A.gradientunits "userSpaceOnUse"
    $ do mconcat $ (map renderStop) (g^.rGradStops)
  where
    matrix = S.matrix a1 a2 b1 b2 c1 c2
    (a1, a2, b1, b2, c1, c2) = getMatrix (g^.rGradTrans)


-- Create a defs element to contain the gradient so that it can be used as
-- an attribute vale for fill.
-- XXX rg implementation is not finished
renderFillTextureDefs :: Int -> Style v -> S.Svg
renderFillTextureDefs i s =
  case (getFillTexture <$> getAttr s) of
    Just (LG g) -> renderLinearGradient g i
    Just (RG g) -> renderRadialGradient g i
    _           -> mempty

-- Render the gradient using the id set up in renderFillTextureDefs.
renderFillTexture :: Int -> Style v -> S.Attribute
renderFillTexture id_ s = case (getFillTexture <$> getAttr s) of
  Just (SC (SomeColor c)) -> (renderAttr A.fill fillColorRgb) `mappend`
                             (renderAttr A.fillOpacity fillColorOpacity)
    where
      fillColorRgb     = Just $ colorToRgbString c
      fillColorOpacity = Just $ colorToOpacity c
  Just (LG _) -> A.fill (S.toValue ("url(#gradient" ++ show id_ ++ ")"))
                `mappend` A.fillOpacity "1"
  Just (RG _) -> mempty
  Nothing     -> renderFillColor s -- check for old style fillColor attribute.

renderText :: Text -> S.Svg
renderText (Text tr tAlign str) =
  S.text_
    ! A.transform transformMatrix
    ! A.dominantBaseline vAlign
    ! A.textAnchor hAlign
    ! A.stroke "none" $
      S.toMarkup str
 where
  vAlign = case tAlign of
             BaselineText -> "alphabetic"
             BoxAlignedText _ h -> case h of -- A mere approximation
               h' | h' <= 0.25 -> "text-after-edge"
               h' | h' >= 0.75 -> "text-before-edge"
               _ -> "middle"
  hAlign = case tAlign of
             BaselineText -> "start"
             BoxAlignedText w _ -> case w of -- A mere approximation
               w' | w' <= 0.25 -> "start"
               w' | w' >= 0.75 -> "end"
               _ -> "middle"
  t                   = tr `mappend` reflectionY
  (a,b,c,d,e,f)       = getMatrix t
  transformMatrix     =  S.matrix a b c d e f

getMatrix :: Transformation R2 -> (Double, Double, Double, Double, Double, Double)
getMatrix t = (a1,a2,b1,b2,c1,c2)
 where
  (unr2 -> (a1,a2)) = apply t unitX
  (unr2 -> (b1,b2)) = apply t unitY
  (unr2 -> (c1,c2)) = transl t

-- | Apply a transformation to some already-rendered SVG.
renderTransform :: Transformation R2 -> S.Svg -> S.Svg
renderTransform t svg =
  if i then svg
  else S.g svg ! (A.transform $ S.matrix a1 a2 b1 b2 c1 c2)
    where (a1,a2,b1,b2,c1,c2) = getMatrix t
          i = (a1,a2,b1,b2,c1,c2) == (1,0,0,1,0,0)

renderStyles :: Bool -> Int -> Style v -> S.Attribute
renderStyles ignoreFill id' s = mconcat . map ($ s) $
  [ renderLineColor
  , if ignoreFill
      then const (renderAttr A.fillOpacity (Just (0 :: Double)))
      else renderFillTexture id'
  , renderLineWidth
  , renderLineCap
  , renderLineJoin
  , renderFillRule
  , renderDashing
  , renderOpacity
  , renderFontSize
  , renderFontSlant
  , renderFontWeight
  , renderFontFamily
  , renderMiterLimit
  ]

renderMiterLimit :: Style v -> S.Attribute
renderMiterLimit s = renderAttr A.strokeMiterlimit miterLimit
 where miterLimit = getLineMiterLimit <$> getAttr s

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
                  (renderAttr A.strokeDashoffset dOffset)
 where
  getDasharray  (Dashing a _) = a
  getDashoffset :: Dashing -> Double
  getDashoffset (Dashing _ o) = o
  dashArrayToStr              = intercalate "," . map show
  dashing_                    = getDashing <$> getAttr s
  arr                         = (dashArrayToStr . getDasharray) <$> dashing_
  dOffset                     = getDashoffset <$> dashing_

renderFontSize :: Style v -> S.Attribute
renderFontSize s = renderAttr A.fontSize fontSize_
 where
  fontSize_ = ((++ "em") . show . getFontSize) <$> getAttr s

renderFontSlant :: Style v -> S.Attribute
renderFontSlant s = renderAttr A.fontStyle fontSlant_
 where
  fontSlant_ = (fontSlantAttr . getFontSlant) <$> getAttr s
  fontSlantAttr :: FontSlant -> String
  fontSlantAttr FontSlantItalic  = "italic"
  fontSlantAttr FontSlantOblique = "oblique"
  fontSlantAttr FontSlantNormal  = "normal"

renderFontWeight :: Style v -> S.Attribute
renderFontWeight s = renderAttr A.fontWeight fontWeight_
 where
  fontWeight_ = (fontWeightAttr . getFontWeight) <$> getAttr s
  fontWeightAttr :: FontWeight -> String
  fontWeightAttr FontWeightNormal = "normal"
  fontWeightAttr FontWeightBold   = "bold"

renderFontFamily :: Style v -> S.Attribute
renderFontFamily s = renderAttr A.fontFamily fontFamily_
 where
  fontFamily_ = getFont <$> getAttr s

-- | Render a style attribute if available, empty otherwise.
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
       (r,g,b,_) = colorToSRGBA c

colorToOpacity :: forall c . Color c => c -> Double
colorToOpacity c = a
 where (_,_,_,a) = colorToSRGBA c
