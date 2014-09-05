{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
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
    , renderDImage
    , renderDImageEmb
    , renderStyles
    , renderMiterLimit
    , renderFillTextureDefs
    , renderFillTexture
    , renderLineTextureDefs
    , renderLineTexture
    , dataUri
    ) where

-- from base
import           Data.List                   (intercalate)
import qualified Data.Foldable as F
import           Data.Foldable (foldMap)

-- from lens
import           Control.Lens                hiding (transform)

-- from diagrams-core
import           Diagrams.Core.Transform     (matrixHomRep)

-- from diagrams-lib
import           Diagrams.Prelude            hiding (Attribute, Render, (<>))
import           Diagrams.TwoD.Path          (getFillRule)
import           Diagrams.TwoD.Text

-- from blaze-svg
import           Text.Blaze.Svg11            (cr, hr, lr, m, mkPath, vr, z, (!))
import qualified Text.Blaze.Svg11            as S
import qualified Text.Blaze.Svg11.Attributes as A
import qualified Data.ByteString.Base64.Lazy as BS64
import qualified Data.ByteString.Lazy.Char8  as BS8

import           Codec.Picture


getNumAttr :: AttributeClass (a n) => (a n -> t) -> Style v n -> Maybe t
getNumAttr f = (f <$>) . getAttr

getMeasuredAttr :: AttributeClass (a n) => (a n -> Measure t) -> Style v n -> Maybe t
getMeasuredAttr f = (fromOutput . f <$>) . getAttr



-- | @svgHeader w h defs s@: @w@ width, @h@ height,
--   @defs@ global definitions for defs sections, @s@ actual SVG content.
svgHeader :: Double -> Double -> Maybe S.Svg -> S.Svg -> S.Svg
svgHeader w h_ defines s =  S.docTypeSvg
  ! A.version "1.1"
  ! A.width    (S.toValue w)
  ! A.height   (S.toValue h_)
  ! A.fontSize "1"
  ! A.viewbox (S.toValue . unwords $ map show ([0, 0, round w, round h_] :: [Int]))
  ! A.stroke "rgb(0,0,0)"
  ! A.strokeOpacity "1"
  $ F.mapM_ S.defs defines >> S.g s
  -- $ do case defines of
  --        Nothing -> return ()
  --        Just defs -> S.defs $ defs
  --      S.g $ s

renderPath :: Path V2 Double -> S.Svg
renderPath trs  = S.path ! A.d makePath
  where
    makePath = mkPath $ mapM_ renderTrail (op Path trs)

renderTrail :: Located (Trail V2 Double) -> S.Path
renderTrail (viewLoc -> (P (V2 x y), t)) = m x y >> withTrail renderLine renderLoop t
  where
    renderLine = mapM_ renderSeg . lineSegments
    renderLoop lp = do
      case loopSegments lp of
        -- let 'z' handle the last segment if it is linear
        (segs, Linear _) -> mapM_ renderSeg segs

        -- otherwise we have to emit it explicitly
        _ -> mapM_ renderSeg (lineSegments . cutLoop $ lp)
      z

renderSeg :: Segment Closed V2 Double -> S.Path
renderSeg (Linear (OffsetClosed (V2 x 0))) = hr x
renderSeg (Linear (OffsetClosed (V2 0 y))) = vr y
renderSeg (Linear (OffsetClosed (V2 x y))) = lr x y
renderSeg (Cubic  (V2 x0 y0)
                  (V2 x1 y1)
                  (OffsetClosed (V2 x2 y2))) = cr x0 y0 x1 y1 x2 y2

renderClip :: Path V2 Double -> Int -> S.Svg -> S.Svg
renderClip p id_ svg =
  S.g ! A.clipPath (S.toValue $ "url(#" ++ clipPathId id_ ++ ")") $ do
    S.clippath ! A.id_ (S.toValue $ clipPathId id_) $ renderPath p
    svg
  where clipPathId i = "myClip" ++ show i

renderStop :: GradientStop Double -> S.Svg
renderStop (GradientStop c v)
  = S.stop ! A.stopColor (S.toValue (colorToRgbString c))
           ! A.offset (S.toValue (show v))
           ! A.stopOpacity (S.toValue (colorToOpacity c))

spreadMethodStr :: SpreadMethod -> String
spreadMethodStr GradPad      = "pad"
spreadMethodStr GradReflect  = "reflect"
spreadMethodStr GradRepeat   = "repeat"

renderLinearGradient :: LGradient Double -> Int -> S.Svg
renderLinearGradient g i = S.lineargradient
    ! A.id_ (S.toValue ("gradient" ++ show i))
    ! A.x1  (S.toValue x1)
    ! A.y1  (S.toValue y1)
    ! A.x2  (S.toValue x2)
    ! A.y2  (S.toValue y2)
    ! A.gradienttransform (S.toValue matrix)
    ! A.gradientunits "userSpaceOnUse"
    ! A.spreadmethod (S.toValue (spreadMethodStr (g^.lGradSpreadMethod)))
    $ foldMap renderStop (g^.lGradStops)
  where
    matrix = S.matrix a1 a2 b1 b2 c1 c2
    [[a1, a2], [b1, b2], [c1, c2]] = matrixHomRep (g^.lGradTrans)
    -- (x1, y1) = unp2 (g^.lGradStart)
    -- (x2, y2) = unp2 (g^.lGradEnd)
    P (V2 x1 y1) = g ^. lGradStart
    P (V2 x2 y2) = g ^. lGradEnd

renderRadialGradient :: RGradient Double -> Int -> S.Svg
renderRadialGradient g i = S.radialgradient
    ! A.id_ (S.toValue ("gradient" ++ show i))
    ! A.r (S.toValue (g^.rGradRadius1))
    ! A.cx (S.toValue cx')
    ! A.cy (S.toValue cy')
    ! A.fx (S.toValue fx')
    ! A.fy (S.toValue fy')
    ! A.gradienttransform (S.toValue matrix)
    ! A.gradientunits "userSpaceOnUse"
    ! A.spreadmethod (S.toValue (spreadMethodStr (g^.rGradSpreadMethod)))
    $ foldMap renderStop ss
  where
    matrix = S.matrix a1 a2 b1 b2 c1 c2
    [[a1, a2], [b1, b2], [c1, c2]] = matrixHomRep (g^.rGradTrans)
    P (V2 cx' cy') = g ^. rGradCenter1
    P (V2 fx' fy') = g ^. rGradCenter0 -- SVG's focal point is our inner center.
    -- (cx', cy') = unp2 (g^.rGradCenter1)
    -- (fx', fy') = unp2 (g^.rGradCenter0) -- SVG's focal point is our inner center.

    -- Adjust the stops so that the gradient begins at the perimeter of
    -- the inner circle (center0, radius0) and ends at the outer circle.
    r0 = g^.rGradRadius0
    r1 = g^.rGradRadius1
    stopFracs = r0 / r1 : map (\s -> (r0 + (s^.stopFraction) * (r1 - r0)) / r1)
                (g^.rGradStops)
    gradStops = case g^.rGradStops of
      []       -> []
      xs@(x:_) -> x : xs
    ss = zipWith (\gs sf -> gs & stopFraction .~ sf ) gradStops stopFracs

-- Create a gradient element so that it can be used as an attribute value for fill.
renderFillTextureDefs :: Int -> Style v Double -> S.Svg
renderFillTextureDefs i s =
  case getFillTexture <$> getAttr s of
    Just (LG g) -> renderLinearGradient g i
    Just (RG g) -> renderRadialGradient g i
    _           -> mempty

-- Render the gradient using the id set up in renderFillTextureDefs.
renderFillTexture :: Int -> Style v Double -> S.Attribute
renderFillTexture id_ s = case getNumAttr getFillTexture s of
  Just (SC (SomeColor c)) -> renderAttr A.fill fillColorRgb `mappend`
                             renderAttr A.fillOpacity fillColorOpacity
    where
      fillColorRgb     = Just $ colorToRgbString c
      fillColorOpacity = Just $ colorToOpacity c
  Just (LG _) -> A.fill (S.toValue ("url(#gradient" ++ show id_ ++ ")"))
                `mappend` A.fillOpacity "1"
  Just (RG _) -> A.fill (S.toValue ("url(#gradient" ++ show id_ ++ ")"))
                `mappend` A.fillOpacity "1"
  Nothing     -> mempty

renderLineTextureDefs :: Int -> Style v Double -> S.Svg
renderLineTextureDefs i s =
  case getLineTexture <$> getAttr s of
    Just (LG g) -> renderLinearGradient g i
    Just (RG g) -> renderRadialGradient g i
    _           -> mempty

renderLineTexture :: Int -> Style v Double -> S.Attribute
renderLineTexture id_ s = case getNumAttr getLineTexture s of
  Just (SC (SomeColor c)) -> renderAttr A.stroke lineColorRgb `mappend`
                             renderAttr A.strokeOpacity lineColorOpacity
    where
      lineColorRgb     = Just $ colorToRgbString c
      lineColorOpacity = Just $ colorToOpacity c
  Just (LG _) -> A.stroke (S.toValue ("url(#gradient" ++ show id_ ++ ")"))
                `mappend` A.strokeOpacity "1"
  Just (RG _) -> A.stroke (S.toValue ("url(#gradient" ++ show id_ ++ ")"))
                `mappend` A.strokeOpacity "1"
  Nothing     -> mempty

dataUri :: String -> BS8.ByteString -> String
dataUri mime dat = "data:"++mime++";base64," ++ BS8.unpack (BS64.encode dat)

renderDImageEmb :: DImage Double Embedded -> S.Svg
renderDImageEmb di@(DImage (ImageRaster dImg) _ _ _) =
  renderDImage di $ dataUri "image/png" img
  where
    img = case encodeDynamicPng dImg of
            Left str   -> error str
            Right img' -> img'

renderDImage :: DImage Double any -> String -> S.Svg
renderDImage (DImage _ w h tr) uridata =
  S.image
    ! A.transform transformMatrix
    ! A.width (S.toValue w)
    ! A.height (S.toValue h)
    ! A.xlinkHref (S.preEscapedToValue uridata)
  where
    [[a,b],[c,d],[e,f]] = matrixHomRep (tr `mappend` reflectionY 
                                           `mappend` tX `mappend` tY)
    transformMatrix = S.matrix a b c d e f
    tX = translationX $ fromIntegral (-w)/2
    tY = translationY $ fromIntegral (-h)/2

renderText :: Bool -> Text Double -> S.Svg
renderText isLocal (Text tt tn tAlign str) =
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
  t                   = (if isLocal then tt else tn) `mappend` reflectionY
  [[a,b],[c,d],[e,f]] = matrixHomRep t
  transformMatrix     = S.matrix a b c d e f

renderStyles :: Int -> Int -> Style v Double -> S.Attribute
renderStyles fillId lineId s = mconcat . map ($ s) $
  [ renderLineTexture lineId
  , renderFillTexture fillId
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

renderMiterLimit :: Style v Double -> S.Attribute
renderMiterLimit s = renderAttr A.strokeMiterlimit miterLimit
 where miterLimit = getLineMiterLimit <$> getAttr s

renderOpacity :: Style v Double -> S.Attribute
renderOpacity s = renderAttr A.opacity opacity_
 where opacity_ = getOpacity <$> getAttr s

renderFillRule :: Style v Double -> S.Attribute
renderFillRule s = renderAttr A.fillRule fillRule_
  where fillRule_ = (fillRuleToStr . getFillRule) <$> getAttr s
        fillRuleToStr :: FillRule -> String
        fillRuleToStr Winding = "nonzero"
        fillRuleToStr EvenOdd = "evenodd"

renderLineWidth :: Style v Double -> S.Attribute
renderLineWidth s = renderAttr A.strokeWidth lineWidth'
  where lineWidth' = getMeasuredAttr getLineWidth s


renderLineCap :: Style v Double -> S.Attribute
renderLineCap s = renderAttr A.strokeLinecap lineCap_
  where lineCap_ = (lineCapToStr . getLineCap) <$> getAttr s
        lineCapToStr :: LineCap -> String
        lineCapToStr LineCapButt   = "butt"
        lineCapToStr LineCapRound  = "round"
        lineCapToStr LineCapSquare = "square"

renderLineJoin :: Style v Double -> S.Attribute
renderLineJoin s = renderAttr A.strokeLinejoin lineJoin_
  where lineJoin_ = (lineJoinToStr . getLineJoin) <$> getAttr s
        lineJoinToStr :: LineJoin -> String
        lineJoinToStr LineJoinMiter = "miter"
        lineJoinToStr LineJoinRound = "round"
        lineJoinToStr LineJoinBevel = "bevel"

renderDashing :: Style v Double -> S.Attribute
renderDashing s = renderAttr A.strokeDasharray arr `mappend`
                  renderAttr A.strokeDashoffset dOffset
 where
  getDasharray  (Dashing a _) = map fromOutput a
  getDashoffset (Dashing _ o) = fromOutput o
  dashArrayToStr              = intercalate "," . map show
  dashing_                    = getNumAttr getDashing s
  arr                         = (dashArrayToStr . getDasharray) <$> dashing_
  dOffset                     = getDashoffset <$> dashing_

renderFontSize :: Style v Double -> S.Attribute
renderFontSize s = renderAttr A.fontSize fontSize_
 where
  fontSize_ = getNumAttr ((++ "em") . str . getFontSize) s
  str o = show $ fromOutput o

renderFontSlant :: Style v Double -> S.Attribute
renderFontSlant s = renderAttr A.fontStyle fontSlant_
 where
  fontSlant_ = (fontSlantAttr . getFontSlant) <$> getAttr s
  fontSlantAttr :: FontSlant -> String
  fontSlantAttr FontSlantItalic  = "italic"
  fontSlantAttr FontSlantOblique = "oblique"
  fontSlantAttr FontSlantNormal  = "normal"

renderFontWeight :: Style v Double -> S.Attribute
renderFontWeight s = renderAttr A.fontWeight fontWeight_
 where
  fontWeight_ = (fontWeightAttr . getFontWeight) <$> getAttr s
  fontWeightAttr :: FontWeight -> String
  fontWeightAttr FontWeightNormal = "normal"
  fontWeightAttr FontWeightBold   = "bold"

renderFontFamily :: Style v Double -> S.Attribute
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

