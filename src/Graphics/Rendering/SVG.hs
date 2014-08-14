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
    , renderDImage
    , renderStyles
    , renderMiterLimit
    , renderFillTextureDefs
    , renderFillTexture
    , renderLineTextureDefs
    , renderLineTexture
    ) where

-- from base
import           Data.List                   (intercalate, intersperse)

-- from lens
import           Control.Lens                hiding (transform)

-- from diagrams-core
import           Diagrams.Core.Transform     (matrixHomRep)

-- from diagrams-lib
import           Diagrams.Prelude            hiding (Attribute, Render, (<>))
import           Diagrams.TwoD.Path          (getFillRule)
import           Diagrams.TwoD.Text
import           Diagrams.TwoD.Types.Double

-- from blaze-svg
import           Text.Blaze.Svg11            (cr, hr, lr, m, mkPath, vr, z, (!))
import qualified Text.Blaze.Svg11            as S
import qualified Text.Blaze.Svg11.Attributes as A
import qualified Data.ByteString.Base64.Lazy as BS64
import qualified Data.ByteString.Lazy.Char8  as BS8

import           Codec.Picture

-- | @svgHeader w h defs s@: @w@ width, @h@ height,
--   @defs@ global definitions for defs sections, @s@ actual SVG content.
svgHeader :: Double -> Double -> Maybe S.Svg -> S.Svg -> S.Svg
svgHeader w h_ defines s =  S.docTypeSvg
  ! A.version "1.1"
  ! A.width    (S.toValue w)
  ! A.height   (S.toValue h_)
  ! A.fontSize "1"
  ! A.viewbox (S.toValue $ concat . intersperse " " $ map show ([0, 0, round w, round h_] :: [Int]))
  ! A.stroke "rgb(0,0,0)"
  ! A.strokeOpacity "1"
  $ do case defines of
         Nothing -> return ()
         Just defs -> S.defs $ defs
       S.g $ s

renderPath :: Path R2 -> S.Svg
renderPath trs  = S.path ! A.d makePath
  where
    makePath = mkPath $ mapM_ renderTrail (op Path trs)

renderTrail :: Located (Trail R2) -> S.Path
renderTrail (viewLoc -> (unp2 -> (x,y), t)) = m x y >> withTrail renderLine renderLoop t
  where
    renderLine = mapM_ renderSeg . lineSegments
    renderLoop lp = do
      case loopSegments lp of
        -- let 'z' handle the last segment if it is linear
        (segs, Linear _) -> mapM_ renderSeg segs

        -- otherwise we have to emit it explicitly
        _ -> mapM_ renderSeg (lineSegments . cutLoop $ lp)
      z

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
renderStop (GradientStop c v)
  = S.stop ! A.stopColor (S.toValue (colorToRgbString c))
           ! A.offset (S.toValue (show v))
           ! A.stopOpacity (S.toValue (colorToOpacity c))

spreadMethodStr :: SpreadMethod -> String
spreadMethodStr GradPad      = "pad"
spreadMethodStr GradReflect  = "reflect"
spreadMethodStr GradRepeat   = "repeat"

renderLinearGradient :: LGradient R2 -> Int -> S.Svg
renderLinearGradient g i = S.lineargradient
    ! A.id_ (S.toValue ("gradient" ++ (show i)))
    ! A.x1  (S.toValue x1)
    ! A.y1  (S.toValue y1)
    ! A.x2  (S.toValue x2)
    ! A.y2  (S.toValue y2)
    ! A.gradienttransform (S.toValue matrix)
    ! A.gradientunits "userSpaceOnUse"
    ! A.spreadmethod (S.toValue (spreadMethodStr (g^.lGradSpreadMethod)))
    $ do mconcat $ (map renderStop) (g^.lGradStops)
  where
    matrix = S.matrix a1 a2 b1 b2 c1 c2
    [[a1, a2], [b1, b2], [c1, c2]] = matrixHomRep (g^.lGradTrans)
    (x1, y1) = unp2 (g^.lGradStart)
    (x2, y2) = unp2 (g^.lGradEnd)

renderRadialGradient :: RGradient R2 -> Int -> S.Svg
renderRadialGradient g i = S.radialgradient
    ! A.id_ (S.toValue ("gradient" ++ (show i)))
    ! A.r (S.toValue (g^.rGradRadius1))
    ! A.cx (S.toValue cx')
    ! A.cy (S.toValue cy')
    ! A.fx (S.toValue fx')
    ! A.fy (S.toValue fy')
    ! A.gradienttransform (S.toValue matrix)
    ! A.gradientunits "userSpaceOnUse"
    ! A.spreadmethod (S.toValue (spreadMethodStr (g^.rGradSpreadMethod)))
    $ do mconcat $ map renderStop ss
  where
    matrix = S.matrix a1 a2 b1 b2 c1 c2
    [[a1, a2], [b1, b2], [c1, c2]] = matrixHomRep (g^.rGradTrans)
    (cx', cy') = unp2 (g^.rGradCenter1)
    (fx', fy') = unp2 (g^.rGradCenter0) -- SVG's focal point is our inner center.

    -- Adjust the stops so that the gradient begins at the perimeter of
    -- the inner circle (center0, radius0) and ends at the outer circle.
    r0 = g^.rGradRadius0
    r1 = g^.rGradRadius1
    stopFracs = r0 / r1 : map (\s -> (r0 + (s^.stopFraction) * (r1-r0)) / r1)
                (g^.rGradStops)
    gradStops = case g^.rGradStops of
      []       -> []
      xs@(x:_) -> x : xs
    ss = zipWith (\gs sf -> gs & stopFraction .~ sf ) gradStops stopFracs

-- Create a gradient element so that it can be used as an attribute value for fill.
renderFillTextureDefs :: Int -> Style v -> S.Svg
renderFillTextureDefs i s =
  case (getFillTexture <$> getAttr s) of
    Just (LG g) -> renderLinearGradient g i
    Just (RG g) -> renderRadialGradient g i
    _           -> mempty

-- Render the gradient using the id set up in renderFillTextureDefs.
renderFillTexture :: Int -> Style v -> S.Attribute
renderFillTexture id_ s = case (getFillTexture <$> getAttr s) :: Maybe (Texture R2) of
  Just (SC (SomeColor c)) -> (renderAttr A.fill fillColorRgb) `mappend`
                             (renderAttr A.fillOpacity fillColorOpacity)
    where
      fillColorRgb     = Just $ colorToRgbString c
      fillColorOpacity = Just $ colorToOpacity c
  Just (LG _) -> A.fill (S.toValue ("url(#gradient" ++ show id_ ++ ")"))
                `mappend` A.fillOpacity "1"
  Just (RG _) -> A.fill (S.toValue ("url(#gradient" ++ show id_ ++ ")"))
                `mappend` A.fillOpacity "1"
  Nothing     -> mempty

renderLineTextureDefs :: Int -> Style v -> S.Svg
renderLineTextureDefs i s =
  case (getLineTexture <$> getAttr s) of
    Just (LG g) -> renderLinearGradient g i
    Just (RG g) -> renderRadialGradient g i
    _           -> mempty

renderLineTexture :: Int -> Style v -> S.Attribute
renderLineTexture id_ s = case (getLineTexture <$> getAttr s) :: Maybe (Texture R2) of
  Just (SC (SomeColor c)) -> (renderAttr A.stroke lineColorRgb) `mappend`
                             (renderAttr A.strokeOpacity lineColorOpacity)
    where
      lineColorRgb     = Just $ colorToRgbString c
      lineColorOpacity = Just $ colorToOpacity c
  Just (LG _) -> A.stroke (S.toValue ("url(#gradient" ++ show id_ ++ ")"))
                `mappend` A.strokeOpacity "1"
  Just (RG _) -> A.stroke (S.toValue ("url(#gradient" ++ show id_ ++ ")"))
                `mappend` A.strokeOpacity "1"
  Nothing     -> mempty

renderDImage :: DImage R2 Embedded -> S.Svg
renderDImage (DImage iD w h tr) =
  S.image
    ! A.transform transformMatrix
    ! A.width (S.toValue w)
    ! A.height (S.toValue h)
    ! A.xlinkHref (S.preEscapedToValue (mkDataURI img))
  where
    [[a,b],[c,d],[e,f]] = matrixHomRep (tr `mappend` reflectionY 
                                           `mappend` tX `mappend` tY)
    transformMatrix = S.matrix a b c d e f
    mkDataURI dat = "data:image/png;base64," ++ BS8.unpack (BS64.encode dat)
    img = case encodeDynamicPng dImg of
            Left str   -> error str
            Right img' -> img'
    ImageRaster dImg = iD
    tX = translationX $ fromIntegral (-w)/2
    tY = translationY $ fromIntegral (-h)/2

renderText :: Bool -> Text R2 -> S.Svg
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

renderStyles :: Int -> Int -> Style v -> S.Attribute
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

renderMiterLimit :: Style v -> S.Attribute
renderMiterLimit s = renderAttr A.strokeMiterlimit miterLimit
 where miterLimit = getLineMiterLimit <$> getAttr s

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
renderLineWidth s = renderAttr A.strokeWidth lineWidth'
  where lineWidth' = (fromOutput . getLineWidth) <$> (getAttr s :: Maybe (LineWidth R2))


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
  getDasharray  (Dashing a  _) = map fromOutput a
  getDashoffset (Dashing _ o) = fromOutput o
  dashArrayToStr              = intercalate "," . map show
  dashing_                    = getDashing <$> (getAttr s :: Maybe (DashingA R2))
  arr                         = (dashArrayToStr . getDasharray) <$> dashing_
  dOffset                     = getDashoffset <$> dashing_

renderFontSize :: Style v -> S.Attribute
renderFontSize s = renderAttr A.fontSize fontSize_
 where
  fontSize_ = ((++ "em") . str . getFontSize) <$> (getAttr s :: Maybe (FontSize R2))
  str o = show $ fromOutput o

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
