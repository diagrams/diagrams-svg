{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
    ( SVGFloat
    , SvgM
    , AttributeValue
    , svgHeader
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
    , getNumAttr
    ) where

-- from base
import           Data.List                   (intercalate)
#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable               (foldMap)
#endif

import           Data.Maybe                  (fromMaybe)
import           Data.Monoid

-- from diagrams-core
import           Diagrams.Core.Transform     (matrixHomRep)

-- from diagrams-lib
import           Diagrams.Prelude            hiding (Attribute, Render, with, (<>))
import           Diagrams.TwoD.Path          (getFillRule)
import           Diagrams.TwoD.Text

-- from text
import           Data.Text                   (pack)
import qualified Data.Text                   as T

-- from lucid-svg
import           Lucid.Svg                   hiding (renderText, Path)

-- from base64-bytestring, bytestring
import qualified Data.ByteString.Base64.Lazy as BS64
import qualified Data.ByteString.Lazy.Char8  as BS8

-- from JuicyPixels
import           Codec.Picture

-- from same package
import Diagrams.Backend.SVG.Attributes       (SvgId(..),SvgClass(..))

-- | Constaint on number type that diagrams-svg can use to render an SVG. This
--   includes the common number types: Double, Float
type SVGFloat n = (Show n, TypeableFloat n)
-- Could we change Text.Blaze.SVG to use
--   showFFloat :: RealFloat a => Maybe Int -> a -> ShowS
-- or something similar for all numbers so we need TypeableFloat constraint.

-- | The Svg monad, synonym for @HtmlT m ()@ Lucid.Base.
type SvgM = Element

type AttributeValue = T.Text

getNumAttr :: AttributeClass (a n) => (a n -> t) -> Style v n -> Maybe t
getNumAttr f = (f <$>) . getAttr

-- | @svgHeader w h defs s@: @w@ width, @h@ height,
--   @defs@ global definitions for defs sections, @s@ actual SVG content.
svgHeader :: SVGFloat n => n -> n -> Maybe SvgM -> [Attribute] -> Bool -> SvgM -> SvgM
svgHeader w h defines attributes genDoctype s =  dt <> with (svg11_ (defs_ [] ds <> s))
  ([ Width <<- toText w
   , Height <<- toText h
   , Font_size <<- "1"
   , ViewBox <<- (pack . unwords $ map show ([0, 0, round w, round h] :: [Int]))
   , Stroke <<- "rgb(0,0,0)"
   , Stroke_opacity <<- "1" ]
   ++ attributes )
  where
    ds = fromMaybe mempty defines
    dt = if genDoctype then doctype else mempty

renderPath :: SVGFloat n => Path V2 n -> SvgM
renderPath trs = if makePath == T.empty then mempty else path_ [D <<- makePath]
  where
    makePath = foldMap renderTrail (op Path trs)

renderTrail :: SVGFloat n => Located (Trail V2 n) -> AttributeValue
renderTrail (viewLoc -> (P (V2 x y), t)) = mA x y <> withTrail renderLine renderLoop t
  where
    renderLine = foldMap renderSeg . lineSegments
    renderLoop lp =
      case loopSegments lp of
        -- let z handle the last segment if it is linear
        (segs, Linear _) -> foldMap renderSeg segs

        -- otherwise we have to emit it explicitly
        _ -> foldMap renderSeg (lineSegments . cutLoop $ lp)
      <> z

renderSeg :: SVGFloat n => Segment Closed V2 n -> AttributeValue
renderSeg (Linear (OffsetClosed (V2 x 0))) = hR x
renderSeg (Linear (OffsetClosed (V2 0 y))) = vR y
renderSeg (Linear (OffsetClosed (V2 x y))) = lR x y
renderSeg (Cubic  (V2 x0 y0)
                  (V2 x1 y1)
                  (OffsetClosed (V2 x2 y2))) = cR x0 y0 x1 y1 x2 y2

renderClip :: SVGFloat n => Path V2 n -> T.Text -> Int -> SvgM -> SvgM
renderClip p prefix ident svg = do
     defs_ [] $ clipPath_ [Id <<- (clipPathId ident)] (renderPath p)
  <> g_  [Clip_path <<- ("url(#" <> clipPathId ident <> ")")] svg
    where
      clipPathId i = prefix <> "myClip" <> (pack . show $ i)

renderStop :: SVGFloat n => GradientStop n -> SvgM
renderStop (GradientStop c v)
  = stop_ [ Stop_color <<- (colorToRgbText c)
          , Offset <<- (toText v)
          , Stop_opacity <<- (toText $ colorToOpacity c) ]

spreadMethodText :: SpreadMethod -> AttributeValue
spreadMethodText GradPad      = "pad"
spreadMethodText GradReflect  = "reflect"
spreadMethodText GradRepeat   = "repeat"

renderLinearGradient :: SVGFloat n => LGradient n -> Int -> SvgM
renderLinearGradient g i = linearGradient_
    [ Id <<- (pack $ "gradient" ++ show i)
    , X1 <<- toText x1
    , Y1 <<- toText y1
    , X2 <<- toText x2
    , Y2 <<- toText y2
    , GradientTransform <<- mx
    , GradientUnits <<- "userSpaceOnUse"
    , SpreadMethod <<- spreadMethodText (g ^. lGradSpreadMethod) ]
    $ foldMap renderStop (g^.lGradStops)
  where
    mx = matrix a1 a2 b1 b2 c1 c2
    [[a1, a2], [b1, b2], [c1, c2]] = matrixHomRep (g ^. lGradTrans)
    P (V2 x1 y1) = g ^. lGradStart
    P (V2 x2 y2) = g ^. lGradEnd

renderRadialGradient :: SVGFloat n => RGradient n -> Int -> SvgM
renderRadialGradient g i = radialGradient_
    [ Id <<- (pack $ "gradient" ++ show i)
    , R  <<- toText (g ^. rGradRadius1)
    , Cx <<- toText cx
    , Cy <<- toText cy
    , Fx <<- toText fx
    , Fy <<- toText fy
    , GradientTransform <<- mx
    , GradientUnits <<- "userSpaceOnUse"
    , SpreadMethod <<- spreadMethodText (g ^. rGradSpreadMethod) ]
    ( foldMap renderStop ss )
  where
    mx = matrix a1 a2 b1 b2 c1 c2
    [[a1, a2], [b1, b2], [c1, c2]] = matrixHomRep (g ^.rGradTrans)
    P (V2 cx cy) = g ^. rGradCenter1
    P (V2 fx fy) = g ^. rGradCenter0 -- SVGs focal point is our inner center.

    -- Adjust the stops so that the gradient begins at the perimeter of
    -- the inner circle (center0, radius0) and ends at the outer circle.
    r0 = g ^. rGradRadius0
    r1 = g ^. rGradRadius1
    stopFracs = r0 / r1 : map (\s -> (r0 + (s ^. stopFraction) * (r1 - r0)) / r1)
                (g ^. rGradStops)
    gradStops = case g ^. rGradStops of
      []       -> []
      xs@(x:_) -> x : xs
    ss = zipWith (\gs sf -> gs & stopFraction .~ sf ) gradStops stopFracs

-- Create a gradient element so that it can be used as an attribute value for fill.
renderFillTextureDefs :: SVGFloat n => Int -> Style v n -> SvgM
renderFillTextureDefs i s =
  case getNumAttr getFillTexture s of
    Just (LG g) -> renderLinearGradient g i
    Just (RG g) -> renderRadialGradient g i
    _           -> mempty

-- Render the gradient using the id set up in renderFillTextureDefs.
renderFillTexture :: SVGFloat n => Int -> Style v n -> [Attribute]
renderFillTexture ident s = case getNumAttr getFillTexture s of
  Just (SC (SomeColor c)) -> renderTextAttr Fill fillColorRgb <>
                             renderAttr Fill_opacity fillColorOpacity
    where
      fillColorRgb     = Just $ colorToRgbText c
      fillColorOpacity = Just $ colorToOpacity c
  Just (LG _) -> [Fill <<- ("url(#gradient" <> (pack . show $ ident)
                                            <> ")"), Fill_opacity <<- "1"]
  Just (RG _) -> [Fill <<- ("url(#gradient" <> (pack . show $ ident)
                                            <> ")"), Fill_opacity <<- "1"]
  Nothing     -> []

renderLineTextureDefs :: SVGFloat n => Int -> Style v n -> SvgM
renderLineTextureDefs i s =
  case getNumAttr getLineTexture s of
    Just (LG g) -> renderLinearGradient g i
    Just (RG g) -> renderRadialGradient g i
    _           -> mempty

renderLineTexture :: SVGFloat n => Int -> Style v n -> [Attribute]
renderLineTexture ident s = case getNumAttr getLineTexture s of
  Just (SC (SomeColor c)) -> renderTextAttr Stroke lineColorRgb <>
                             renderAttr Stroke_opacity lineColorOpacity
    where
      lineColorRgb     = Just $ colorToRgbText c
      lineColorOpacity = Just $ colorToOpacity c
  Just (LG _) -> [Stroke <<- ("url(#gradient" <> (pack . show $ ident)
                                              <> ")"), Stroke_opacity <<- "1"]
  Just (RG _) -> [Stroke <<- ("url(#gradient" <> (pack . show $ ident)
                                              <> ")"), Stroke_opacity <<- "1"]
  Nothing     -> []

dataUri :: String -> BS8.ByteString -> AttributeValue
dataUri mime dat = pack $ "data:"++mime++";base64," ++ BS8.unpack (BS64.encode dat)

renderDImageEmb :: SVGFloat n => DImage n Embedded -> SvgM
renderDImageEmb di@(DImage (ImageRaster dImg) _ _ _) =
  renderDImage di $ dataUri "image/png" img
  where
    img = case encodeDynamicPng dImg of
            Left str   -> error str
            Right img' -> img'

renderDImage :: SVGFloat n => DImage n any -> AttributeValue -> SvgM
renderDImage (DImage _ w h tr) uridata =
  image_
    [ Transform <<- transformMatrix
    , Width <<-  (pack . show $ w)
    , Height <<- (pack . show $ h)
    , XlinkHref <<- uridata ]
  where
    [[a,b],[c,d],[e,f]] = matrixHomRep (tr `mappend` reflectionY
                                           `mappend` tX `mappend` tY)
    transformMatrix = matrix a b c d e f
    tX = translationX $ fromIntegral (-w)/2
    tY = translationY $ fromIntegral (-h)/2

renderText :: SVGFloat n => Text n -> SvgM
renderText (Text tt tAlign str) =
  text_
    [ Transform <<- transformMatrix
    , Dominant_baseline <<- vAlign
    , Text_anchor <<- hAlign
    , Stroke <<- "none" ]
    $ toElement str
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
  t                   = tt `mappend` reflectionY
  [[a,b],[c,d],[e,f]] = matrixHomRep t
  transformMatrix     = matrix a b c d e f

renderStyles :: SVGFloat n => Int -> Int -> Style v n -> [Attribute]
renderStyles fillId lineId s = concatMap ($ s) $
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
  , renderSvgId
  , renderSvgClass
  , renderMiterLimit ]


renderSvgId :: SVGFloat n => Style v n -> [Attribute]
renderSvgId s = renderTextAttr Id svgIdAttr
 where svgIdAttr = pack . getSvgId <$> getAttr s

renderSvgClass :: SVGFloat n => Style v n -> [Attribute]
renderSvgClass s = renderTextAttr Class svgClassAttr
 where svgClassAttr = pack . getSvgClass <$> getAttr s

renderMiterLimit :: SVGFloat n => Style v n -> [Attribute]
renderMiterLimit s = renderAttr Stroke_miterlimit miterLimit
 where miterLimit = getLineMiterLimit <$> getAttr s

renderOpacity :: SVGFloat n => Style v n -> [Attribute]
renderOpacity s = renderAttr Opacity o
 where o = getOpacity <$> getAttr s

renderFillRule :: SVGFloat n => Style v n -> [Attribute]
renderFillRule s = renderTextAttr Fill_rule fr
  where fr = (fillRuleToText . getFillRule) <$> getAttr s
        fillRuleToText :: FillRule -> AttributeValue
        fillRuleToText Winding = "nonzero"
        fillRuleToText EvenOdd = "evenodd"

renderLineWidth :: SVGFloat n => Style v n -> [Attribute]
renderLineWidth s = renderAttr Stroke_width lWidth
  where lWidth = getNumAttr getLineWidth s

renderLineCap :: SVGFloat n => Style v n -> [Attribute]
renderLineCap s = renderTextAttr Stroke_linecap lCap
  where lCap = (lineCapToText . getLineCap) <$> getAttr s
        lineCapToText :: LineCap -> AttributeValue
        lineCapToText LineCapButt   = "butt"
        lineCapToText LineCapRound  = "round"
        lineCapToText LineCapSquare = "square"

renderLineJoin :: SVGFloat n => Style v n -> [Attribute]
renderLineJoin s = renderTextAttr Stroke_linejoin lj
  where lj = (lineJoinToText . getLineJoin) <$> getAttr s
        lineJoinToText :: LineJoin -> AttributeValue
        lineJoinToText LineJoinMiter = "miter"
        lineJoinToText LineJoinRound = "round"
        lineJoinToText LineJoinBevel = "bevel"

renderDashing :: SVGFloat n => Style v n -> [Attribute]
renderDashing s = renderTextAttr Stroke_dasharray arr <>
                  renderAttr Stroke_dashoffset dOffset
 where
  getDasharray  (Dashing a _) = a
  getDashoffset (Dashing _ o) = o
  dashArrayToStr              = intercalate "," . map show
  -- Ignore dashing if dashing array is empty
  checkEmpty (Just (Dashing [] _)) = Nothing
  checkEmpty other                 = other
  dashing'                    = checkEmpty $ getNumAttr getDashing s
  arr                         = (pack . dashArrayToStr . getDasharray) <$> dashing'
  dOffset                     = getDashoffset <$> dashing'

renderFontSize :: SVGFloat n => Style v n -> [Attribute]
renderFontSize s = renderTextAttr Font_size fs
 where
  fs = pack <$> getNumAttr ((++ "px") . show . getFontSize) s

renderFontSlant :: SVGFloat n => Style v n -> [Attribute]
renderFontSlant s = renderTextAttr Font_style fs
 where
  fs = (fontSlantAttr . getFontSlant) <$> getAttr s
  fontSlantAttr :: FontSlant -> AttributeValue
  fontSlantAttr FontSlantItalic  = "italic"
  fontSlantAttr FontSlantOblique = "oblique"
  fontSlantAttr FontSlantNormal  = "normal"

renderFontWeight :: SVGFloat n => Style v n -> [Attribute]
renderFontWeight s = renderTextAttr Font_weight fw
 where
  fw = (fontWeightAttr . getFontWeight) <$> getAttr s
  fontWeightAttr :: FontWeight -> AttributeValue
  fontWeightAttr FontWeightNormal = "normal"
  fontWeightAttr FontWeightBold   = "bold"

renderFontFamily :: SVGFloat n => Style v n -> [Attribute]
renderFontFamily s = renderTextAttr Font_family ff
 where
  ff = (pack . getFont) <$> getAttr s

-- | Render a style attribute if available, empty otherwise.
renderAttr :: Show s => AttrTag -> Maybe s -> [Attribute]
renderAttr attr valM = maybe [] (\v -> [(bindAttr attr) (pack . show $ v)]) valM

-- renderTextAttr :: (AttributeValue -> Attribute) -> Maybe AttributeValue -> [Attribute]
renderTextAttr :: AttrTag -> Maybe AttributeValue -> [Attribute]
renderTextAttr attr valM = maybe [] (\v -> [(bindAttr attr) v]) valM

colorToRgbText :: forall c . Color c => c -> AttributeValue
colorToRgbText c = T.concat
  [ "rgb("
  , int r, ","
  , int g, ","
  , int b
  , ")" ]
 where
   int d     = pack . show $ (round (d * 255) :: Int)
   (r,g,b,_) = colorToSRGBA c

colorToOpacity :: forall c . Color c => c -> Double
colorToOpacity c = a
 where (_,_,_,a) = colorToSRGBA c
