{-# LANGUAGE CPP               #-}
{-# LANGUAGE TypeApplications               #-}
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
    ( Element
    , AttributeValue
    , svgHeader
    , renderPath
    , renderLines
    , renderLoops
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
    -- , getNumAttr
    ) where

-- from base
import           Data.List                   (intercalate)
#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable               (foldMap)
#endif

import           Data.Maybe                  (fromMaybe)
-- import           Data.Monoid

-- from diagrams-core
-- import           Diagrams.Core.Transform     (matrixHomRep)

-- from diagrams-lib
import Geometry
-- import           Diagrams.Prelude            hiding (Attribute, Render, with, (<>))
-- import           Diagrams.TwoD.Path          (getFillRule)
-- import           Diagrams.TwoD.Text

-- from text
import           Data.Text                   (pack)
import qualified Data.Text                   as T

-- from lucid-svg
import           Graphics.Svg                hiding (renderText, (<>))
import Diagrams.TwoD.Attributes
import Diagrams.TwoD.Image
import Diagrams.TwoD.Text
import Diagrams.Prelude hiding (with)

-- from base64-bytestring, bytestring
import qualified Data.ByteString.Base64.Lazy as BS64
import qualified Data.ByteString.Lazy.Char8  as BS8

-- from JuicyPixels
import           Codec.Picture

-- | Constaint on number type that diagrams-svg can use to render an SVG. This
--   includes the common number types: Double, Float
-- type SVGFloat n = (Show n, TypeableFloat n)
-- Could we change Text.Blaze.SVG to use
--   showFFloat :: RealFloat a => Maybe Int -> a -> ShowS
-- or something similar for all numbers so we need TypeableFloat constraint.

type AttributeValue = T.Text

-- getNumAttr :: AttributeClass (a n) => (a n -> t) -> Attributes -> Maybe t
-- getNumAttr f = (f <$>) . getAttr

-- | @svgHeader w h defs s@: @w@ width, @h@ height,
--   @defs@ global definitions for defs sections, @s@ actual SVG content.
svgHeader :: Double -> Double -> Maybe Element -> [Attribute] -> Bool
                        -> Element -> Element
svgHeader w h defines attributes genDoctype s =
  dt <> with (svg11_ (defs_ [] ds <> s))
    ([ Width_ <<- toText w
     , Height_ <<- toText h
     , Font_size_ <<- "1"
     , ViewBox_ <<- (pack . unwords $ map show ([0, 0, round w, round h] :: [Int]))
     , Stroke_ <<- "rgb(0,0,0)"
     , Stroke_opacity_ <<- "1" ]
     ++ attributes )
  where
    ds = fromMaybe mempty defines
    dt = if genDoctype then doctype else mempty

renderLines :: [Located (Line V2 Double)] -> Element
renderLines ts = path_ [D_ <<- foldMap renderLine ts]
  where
    renderLine (Loc (P (V2 x y)) t) = mA x y <> foldMapOf segments renderSeg t

renderLoops :: [Located (Loop V2 Double)] -> Element
renderLoops ts = path_ [D_ <<- foldMap renderLoop ts]
  where
    renderLoop :: Located (Loop V2 Double) -> AttributeValue
    renderLoop (Loc (P (V2 x y)) loop@(Loop line closing)) =
      mA x y <> case closing of
        -- let z handle the last segment if it is linear
        LinearClosing -> foldMapOf segments renderSeg line <> z

        -- otherwise we have to emit it explicitly (segments returns the
        -- closing segment as a normal segment for loops)
        _ -> foldMapOf segments renderSeg loop

renderPath :: Path V2 Double -> Element
renderPath trs = if makePath == T.empty then mempty else path_ [D_ <<- makePath]
  where
    makePath :: AttributeValue
    makePath = foldMapOf each renderTrail trs -- (op Path trs)

renderTrail :: Located (Trail V2 Double) -> AttributeValue
renderTrail (viewLoc -> (P (V2 x y), t)) =
  mA x y <> withTrail renderLine renderLoop t
  where
    renderLine = foldMapOf segments renderSeg --  . lineSegments
    renderLoop loop@(Loop line closing) =
      case closing of
        -- let z handle the last segment if it is linear
        LinearClosing -> foldMapOf segments renderSeg line <> z

        -- otherwise we have to emit it explicitly (segments returns the
        -- closing segment as a normal segment for loops)
        _ -> foldMapOf segments renderSeg loop

renderSeg :: Segment V2 Double -> AttributeValue
renderSeg (Linear (V2 x 0)) = hR x
renderSeg (Linear (V2 0 y)) = vR y
renderSeg (Linear (V2 x y)) = lR x y
renderSeg (Cubic  (V2 x0 y0)
                  (V2 x1 y1)
                  (V2 x2 y2)) = cR x0 y0 x1 y1 x2 y2

renderClip :: Path V2 Double -> T.Text -> Int -> Element -> Element
renderClip p prefix ident svg = do
     defs_ [] $ clipPath_ [Id_ <<- (clipPathId ident)] (renderPath p)
  <> g_  [Clip_path_ <<- ("url(#" <> clipPathId ident <> ")")] svg
    where
      clipPathId i = prefix <> "myClip" <> (pack . show $ i)

renderStop :: GradientStop -> Element
renderStop (GradientStop c v)
  = stop_ [ Stop_color_ <<- (colorToRgbText c)
          , Offset_ <<- (toText v)
          , Stop_opacity_ <<- (toText $ colorToOpacity c) ]

spreadMethodText :: SpreadMethod -> AttributeValue
spreadMethodText GradPad     = "pad"
spreadMethodText GradReflect = "reflect"
spreadMethodText GradRepeat  = "repeat"

renderLinearGradient :: LGradient -> Int -> Element
renderLinearGradient g i = linearGradient_
    [ Id_ <<- (pack $ "gradient" ++ show i)
    , X1_ <<- toText x1
    , Y1_ <<- toText y1
    , X2_ <<- toText x2
    , Y2_ <<- toText y2
    , GradientTransform_ <<- mx
    , GradientUnits_ <<- "userSpaceOnUse"
    , SpreadMethod_ <<- spreadMethodText (g ^. gradientSpreadMethod) ]
    $ foldMap renderStop (g^.gradientStops)
  where
    mx = matrix a1 a2 b1 b2 c1 c2
    [[a1, a2], [b1, b2], [c1, c2]] = matrixHomRep (g ^. gradientTransform)
    P (V2 x1 y1) = g ^. gradientStart
    P (V2 x2 y2) = g ^. gradientEnd

renderRadialGradient :: RGradient -> Int -> Element
renderRadialGradient g i = radialGradient_
    [ Id_ <<- (pack $ "gradient" ++ show i)
    , R_  <<- toText (g ^. gradientRadius1)
    , Cx_ <<- toText cx
    , Cy_ <<- toText cy
    , Fx_ <<- toText fx
    , Fy_ <<- toText fy
    , GradientTransform_ <<- mx
    , GradientUnits_ <<- "userSpaceOnUse"
    , SpreadMethod_ <<- spreadMethodText (g ^. gradientSpreadMethod) ]
    ( foldMap renderStop ss )
  where
    mx = matrix a1 a2 b1 b2 c1 c2
    [[a1, a2], [b1, b2], [c1, c2]] = matrixHomRep (g ^. gradientTransform)
    P (V2 cx cy) = g ^. gradientCenter1
    P (V2 fx fy) = g ^. gradientCenter0 -- SVGs focal point is our inner center.

    -- Adjust the stops so that the gradient begins at the perimeter of
    -- the inner circle (center0, radius0) and ends at the outer circle.
    r0 = g ^. gradientRadius0
    r1 = g ^. gradientRadius1
    stopFracs = r0 / r1 : map (\s -> (r0 + (s ^. stopFraction) * (r1 - r0)) / r1)
                (g ^. gradientStops)
    gradStops = case g ^. gradientStops of
      []       -> []
      xs@(x:_) -> x : xs
    ss = zipWith (\gs sf -> gs & stopFraction .~ sf ) gradStops stopFracs

-- Create a gradient element so that it can be used as an attribute value for fill.
renderFillTextureDefs :: Int -> Attributes -> Element
renderFillTextureDefs i s =
  case getAttr _FillTexture s of
    Just (LG g) -> defs_ [] $ renderLinearGradient g i
    Just (RG g) -> defs_ [] $ renderRadialGradient g i
    _           -> mempty

-- Render the gradient using the id set up in renderFillTextureDefs.
renderFillTexture :: Int -> Attributes -> [Attribute]
-- renderFillTexture ident s = case getAttr getFillTexture s of
renderFillTexture ident s = case getAttr _FillTexture s of
  Just (SC (SomeColor c)) -> renderTextAttr Fill_ fillColorRgb <>
                             renderAttr Fill_opacity_ fillColorOpacity
    where
      fillColorRgb     = Just $ colorToRgbText c
      fillColorOpacity = Just $ colorToOpacity c
  Just (LG _) -> [Fill_ <<- ("url(#gradient" <> (pack . show $ ident)
                                             <> ")"), Fill_opacity_ <<- "1"]
  Just (RG _) -> [Fill_ <<- ("url(#gradient" <> (pack . show $ ident)
                                             <> ")"), Fill_opacity_ <<- "1"]
  Nothing     -> []

renderLineTextureDefs :: Int -> Attributes -> Element
renderLineTextureDefs i s =
  case getAttr _LineTexture s of
    Just (LG g) -> defs_ [] $ renderLinearGradient g i
    Just (RG g) -> defs_ [] $ renderRadialGradient g i
    _           -> mempty

renderLineTexture :: Int -> Attributes -> [Attribute]
renderLineTexture ident s = case getAttr _LineTexture s of
  Just (SC (SomeColor c)) -> renderTextAttr Stroke_ lineColorRgb <>
                             renderAttr Stroke_opacity_ lineColorOpacity
    where
      lineColorRgb     = Just $ colorToRgbText c
      lineColorOpacity = Just $ colorToOpacity c
  Just (LG _) -> [Stroke_ <<- ("url(#gradient" <> (pack . show $ ident)
                                               <> ")"), Stroke_opacity_ <<- "1"]
  Just (RG _) -> [Stroke_ <<- ("url(#gradient" <> (pack . show $ ident)
                                               <> ")"), Stroke_opacity_ <<- "1"]
  Nothing     -> []

dataUri :: String -> BS8.ByteString -> AttributeValue
dataUri mime dat = pack $ "data:"++mime++";base64," ++ BS8.unpack (BS64.encode dat)

renderDImageEmb :: T2 Double -> DImage Double Embedded -> Element
renderDImageEmb t2 di@(DImage _ _ (ImageRaster dImg)) =
  renderDImage t2 di $ dataUri "image/png" img
  where
    img = case encodeDynamicPng dImg of
            Left str   -> error str
            Right img' -> img'

renderDImage :: T2 Double -> DImage Double any -> AttributeValue -> Element
renderDImage tr (DImage w h _) uridata =
  image_
    [ Transform_ <<- transformMatrix
    , Width_ <<-  (pack . show $ w)
    , Height_ <<- (pack . show $ h)
    , XlinkHref_ <<- uridata ]
  where
    [[a,b],[c,d],[e,f]] = matrixHomRep (tr `mappend` reflectionY
                                           `mappend` tX `mappend` tY)
    transformMatrix = matrix a b c d e f
    tX = translationX $ fromIntegral (-w)/2
    tY = translationY $ fromIntegral (-h)/2

renderText :: T2 Double -> Text Double -> Element
renderText tt (Text tAlign str) =
  text_
    [ Transform_ <<- transformMatrix
    , Dominant_baseline_ <<- vAlign
    , Text_anchor_ <<- hAlign
    , Stroke_ <<- "none" ]
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

renderStyles :: Int -> Int -> Attributes -> [Attribute]
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
  , renderFontFamily ]
  -- , renderMiterLimit ]

renderMiterLimit :: Attributes -> [Attribute]
renderMiterLimit s = renderAttr Stroke_miterlimit_ miterLimit
  where miterLimit = getAttr _LineMiterLimit s

renderOpacity :: Attributes -> [Attribute]
renderOpacity s = renderAttr Opacity_ o
  where o = getAttr _Opacity s

renderFillRule :: Attributes -> [Attribute]
renderFillRule s = renderTextAttr Fill_rule_ fr
  where fr = fillRuleToText <$> getAttr _FillRule s
        fillRuleToText :: FillRule -> AttributeValue
        fillRuleToText Winding = "nonzero"
        fillRuleToText EvenOdd = "evenodd"

renderLineWidth :: Attributes -> [Attribute]
renderLineWidth s = renderAttr Stroke_width_ lWidth
  where
    lWidth :: Maybe Double
    lWidth = getAttr _LineWidth s

renderLineCap :: Attributes -> [Attribute]
renderLineCap s = renderTextAttr Stroke_linecap_ lCap
  where lCap = lineCapToText <$> getAttr _LineCap s
        lineCapToText :: LineCap -> AttributeValue
        lineCapToText LineCapButt   = "butt"
        lineCapToText LineCapRound  = "round"
        lineCapToText LineCapSquare = "square"

renderLineJoin :: Attributes -> [Attribute]
renderLineJoin s = renderTextAttr Stroke_linejoin_ lj
  where lj = lineJoinToText <$> getAttr _LineJoin s
        lineJoinToText :: LineJoin -> AttributeValue
        lineJoinToText LineJoinMiter = "miter"
        lineJoinToText LineJoinRound = "round"
        lineJoinToText LineJoinBevel = "bevel"

renderDashing :: Attributes -> [Attribute]
renderDashing s = renderTextAttr Stroke_dasharray_ arr <>
                  renderAttr Stroke_dashoffset_ dOffset
 where
  getDasharray  (Dashing a _) = a
  getDashoffset (Dashing _ o) = o
  dashArrayToStr = intercalate "," . map show
  -- Ignore dashing if dashing array is empty
  checkEmpty (Just (Dashing [] _)) = Nothing
  checkEmpty other = other
  dashing' :: Maybe (Dashing Double)
  dashing' = checkEmpty $ getAttr _Dashing s
  arr = (pack . dashArrayToStr . getDasharray) <$> dashing'
  dOffset = getDashoffset <$> dashing'

renderFontSize :: Attributes -> [Attribute]
renderFontSize s = renderTextAttr Font_size_ fs
 where
  fs = pack . (++ "px") . show <$> getAttr (_FontSize @ Double) s

renderFontSlant :: Attributes -> [Attribute]
renderFontSlant s = renderTextAttr Font_style_ fs
 where
  fs = fontSlantAttr <$> getAttr _FontSlant s
  fontSlantAttr :: FontSlant -> AttributeValue
  fontSlantAttr FontSlantItalic  = "italic"
  fontSlantAttr FontSlantOblique = "oblique"
  fontSlantAttr FontSlantNormal  = "normal"

renderFontWeight :: Attributes -> [Attribute]
renderFontWeight s = renderTextAttr Font_weight_ fw
 where
  fw = fontWeightAttr <$> getAttr _FontWeight s
  fontWeightAttr :: FontWeight -> AttributeValue
  fontWeightAttr FontWeightNormal = "normal"
  fontWeightAttr FontWeightBold   = "bold"
  fontWeightAttr FontWeightLighter = "lighter"
  fontWeightAttr FontWeightBolder  = "bolder"
  fontWeightAttr FontWeightThin = "100"
  fontWeightAttr FontWeightUltraLight = "200"
  fontWeightAttr FontWeightLight = "300"
  fontWeightAttr FontWeightMedium = "400"
  fontWeightAttr FontWeightSemiBold = "600"
  fontWeightAttr FontWeightUltraBold = "800"
  fontWeightAttr FontWeightHeavy = "900"


renderFontFamily :: Attributes -> [Attribute]
renderFontFamily s = renderTextAttr Font_family_ ff
 where
   ff = pack <$> getAttr _Font s

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
