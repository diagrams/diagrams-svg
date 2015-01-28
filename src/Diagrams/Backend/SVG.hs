{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE NondecreasingIndentation   #-}
{-# LANGUAGE UndecidableInstances       #-} -- UndecidableInstances needed for ghc < 707
{-# LANGUAGE GADTs                      #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.SVG
-- Copyright   :  (c) 2011-2012 diagrams-svg team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A full-featured rendering backend for diagrams producing SVG files,
-- implemented natively in Haskell (making it easy to use on any
-- platform).
--
-- To invoke the SVG backend, you have three options.
--
-- * You can use the "Diagrams.Backend.SVG.CmdLine" module to create
--   standalone executables which output SVG images when invoked.
--
-- * You can use the 'renderSVG' or 'renderPretty' functions provided by
--   this module, which give you more flexible programmatic control over when
--   and how images are output (making it easy to, for example, write a
--   single program that outputs multiple images, or one that outputs
--   images dynamically based on user input, and so on). The only
--   difference between the two functions is that 'renderPretty', pretty
--   prints the SVG output.
--
-- * For the most flexibility (/e.g./ if you want access to the
--   resulting SVG value directly in memory without writing it to
--   disk), you can manually invoke the 'renderDia' method from the
--   'Diagrams.Core.Types.Backend' instance for @SVG@.  In particular,
--   'Diagrams.Core.Types.renderDia' has the generic type
--
-- > renderDia :: b -> Options b v n -> QDiagram b v n m -> Result b v n
--
-- (omitting a few type class constraints).  @b@ represents the
-- backend type, @v@ the vector space, @n@ the numerical field, and @m@ the
-- type of monoidal query annotations on the diagram.  'Options' and 'Result'
-- are associated data and type families, respectively, which yield the
-- type of option records and rendering results specific to any
-- particular backend.  For @b ~ SVG@, @v ~ V2@, and @n ~ Double@, we have
--
-- > data Options SVG R2 Double = SVGOptions
-- >                       { size :: SizeSpec2D Double  -- ^ The requested size.
-- >                       , svgDefinitions :: Maybe S.Svg
-- >                       -- ^ Custom definitions that will be added to the @defs@
-- >                       --  section of the output.
-- >                       }
--
-- @
-- data family Render SVG R2 Double = R 'SvgRenderM'
-- @
--
-- @
-- type family Result SVG R2 Double = 'Text.Blaze.Svg11.Svg'
-- @
--
-- So the type of 'renderDia' resolves to
--
-- @
-- renderDia :: SVG -> Options SVG V2 Double -> QDiagram SVG V2 Double m -> 'Text.Blaze.Svg11.Svg'
-- @
--
-- which you could call like @renderDia SVG (SVGOptions (Width 250) Nothing)
-- myDiagram@.  (In some situations GHC may not be able to infer the
-- type @m@, in which case you can use a type annotation to specify
-- it; it may be useful to simply use the type synonym @Diagram SVG
-- = QDiagram SVG R2 Double Any@.) This returns an
-- 'Text.Blaze.Svg11.Svg' value, which you can, /e.g./ render to a
-- 'ByteString' using 'Text.Blaze.Svg.Renderer.Utf8.renderSvg'.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.SVG
  ( SVG(..) -- rendering token
  , B
  , Options(..), sizeSpec, svgDefinitions -- for rendering options specific to SVG
  , SVGFloat

  , renderSVG
  , renderPretty
  , loadImageSVG
  ) where

-- from JuicyPixels
import           Codec.Picture
import           Codec.Picture.Types(dynamicMap)

import           Data.Foldable                as F (foldMap)
import           Data.Text.Lazy.IO            as LT
import           Data.Tree

-- from base
import           Control.Monad.State
import           Data.Typeable

-- from hashable
import           Data.Hashable                (Hashable (..))

-- from bytestring
import qualified Data.ByteString              as SBS
import qualified Data.ByteString.Lazy         as BS

-- from lens
import           Control.Lens                 hiding (transform, ( # ))

-- from diagrams-core
import           Diagrams.Core.Compile
import           Diagrams.Core.Types          (Annotation (..))

-- from diagrams-lib
import           Diagrams.Prelude             hiding (Attribute, view, size)
import           Diagrams.TwoD.Adjust         (adjustDia2D)
import           Diagrams.TwoD.Attributes     (splitTextureFills)
import           Diagrams.TwoD.Path           (Clip (Clip))
import           Diagrams.TwoD.Text
import           Diagrams.Backend.Build

-- from lucid-svg
import           Lucid.Svg

-- from this package
import qualified Graphics.Rendering.SVG       as R
import           Graphics.Rendering.SVG       (SVGFloat, SvgM)

-- | @SVG@ is simply a token used to identify this rendering backend
--   (to aid type inference).
data SVG = SVG
  deriving (Show, Typeable)

type B = SVG

type instance V SVG = V2
type instance N SVG = Double

data SvgRenderState = SvgRenderState { _clipPathId  :: Int
                                     , _fillGradId  :: Int
                                     , _lineGradId  :: Int }

makeLenses ''SvgRenderState

-- Fill gradients ids are even, line gradient ids are odd.
initialSvgRenderState :: SvgRenderState
initialSvgRenderState = SvgRenderState 0 0 1

-- | Monad to keep track of state when rendering an SVG.
--   Currently just keeps a monotonically increasing counter
--   for assiging a unique clip path ID.
type SvgRenderM = State SvgRenderState SvgM

instance SVGFloat n => Monoid (Render SVG V2 n) where
  mempty  = R $ return mempty
  (R r1) `mappend` (R r2_) =
    R $ do
      svg1 <- r1
      svg2 <- r2_
      return (svg1 `mappend` svg2)

-- Handle clip attributes.
--
renderSvgWithClipping :: forall n. SVGFloat n
                      => SvgM          -- ^ Input SVG
                      -> Style V2 n    -- ^ Styles
                      -> SvgRenderM    -- ^ Resulting svg

renderSvgWithClipping svg s =
  case op Clip <$> getAttr s of
    Nothing -> return svg
    Just paths -> renderClips paths
  where
    renderClips :: SVGFloat n => [Path V2 n] -> SvgRenderM
    renderClips []     = return svg
    renderClips (p:ps) = do
      clipPathId += 1
      ident <- use clipPathId
      R.renderClip p ident <$> renderClips ps

-- | Create a new texture defs svg element using the style and the current
--   id number, then increment the gradient id number.
fillTextureDefs :: SVGFloat n => Style v n -> SvgRenderM
fillTextureDefs s = do
  ident <- use fillGradId
  fillGradId += 2 -- always even
  return $ R.renderFillTextureDefs ident s

lineTextureDefs :: SVGFloat n => Style v n -> SvgRenderM
lineTextureDefs s = do
  ident <- use lineGradId
  lineGradId += 2 -- always odd
  return $ R.renderLineTextureDefs ident s

instance SVGFloat n => Backend SVG V2 n where
  data Render  SVG V2 n = R SvgRenderM
  type Result  SVG V2 n = SvgM
  data Options SVG V2 n = SVGOptions
    { _size           :: SizeSpec V2 n   -- ^ The requested size.
    , _svgDefinitions :: [Attribute]
                          -- ^ Custom definitions that will be added to the @defs@
                          --   section of the output.
    }

  renderRTree _ opts rt = evalState svgOutput initialSvgRenderState
    where
      svgOutput = do
        let R r    = toRender rt
            V2 w h = specToSize 100 (opts^.sizeSpec)
        svg <- r
        return $ R.svgHeader w h (opts^.svgDefinitions) svg

  adjustDia c opts d = adjustDia2D sizeSpec c opts (d # reflectY)

instance SVGFloat n => BackendBuild SVG V2 n where
  outputSize = sizeSpec
  saveDia outFile opts dia = renderToFile outFile build
    where
      build = renderDia SVG opts dia

toRender :: forall n. SVGFloat n => RTree SVG V2 n Annotation -> Render SVG V2 n
toRender = fromRTree
  . Node (RStyle (mempty # recommendFillColor (transparent :: AlphaColour Double)))
  . (:[])
  . splitTextureFills
    where
      fromRTree (Node (RAnnot (Href uri)) rs)
        = R $ do
            let R r =  foldMap fromRTree rs
            svg <- r
            return $ a_ [xlinkHref_ $ toText uri] svg
      fromRTree (Node (RAnnot (OpacityGroup o)) rs)
        = R $ do
            let R r =  foldMap fromRTree rs
            svg <- r
            return $ g_ [opacity_ $ toText o] svg
      fromRTree (Node (RPrim p) _) = render SVG p
      fromRTree (Node (RStyle sty) ts)
        = R $ do
            let R r = foldMap fromRTree ts

            -- render subtrees
            svg <- r

            idFill <- use fillGradId
            idLine <- use lineGradId
            clippedSvg <- renderSvgWithClipping svg sty
            lineGradDefs <- lineTextureDefs sty
            fillGradDefs <- fillTextureDefs sty
            let textureDefs = fillGradDefs `mappend` lineGradDefs
            return $ (g_ $ R.renderStyles idFill idLine sty)
                     (textureDefs `mappend` clippedSvg)
      fromRTree (Node _ rs) = foldMap fromRTree rs

sizeSpec :: SVGFloat n => Lens' (Options SVG V2 n) (SizeSpec V2 n)
sizeSpec = lens getter setter
  where
    getter (SVGOptions {_size = s}) = s
    setter o s = o {_size = s}

getSVGDefs :: SVGFloat n => Options SVG V2 n -> [Attribute]
getSVGDefs (SVGOptions {_svgDefinitions = d}) = d

setSVGDefs :: SVGFloat n => Options SVG V2 n -> [Attribute] -> Options SVG V2 n
setSVGDefs o d = o {_svgDefinitions = d}

svgDefinitions :: SVGFloat n => Lens' (Options SVG V2 n) [Attribute]
svgDefinitions = lens getSVGDefs setSVGDefs

instance SVGFloat n => Renderable (Path V2 n) SVG where
  render _ = R . return . R.renderPath

instance SVGFloat n => Renderable (Text n) SVG where
  render _ = R . return . R.renderText

instance SVGFloat n => Renderable (DImage n Embedded) SVG where
  render _ = R . return . R.renderDImageEmb

-- TODO: instance Renderable Image SVG where

-- | Render a diagram as an SVG, writing to the specified output file
--   and using the requested size.
renderSVG :: SVGFloat n => FilePath -> SizeSpec V2 n -> QDiagram SVG V2 n Any -> IO ()
renderSVG outFile spec
  = BS.writeFile outFile
  . renderBS
  . renderDia SVG (SVGOptions spec [])

-- | Render a diagram as a pretty printed SVG.
renderPretty :: SVGFloat n => FilePath -> SizeSpec V2 n -> QDiagram SVG V2 n Any -> IO ()
renderPretty outFile spec
  = LT.writeFile outFile
  . prettyText
  . renderDia SVG (SVGOptions spec [])

data Img = Img !Char !BS.ByteString deriving Typeable

-- | Load images (JPG/PNG/...) in a SVG specific way.
loadImageSVG :: SVGFloat n => FilePath -> IO (QDiagram SVG V2 n Any)
loadImageSVG fp = do
    raw <- SBS.readFile fp
    dyn <- eIO $ decodeImage raw
    let dat = BS.fromChunks [raw]
    let pic t d = return $ image (DImage (ImageNative (Img t d))
                                   (dynamicMap imageWidth dyn)
                                   (dynamicMap imageHeight dyn) mempty)
    if pngHeader `SBS.isPrefixOf` raw then pic 'P' dat else do
    if jpgHeader `SBS.isPrefixOf` raw then pic 'J' dat else do
    case dyn of
      (ImageYCbCr8 _) -> pic 'J' dat
      _               -> pic 'P' =<< eIO (encodeDynamicPng dyn)
  where pngHeader :: SBS.ByteString
        pngHeader = SBS.pack [137, 80, 78, 71, 13, 10, 26, 10]
        jpgHeader :: SBS.ByteString
        jpgHeader = SBS.pack [0xFF, 0xD8]
        eIO :: Either String a -> IO a
        eIO = either fail return

instance SVGFloat n => Renderable (DImage n (Native Img)) SVG where
  render _ di@(DImage (ImageNative (Img t d)) _ _ _) = R $ do
    mime <- case t of
          'J' -> return "image/jpeg"
          'P' -> return "image/png"
          _   -> fail   "Unknown mime type while rendering image"
    return $ R.renderDImage di $ R.dataUri mime d

deriving instance Hashable Attribute

instance (Hashable n, SVGFloat n) => Hashable (Options SVG V2 n) where
  hashWithSalt s  (SVGOptions sz defs) = s `hashWithSalt` sz `hashWithSalt` defs
