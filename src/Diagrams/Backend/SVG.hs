{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NondecreasingIndentation   #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
  -- UndecidableInstances needed for ghc < 707

----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.SVG
-- Copyright   :  (c) 2011-2015 diagrams-svg team (see LICENSE)
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
-- particular backend.  For @b ~ SVG@, @v ~ V2@, we have
--
-- > data Options SVG V2 n = SVGOptions
-- >     { _size           :: SizeSpec V2 n   -- ^ The requested size.
-- >     , _svgDefinitions :: Maybe SvgM
-- >                           -- ^ Custom definitions that will be added to the @defs@
-- >                           --   section of the output.
-- >     , _idPrefix       :: T.Text
-- >     }
--
-- @
-- data family Render SVG V2 n = R 'SvgRenderM n'
-- @
--
-- @
-- type family Result SVG V2 n = 'Graphics.Rendering.SVG.SvgM'
-- @
--
-- So the type of 'renderDia' resolves to
--
-- @
-- renderDia :: SVG -> Options SVG V2 n -> QDiagram SVG V2 n m -> 'Graphics.Rendering.SVG.SvgM'
-- @
--
-- which you could call like @renderDia SVG (SVGOptions (mkWidth 250)
-- Nothing "") myDiagram@ (if you have the 'OverloadedStrings' extension
-- enabled; otherwise you can use 'Text.pack ""').  (In some
-- situations GHC may not be able to infer the type @m@, in which case
-- you can use a type annotation to specify it; it may be useful to
-- simply use the type synonym @Diagram SVG = QDiagram SVG V2 Double
-- Any@.) This returns an 'Graphics.Rendering.SVG.SvgM' value, which
-- you can, /e.g./ render to a 'ByteString' using 'Lucid.Svg.renderBS'
-- from the 'lucid' package.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.SVG
  ( SVG(..) -- rendering token
  , B
    -- for rendering options specific to SVG
  , Options(..), sizeSpec, svgDefinitions, idPrefix, svgAttributes, generateDoctype
  , SVGFloat

  , renderSVG
  , renderSVG'
  , renderPretty
  , renderPretty'
  , loadImageSVG
  ) where

-- from JuicyPixels
import           Codec.Picture
import           Codec.Picture.Types      (dynamicMap)

#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable            as F (foldMap)
#endif
import qualified Data.Text                as T
import           Data.Text.Lazy.IO        as LT
import           Data.Tree
import           System.FilePath

-- from base
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import           Data.Typeable

-- from hashable
import           Data.Hashable            (Hashable (), hashWithSalt)

-- from bytestring
import qualified Data.ByteString          as SBS
import qualified Data.ByteString.Lazy     as BS

-- from lens
import           Control.Lens             hiding (transform, ( # ))

-- from diagrams-core
import           Diagrams.Core.Compile
import           Diagrams.Core.Types      (Annotation (..))

-- from diagrams-lib
import           Diagrams.Prelude         hiding (Attribute, size, view, local)
import           Diagrams.TwoD.Adjust     (adjustDia2D)
import           Diagrams.TwoD.Attributes (splitTextureFills)
import           Diagrams.TwoD.Path       (Clip (Clip))
import           Diagrams.TwoD.Text

-- from lucid-svg
import           Lucid.Svg

-- from this package
import           Graphics.Rendering.SVG   (SVGFloat, SvgM)
import qualified Graphics.Rendering.SVG   as R

-- | @SVG@ is simply a token used to identify this rendering backend
--   (to aid type inference).
data SVG = SVG
  deriving (Show, Typeable)

type B = SVG

type instance V SVG = V2
type instance N SVG = Double

data Environment n = Environment
  { _style :: Style V2 n
  , __pre :: T.Text
  }

makeLenses ''Environment

data SvgRenderState = SvgRenderState
  { _clipPathId :: Int
  , _fillGradId :: Int
  , _lineGradId :: Int
  }

makeLenses ''SvgRenderState

initialEnvironment :: SVGFloat n => T.Text -> Environment n
initialEnvironment = Environment (mempty # recommendFillColor transparent)

-- Fill gradients ids are even, line gradient ids are odd.
initialSvgRenderState :: SvgRenderState
initialSvgRenderState = SvgRenderState 0 0 1

-- | Monad to keep track of environment and state when rendering an SVG.
type SvgRenderM n = ReaderT (Environment n) (State SvgRenderState) SvgM

runRenderM :: SVGFloat n => T.Text -> SvgRenderM n -> SvgM
runRenderM o s = flip evalState initialSvgRenderState
               $ runReaderT  s (initialEnvironment o)

instance SVGFloat n => Monoid (Render SVG V2 n) where
  mempty = R $ return mempty
  R r1 `mappend` R r2_ = R $ do
    svg1 <- r1
    svg2 <- r2_
    return (svg1 `mappend` svg2)

-- Handle clip attributes.
--
renderSvgWithClipping :: forall n. SVGFloat n
                      => T.Text
                      -> SvgM          -- ^ Input SVG
                      -> Style V2 n    -- ^ Styles
                      -> SvgRenderM n  -- ^ Resulting svg

renderSvgWithClipping prefix svg s =
  case op Clip <$> getAttr s of
    Nothing    -> return svg
    Just paths -> renderClips paths
  where
    renderClips :: SVGFloat n => [Path V2 n] -> SvgRenderM n
    renderClips []     = return svg
    renderClips (p:ps) = do
      clipPathId += 1
      ident <- use clipPathId
      R.renderClip p prefix ident <$> renderClips ps

-- | Create a new texture defs svg element using the style and the current
--   id number, then increment the gradient id number.
fillTextureDefs :: SVGFloat n => Style v n -> SvgRenderM n
fillTextureDefs s = do
  ident <- use fillGradId
  fillGradId += 2 -- always even
  return $ R.renderFillTextureDefs ident s

lineTextureDefs :: SVGFloat n => Style v n -> SvgRenderM n
lineTextureDefs s = do
  ident <- use lineGradId
  lineGradId += 2 -- always odd
  return $ R.renderLineTextureDefs ident s

instance SVGFloat n => Backend SVG V2 n where
  newtype Render  SVG V2 n = R (SvgRenderM n)
  type    Result  SVG V2 n = SvgM
  data    Options SVG V2 n = SVGOptions
    { _size            :: SizeSpec V2 n   -- ^ The requested size.
    , _svgDefinitions  :: Maybe SvgM
                          -- ^ Custom definitions that will be added to the @defs@
                          --   section of the output.
    , _idPrefix        :: T.Text
    , _svgAttributes   :: [Attribute]
                          -- ^ Attriubtes to apply to the entire svg element.
    , _generateDoctype :: Bool
    }

  renderRTree :: SVG -> Options SVG V2 n -> RTree SVG V2 n Annotation -> Result SVG V2 n
  renderRTree _ opts rt = runRenderM (opts ^.idPrefix) svgOutput
    where
      svgOutput = do
        let R r    = rtree (splitTextureFills rt)
            V2 w h = specToSize 100 (opts^.sizeSpec)
        svg <- r
        return $ R.svgHeader w h (opts^.svgDefinitions)
                                 (opts^.svgAttributes)
                                 (opts^.generateDoctype) svg

  adjustDia c opts d = adjustDia2D sizeSpec c opts (d # reflectY)

rtree :: SVGFloat n => RTree SVG V2 n Annotation -> Render SVG V2 n
rtree (Node n rs) = case n of
  RPrim p                 -> render SVG p
  RStyle sty              -> R $ local (over style (<> sty)) r
  RAnnot (OpacityGroup o) -> R $ g_ [opacity_ $ toText o] <$> r
  RAnnot (Href uri)       -> R $ a_ [xlinkHref_ $ T.pack uri] <$> r
  _                       -> R r
  where
    R r = foldMap rtree rs

-- | Lens onto the size of the svg options.
sizeSpec :: SVGFloat n => Lens' (Options SVG V2 n) (SizeSpec V2 n)
sizeSpec f opts = f (_size opts) <&> \s -> opts { _size = s }

-- | Lens onto the svg definitions of the svg options.
svgDefinitions :: SVGFloat n => Lens' (Options SVG V2 n) (Maybe SvgM)
svgDefinitions f opts =
  f (_svgDefinitions opts) <&> \ds -> opts { _svgDefinitions = ds }

-- | Lens onto the idPrefix of the svg options. This is the prefix given
--   to clipping paths to distinguish them from other svg files in the
--   same web page.
idPrefix :: SVGFloat n => Lens' (Options SVG V2 n) T.Text
idPrefix f opts = f (_idPrefix opts) <&> \i -> opts { _idPrefix = i }

-- | Lens onto the svgAttributes field of the svg options. This field
--   is provided to supply SVG attributes to the entire diagram.
svgAttributes :: SVGFloat n => Lens' (Options SVG V2 n) [Attribute]
svgAttributes f opts =
  f (_svgAttributes opts) <&> \ds -> opts { _svgAttributes = ds }

-- | Lens onto the generateDoctype field of the svg options. Set
--   to False if you don't want a doctype tag included in the output.
generateDoctype :: SVGFloat n => Lens' (Options SVG V2 n) Bool
generateDoctype f opts =
  f (_generateDoctype opts) <&> \ds -> opts { _generateDoctype = ds }

-- paths ---------------------------------------------------------------

attributedRender :: SVGFloat n => SvgM -> SvgRenderM n
attributedRender svg = do
  SvgRenderState _idClip idFill idLine <- get
  Environment sty preT <- ask
  clippedSvg   <- renderSvgWithClipping preT svg sty
  lineGradDefs <- lineTextureDefs sty
  fillGradDefs <- fillTextureDefs sty
  return $ do
    let gDefs = fillGradDefs >> lineGradDefs
    defs <- gDefs
    unless (defs == mempty) (defs_ gDefs)
    g_ (R.renderStyles idFill idLine sty) clippedSvg

instance SVGFloat n => Renderable (Path V2 n) SVG where
  render _ = R . attributedRender . R.renderPath

instance SVGFloat n => Renderable (Text n) SVG where
  render _ = R . attributedRender . R.renderText

instance SVGFloat n => Renderable (DImage n Embedded) SVG where
  render _ = R . return . R.renderDImageEmb

-- | Render a diagram as an SVG, writing to the specified output file
--   and using the requested size.
renderSVG :: SVGFloat n => FilePath -> SizeSpec V2 n -> QDiagram SVG V2 n Any -> IO ()
renderSVG outFile spec = renderSVG' outFile (SVGOptions spec Nothing (mkPrefix outFile) [] True)

-- | Render a diagram as a pretty printed SVG.
renderPretty :: SVGFloat n => FilePath -> SizeSpec V2 n -> QDiagram SVG V2 n Any -> IO ()
renderPretty outFile spec = renderPretty' outFile (SVGOptions spec Nothing (mkPrefix outFile)[] True)

-- Create a prefile using the basename of the output file. Only standard
-- letters are considered.
mkPrefix :: FilePath -> T.Text
mkPrefix = T.filter isAlpha . T.pack . takeBaseName

-- | Render a diagram as an SVG, writing to the specified output file
--   and using the backend options. The id prefix is derived from the
--   basename of the output file.
renderSVG' :: SVGFloat n => FilePath -> Options SVG V2 n -> QDiagram SVG V2 n Any -> IO ()
renderSVG' outFile opts = BS.writeFile outFile . renderBS . renderDia SVG opts

-- | Render a diagram as a pretty printed SVG to the specified output
--   file and using the backend options. The id prefix is derived from the
--   basename of the output file.
renderPretty' :: SVGFloat n => FilePath -> Options SVG V2 n -> QDiagram SVG V2 n Any -> IO ()
renderPretty' outFile opts = LT.writeFile outFile . prettyText . renderDia SVG opts

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
    if | pngHeader `SBS.isPrefixOf` raw -> pic 'P' dat
       | jpgHeader `SBS.isPrefixOf` raw -> pic 'J' dat
       | otherwise -> case dyn of
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

instance (Hashable n, SVGFloat n) => Hashable (Options SVG V2 n) where
  hashWithSalt s  (SVGOptions sz defs ia sa gd) =
    s  `hashWithSalt`
    sz `hashWithSalt`
    ds `hashWithSalt`
    ia `hashWithSalt`
    sa `hashWithSalt`
    gd
      where ds = fmap renderBS defs
