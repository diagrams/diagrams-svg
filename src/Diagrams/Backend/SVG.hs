{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

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
-- > renderDia :: b -> Options b v -> QDiagram b v m -> Result b v
--
-- (omitting a few type class constraints).  @b@ represents the
-- backend type, @v@ the vector space, and @m@ the type of monoidal
-- query annotations on the diagram.  'Options' and 'Result' are
-- associated data and type families, respectively, which yield the
-- type of option records and rendering results specific to any
-- particular backend.  For @b ~ SVG@ and @v ~ R2@, we have
--
-- > data Options SVG R2 = SVGOptions
-- >                       { size :: SizeSpec2D   -- ^ The requested size.
-- >                       , svgDefinitions :: Maybe S.Svg
-- >                       -- ^ Custom definitions that will be added to the @defs@
-- >                       --  section of the output.
-- >                       }
--
-- @
-- data family Render SVG R2 = R 'SvgRenderM'
-- @
--
-- @
-- type family Result SVG R2 = 'Text.Blaze.Svg11.Svg'
-- @
--
-- So the type of 'renderDia' resolves to
--
-- @
-- renderDia :: SVG -> Options SVG R2 -> QDiagram SVG R2 m -> 'Text.Blaze.Svg11.Svg'
-- @
--
-- which you could call like @renderDia SVG (SVGOptions (Width 250))
-- myDiagram@.  (In some situations GHC may not be able to infer the
-- type @m@, in which case you can use a type annotation to specify
-- it; it may be useful to simply use the type synonym @Diagram SVG
-- R2 = QDiagram SVG R2 Any@.) This returns an
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
-- for testing
import           Data.Foldable                as F (foldMap, mapM_)
import           Data.Tree

-- from base
import           Control.Monad.State
import           Data.Typeable
import           GHC.Generics                 (Generic)

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
import           Diagrams.Prelude             hiding (view, size)
import           Diagrams.TwoD.Adjust         (adjustDia2D)
import           Diagrams.TwoD.Attributes     (splitTextureFills)
import           Diagrams.TwoD.Path           (Clip (Clip))
import           Diagrams.TwoD.Size           (sizePair)
import           Diagrams.TwoD.Text

-- from blaze-svg
import           Text.Blaze.Internal          (ChoiceString (..), MarkupM (..),
                                               StaticString (..))
import           Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import           Text.Blaze.Svg11             ((!))
import qualified Text.Blaze.Svg11             as S
import           Text.Blaze.Svg11.Attributes  (xlinkHref)
import qualified Text.Blaze.Svg.Renderer.Pretty as Pretty

-- from this package
import qualified Graphics.Rendering.SVG       as R
import           Graphics.Rendering.SVG       (SVGFloat)

-- | @SVG@ is simply a token used to identify this rendering backend
--   (to aid type inference).
data SVG = SVG
  deriving (Show, Typeable)

type B = SVG

data SvgRenderState = SvgRenderState { _clipPathId  :: Int
                                     , _fillGradId  :: Int
                                     , _lineGradId  :: Int
                                     , _isLocalText :: Bool }

makeLenses ''SvgRenderState

-- Fill gradients ids are even, line gradient ids are odd.
initialSvgRenderState :: SvgRenderState
initialSvgRenderState = SvgRenderState 0 0 1 True

-- | Monad to keep track of state when rendering an SVG.
--   Currently just keeps a monotonically increasing counter
--   for assiging a unique clip path ID.
type SvgRenderM = State SvgRenderState S.Svg

instance SVGFloat n => Monoid (Render SVG V2 n) where
  mempty  = R $ return mempty
  (R r1) `mappend` (R r2_) =
    R $ do
      svg1 <- r1
      svg2 <- r2_
      return (svg1 `mappend` svg2)

-- Handle clip attributes.
renderSvgWithClipping :: forall n. SVGFloat n
                      => S.Svg             -- ^ Input SVG
                      -> Style V2 n    -- ^ Styles
                      -> SvgRenderM        -- ^ Resulting svg
renderSvgWithClipping svg s =
  case op Clip <$> getAttr s of
    Nothing -> return svg
    Just paths -> renderClips paths
  where
    renderClips :: SVGFloat n => [Path V2 n] -> SvgRenderM
    renderClips []     = return svg
    renderClips (p:ps) = do
      clipPathId += 1
      id_ <- use clipPathId
      R.renderClip p id_ <$> renderClips ps

-- | Create a new texture defs svg element using the style and the current
--   id number, then increment the gradient id number.
fillTextureDefs :: SVGFloat n => Style v n -> SvgRenderM
fillTextureDefs s = do
  id_ <- use fillGradId
  fillGradId += 2 -- always even
  return $ R.renderFillTextureDefs id_ s

lineTextureDefs :: SVGFloat n => Style v n -> SvgRenderM
lineTextureDefs s = do
  id_ <- use lineGradId
  lineGradId += 2 -- always odd
  return $ R.renderLineTextureDefs id_ s

instance SVGFloat n => Backend SVG V2 n where
  data Render  SVG V2 n = R SvgRenderM
  type Result  SVG V2 n = S.Svg
  data Options SVG V2 n = SVGOptions
    { _size :: SizeSpec2D n   -- ^ The requested size.
    , _svgDefinitions :: Maybe S.Svg
                          -- ^ Custom definitions that will be added to the @defs@
                          --   section of the output.
    }

  renderRTree _ opts rt = evalState svgOutput initialSvgRenderState
    where
      svgOutput = do
        let R r   = toRender rt
            (w,h) = sizePair (opts^.sizeSpec)
        svg <- r
        return $ R.svgHeader w h (opts^.svgDefinitions) svg

  adjustDia c opts d = adjustDia2D sizeSpec c opts (d # reflectY)

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
            return $ (S.a ! xlinkHref (S.toValue uri)) svg
      fromRTree (Node (RPrim p) _) = render SVG p
      fromRTree (Node (RStyle sty) ts)
        = R $ do
            let R r = foldMap fromRTree ts

            -- save current setting for local text
            oldIsLocal <- use isLocalText

            -- check if this style speficies a font size in Local units
            F.mapM_ (assign isLocalText)
                    -- getNumAttr getFontSizeIsLocal sty
                    ((getFontSizeIsLocal :: FontSize n -> Bool) <$> getAttr sty)    

            -- render subtrees
            svg <- r
            -- restore the old setting for local text
            isLocalText .= oldIsLocal

            idFill <- use fillGradId
            idLine <- use lineGradId
            clippedSvg <- renderSvgWithClipping svg sty
            lineGradDefs <- lineTextureDefs sty
            fillGradDefs <- fillTextureDefs sty
            let textureDefs = fillGradDefs `mappend` lineGradDefs
            return $ (S.g ! R.renderStyles idFill idLine sty)
                     (textureDefs `mappend` clippedSvg)
      fromRTree (Node _ rs) = foldMap fromRTree rs

getSize :: SVGFloat n => Options SVG V2 n -> SizeSpec2D n
getSize (SVGOptions {_size = s}) = s

setSize :: SVGFloat n => Options SVG V2 n -> SizeSpec2D n -> Options SVG V2 n
setSize o s = o {_size = s}

sizeSpec :: SVGFloat n => Lens' (Options SVG V2 n) (SizeSpec2D n)
sizeSpec = lens getSize setSize

getSVGDefs :: SVGFloat n => Options SVG V2 n -> Maybe S.Svg
getSVGDefs (SVGOptions {_svgDefinitions = d}) = d

setSVGDefs :: SVGFloat n => Options SVG V2 n -> Maybe S.Svg -> Options SVG V2 n
setSVGDefs o d = o {_svgDefinitions = d}

svgDefinitions :: SVGFloat n => Lens' (Options SVG V2 n) (Maybe S.Svg)
svgDefinitions = lens getSVGDefs setSVGDefs

instance (Hashable n, SVGFloat n) => Hashable (Options SVG V2 n) where
  hashWithSalt s (SVGOptions sz defs) =
    s `hashWithSalt` sz `hashWithSalt` defs

instance Hashable StaticString where
  hashWithSalt s (StaticString dl bs txt)
    = s `hashWithSalt` dl [] `hashWithSalt` bs `hashWithSalt` txt

deriving instance Generic ChoiceString

instance Hashable ChoiceString

instance Hashable (MarkupM a) where
  hashWithSalt s (Parent w x y z) =
    s          `hashWithSalt`
    (0 :: Int) `hashWithSalt`
    w          `hashWithSalt`
    x          `hashWithSalt`
    y          `hashWithSalt`
    z
  hashWithSalt s (CustomParent cs m) =
    s          `hashWithSalt`
    (1 :: Int) `hashWithSalt`
    cs         `hashWithSalt`
    m
  hashWithSalt s (Leaf s1 s2 s3) =
    s          `hashWithSalt`
    (2 :: Int) `hashWithSalt`
    s1         `hashWithSalt`
    s2         `hashWithSalt`
    s3
  hashWithSalt s (CustomLeaf cs b) =
    s          `hashWithSalt`
    (3 :: Int) `hashWithSalt`
    cs         `hashWithSalt`
    b
  hashWithSalt s (Content cs) =
    s          `hashWithSalt`
    (4 :: Int) `hashWithSalt`
    cs
  hashWithSalt s (Append m1 m2) =
    s          `hashWithSalt`
    (5 :: Int) `hashWithSalt`
    m1         `hashWithSalt`
    m2
  hashWithSalt s (AddAttribute s1 s2 s3 m) =
    s          `hashWithSalt`
    (6 :: Int) `hashWithSalt`
    s1         `hashWithSalt`
    s2         `hashWithSalt`
    s3         `hashWithSalt`
    m
  hashWithSalt s (AddCustomAttribute s1 s2 m) =
    s          `hashWithSalt`
    (7 :: Int) `hashWithSalt`
    s1         `hashWithSalt`
    s2         `hashWithSalt`
    m
  hashWithSalt s Empty = s `hashWithSalt` (8 :: Int)

instance SVGFloat n => Renderable (Path V2 n) SVG where
  render _ = R . return . R.renderPath

instance SVGFloat n => Renderable (Text n) SVG where
  render _ t = R $ do
    isLocal <- use isLocalText
    return $ R.renderText isLocal t

instance SVGFloat n => Renderable (DImage n Embedded) SVG where
  render _ = R . return . R.renderDImageEmb

-- TODO: instance Renderable Image SVG where

-- | Render a diagram as an SVG, writing to the specified output file
--   and using the requested size.
renderSVG :: SVGFloat n => FilePath -> SizeSpec2D n -> Diagram SVG V2 n -> IO ()
renderSVG outFile szSpec
  = BS.writeFile outFile
  . renderSvg
  . renderDia SVG (SVGOptions szSpec Nothing)

-- | Render a diagram as a pretty printed SVG.
renderPretty :: SVGFloat n => FilePath -> SizeSpec2D n -> Diagram SVG V2 n -> IO ()
renderPretty outFile szSpec
  = writeFile outFile
  . Pretty.renderSvg
  . renderDia SVG (SVGOptions szSpec Nothing)



data Img = Img !Char !BS.ByteString deriving Typeable

-- | Load images (JPG/PNG/...) in a SVG specific way.
loadImageSVG :: SVGFloat n => FilePath -> IO (Diagram SVG V2 n)
loadImageSVG fp = do
    raw <- SBS.readFile fp
    dyn <- eIO $ decodeImage raw
    let dat = BS.fromChunks [raw]
    let pic t d = return $ image (DImage (ImageNative (Img t d)) (dynamicMap imageWidth dyn) (dynamicMap imageHeight dyn) mempty)
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
          _   -> fail "Unknown mime type while rendering image"
    return $ R.renderDImage di $ R.dataUri mime d

