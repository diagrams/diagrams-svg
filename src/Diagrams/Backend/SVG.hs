{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DeriveDataTypeable    #-}

-----------------------------------------------------------------------------
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
-- * You can use the 'renderSVG' function provided by this module,
--   which gives you more flexible programmatic control over when and
--   how images are output (making it easy to, for example, write a
--   single program that outputs multiple images, or one that outputs
--   images dynamically based on user input, and so on).
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
  , Options(..) -- for rendering options specific to SVG

  , renderSVG
  ) where

-- from base
import Data.Typeable
import Control.Monad.State

-- from bytestring
import qualified Data.ByteString.Lazy as BS

-- from diagrams-lib
import Diagrams.Prelude
import Diagrams.TwoD.Path (getClip)
import Diagrams.TwoD.Adjust (adjustDia2D)
import Diagrams.TwoD.Text

-- from monoid-extras
import Data.Monoid.Split (Split(..))

-- from blaze-svg
import qualified Text.Blaze.Svg11 as S
import Text.Blaze.Svg11 ((!))
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)

-- from this package
import qualified Graphics.Rendering.SVG as R

-- | @SVG@ is simply a token used to identify this rendering backend
--   (to aid type inference).
data SVG = SVG
    deriving (Show, Typeable)

data SvgRenderState = SvgRenderState { clipPathId :: Int }

initialSvgRenderState :: SvgRenderState
initialSvgRenderState = SvgRenderState 0

-- | Monad to keep track of state when rendering an SVG.
--   Currently just keeps a monotonically increasing counter
--   for assiging a unique clip path ID.
type SvgRenderM = State SvgRenderState S.Svg

incrementClipPath :: State SvgRenderState ()
incrementClipPath = modify (\(SvgRenderState x) -> SvgRenderState (x + 1))

instance Monoid (Render SVG R2) where
  mempty  = R $ return mempty
  (R r1) `mappend` (R r2_) =
    R $ do
      svg1 <- r1
      svg2 <- r2_
      return (svg1 `mappend` svg2)

-- | Renders a <g> element with styles applied as attributes.
renderStyledGroup :: Style v -> (S.Svg -> S.Svg)
renderStyledGroup s = S.g ! R.renderStyles s

renderSvgWithClipping :: S.Svg             -- ^ Input SVG
                      -> Style v           -- ^ Styles
                      -> Int               -- ^ Clip Path ID
                      -> Transformation R2 -- ^ Freeze transform
                      -> S.Svg             -- ^ Resulting svg
renderSvgWithClipping svg s id_ t = do
  R.renderClip (transform (inv t) <$> getClip <$> getAttr s) id_  -- Clipping if any
  svg                                       -- The diagram

instance Backend SVG R2 where
  data Render  SVG R2 = R SvgRenderM
  type Result  SVG R2 = S.Svg
  data Options SVG R2 = SVGOptions
                        { size :: SizeSpec2D   -- ^ The requested size.
                        }

  -- | Here the SVG backend is different from the other backends.  We
  --   give a different definition of renderDia, where only the
  --   non-frozen transformation is applied to the primitives before
  --   they are passed to render.  This means that withStyle is
  --   responsible for applying the frozen transformation to the
  --   primitives.
  withStyle _ s t (R r) =
    R $ do
      incrementClipPath
      clipPathId_ <- gets clipPathId
      svg <- r
      let styledSvg = renderStyledGroup s ! (R.renderClipPathId s clipPathId_) $
                        renderSvgWithClipping svg s clipPathId_ t
      -- This is where the frozen transformation is applied.
      return (R.renderTransform t styledSvg)

  doRender _ (SVGOptions sz) (R r) =
    evalState svgOutput initialSvgRenderState
   where
    svgOutput = do
      svg <- r
      let (w,h) = case sz of
                    Width w'   -> (w',w')
                    Height h'  -> (h',h')
                    Dims w' h' -> (w',h')
                    Absolute   -> (100,100)
      return $ R.svgHeader w h $ svg

  adjustDia c opts d = adjustDia2D size setSvgSize c opts
                         (d # reflectY
                            # recommendFillColor
                                (transparent :: AlphaColour Double)
                         )
    where setSvgSize sz o = o { size = sz }

  -- | This implementation of renderDia is the same as the default one,
  --   except that it only applies the non-frozen transformation to the
  --   primitives before passing them to render.
  renderDia SVG opts d =
    doRender SVG opts' . mconcat . map renderOne . prims $ d'
      where (opts', d') = adjustDia SVG opts d
            renderOne :: (Prim SVG R2, (Split (Transformation R2), Style R2))
                      -> Render SVG R2
            renderOne (p, (M t,      s))
              = withStyle SVG s mempty (render SVG (transform t p))

            renderOne (p, (t1 :| t2, s))
              -- Here is the difference from the default
              -- implementation: "t2" instead of "t1 <> t2".
              = withStyle SVG s t1 (render SVG (transform t2 p))

instance Renderable (Segment R2) SVG where
  render c = render c . flip Trail False . (:[])

instance Renderable (Trail R2) SVG where
  render c t = render c $ Path [(p2 (0,0), t)]

instance Renderable (Path R2) SVG where
  render _ = R . return . R.renderPath

instance Renderable Text SVG where
  render _ = R . return . R.renderText

-- TODO: instance Renderable Image SVG where


-- | Render a diagram as an SVG, writing to the specified output file
--   and using the requested size.
renderSVG :: FilePath -> SizeSpec2D -> Diagram SVG R2 -> IO ()
renderSVG outFile sizeSpec
  = BS.writeFile outFile
  . renderSvg
  . renderDia SVG (SVGOptions sizeSpec)