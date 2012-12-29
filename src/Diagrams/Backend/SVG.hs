{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , DeriveDataTypeable
  #-}
{-|
  The SVG backend.
-}
module Diagrams.Backend.SVG
  ( SVG(..) -- rendering token
  , Options(..) -- for rendering options specific to SVG

  , renderSVG
  ) where

-- from base
import Data.Typeable
import Control.Monad.State

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

data SVG = SVG
    deriving (Show, Typeable)


data SvgRenderState = SvgRenderState { clipPathId :: Int }

initialSvgRenderState :: SvgRenderState
initialSvgRenderState = SvgRenderState 0

-- Monad to keep track of state when rendering an SVG.
-- Currently just keeps a monotonically increasing counter
-- for assiging unique clip path ID
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

-- renders a <g> element with styles applied as attributes.
renderStyledGroup :: Style v -> (S.Svg -> S.Svg)
renderStyledGroup s = S.g ! R.renderStyles s

renderSvgWithClipping :: S.Svg             -- Input SVG
                      -> Style v           -- Styles
                      -> Int               -- Clip Path ID
                      -> Transformation R2 -- Freeze transform
                      -> S.Svg             -- Resulting svg
renderSvgWithClipping svg s id_ t = do
  R.renderClip (transform (inv t) <$> getClip <$> getAttr s) id_  -- Clipping if any
  svg                                       -- The diagram

instance Backend SVG R2 where
  data Render  SVG R2 = R SvgRenderM
  type Result  SVG R2 = S.Svg
  data Options SVG R2 = SVGOptions
                        { size :: SizeSpec2D   -- ^ The requested size.
                        }

  -- Here the SVG backend is different from the other backends.  We
  -- give a different definition of renderDia, where only the
  -- non-frozen transformation is applied to the primitives before
  -- they are passed to render.  This means that withStyle is
  -- responsible for applying the frozen transformation to the
  -- primitives.
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

  -- This implementation of renderDia is the same as the default one,
  -- except that it only applies the non-frozen transformation to the
  -- primitives before passing them to render.
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

------------------------------------------------------------

renderSVG :: FilePath -> SizeSpec2D -> Diagram SVG R2 -> IO ()
renderSVG outFile sizeSpec
  = BS.writeFile outFile
  . renderSvg
  . renderDia SVG (SVGOptions sizeSpec)