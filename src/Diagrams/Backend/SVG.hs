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
  ) where

-- from base
import Data.Typeable
import Control.Monad.State

-- from diagrams-lib
import Diagrams.Prelude
import Diagrams.TwoD.Path (getClip)
import Diagrams.TwoD.Adjust (adjustDia2D)
import Diagrams.TwoD.Text

-- from blaze-svg
import qualified Text.Blaze.Svg11 as S
import Text.Blaze.Svg11 ((!))

-- from colour
import Data.Colour (transparent)

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

renderSvgWithClipping :: S.Svg     -- Input SVG
                      -> Style v -- Styles
                      -> Int     -- Clip Path ID
                      -> S.Svg   -- Resulting svg
renderSvgWithClipping svg s id_ = do
  R.renderClip (getClip <$> getAttr s) id_  -- Clipping if any
  svg  -- The diagram

instance Backend SVG R2 where
  data Render  SVG R2 = R SvgRenderM
  type Result  SVG R2 = S.Svg
  data Options SVG R2 = SVGOptions
                        { fileName     :: String       -- ^ the name of the file you want generated
                        , size :: SizeSpec2D           -- ^ The requested size.
                        }

  withStyle _ s _ (R r) =
    R $ do
      incrementClipPath
      clipPathId_ <- gets clipPathId
      svg <- r
      let styledSvg = renderStyledGroup s ! (R.renderClipPathId s clipPathId_) $ renderSvgWithClipping svg s clipPathId_
      return styledSvg

  doRender _ (SVGOptions _ sz) (R r) =
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

  adjustDia c opts d = adjustDia2D size setSvgSize c opts (d # reflectY
                                                               # fcA transparent
                                                          )
    where setSvgSize sz o = o { size = sz }

instance Renderable (Segment R2) SVG where
  render c = render c . flip Trail False . (:[])

instance Renderable (Trail R2) SVG where
  render c t = render c $ Path [(p2 (0,0), t)]

instance Renderable (Path R2) SVG where
  render _ = R . return . R.renderPath

instance Renderable Text SVG where
  render _ = R . return . R.renderText

-- TODO: instance Renderable Image SVG where
