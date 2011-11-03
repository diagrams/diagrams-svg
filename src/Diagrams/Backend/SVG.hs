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
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Typeable

-- from colour
import Data.Colour (transparent)

-- from diagrams-lib
import Diagrams.Prelude
import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Adjust (adjustDia2D)
import Diagrams.TwoD.Text

-- from blaze-builder
import qualified Blaze.ByteString.Builder as B

-- from this package
import qualified Graphics.Rendering.SVG as R



-- | This data declaration is simply used as a token to
-- distinguish this rendering engine.
--
-- The options for an SVG rendering are from the data type
--
-- @
--data Options SVG R2 = SVGOptions { size :: (Double, Double)
--                                   -- ^ The requested size.
--                                 }
-- @
--
-- The result of an SVG rendering is
--
-- @
--data Result SVG R2 = 'B.Builder'
-- @
data SVG = SVG
    deriving (Show, Typeable)


instance Monoid (Render SVG R2) where
  mempty  = R $ mempty
  (R r1) `mappend` (R r2) = R (r1 <> r2)


instance Backend SVG R2 where
  data Render  SVG R2 = R (R.Render)
  type Result  SVG R2 = B.Builder
  data Options SVG R2 = SVGOptions { size :: (Double, Double)
                                     -- ^ The requested size.
                                   }

-- TODO: Clip
  withStyle _ s t (R r) = R $
    let handle :: AttributeClass a => (a -> R.Attribute) -> Maybe R.Attribute
        handle f = f `fmap` getAttr s
    in flip (R.renderAttrs t) r $ catMaybes
         [ handle R.AFillRule
         , handle R.AFont
         , handle R.AFontSize
         , handle R.AFontSlant
         , handle R.AFontWeight
         , handle R.ALineColor
         , handle R.AFillColor
         , handle R.AOpacity
         , handle R.ALineWidth
         , handle R.ALineCap
         , handle R.ALineJoin
         , handle R.ADashing
         ]

  doRender _ (SVGOptions (w,h)) (R r) =
    R.unR (R.svgHeader w h <> r <> R.svgFooter) mempty

  adjustDia c opts d = adjustDia2D size c opts (d # reflectY
                                                  # fcA transparent
                                               )

instance Renderable Ellipse SVG where
  render _ = R . R.renderEllipse

instance Renderable (Segment R2) SVG where
  render c = render c . flip Trail False . (:[])

instance Renderable (Trail R2) SVG where
  render c t = render c $ Path [(P (0,0), t)]

instance Renderable (Path R2) SVG where
  render _ = R . R.renderPath

instance Renderable Text SVG where
  render _ = R . R.renderText

-- TODO: instance Renderable Image SVG where
