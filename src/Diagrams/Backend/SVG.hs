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
import Data.Monoid
import Data.Typeable

-- from colour
import Data.Colour (transparent)

-- from diagrams-lib
import Diagrams.Prelude hiding ((<>))
import Diagrams.TwoD.Adjust (adjustDia2D)
import Diagrams.TwoD.Text

-- from blaze-svg
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S

-- from this package
import qualified Graphics.Rendering.SVG as R

data SVG = SVG
    deriving (Show, Typeable)


instance Monoid (Render SVG R2) where
  mempty  = R $ mempty
  (R r1) `mappend` (R r2_) = R (r1 <> r2_)


instance Backend SVG R2 where
  data Render  SVG R2 = R S.Svg
  type Result  SVG R2 = S.Svg
  data Options SVG R2 = SVGOptions
                        { fileName     :: String       -- ^ the name of the file you want generated
                        , size :: SizeSpec2D           -- ^ The requested size.
                        }

  withStyle _ _ _ d = d

  doRender _ (SVGOptions _ sz) (R r) =
    let (w,h) = case sz of
                  Width w'   -> (w',w')
                  Height h'  -> (h',h')
                  Dims w' h' -> (w',h')
                  Absolute   -> (100,100)
    in R.svgHeader w h $ r

  adjustDia c opts d = adjustDia2D size setSvgSize c opts (d # reflectY
                                                             # fcA transparent
                                                          )
    where setSvgSize sz o = o { size = sz }

instance Renderable (Segment R2) SVG where
  render c = render c . flip Trail False . (:[])

instance Renderable (Trail R2) SVG where
  render c t = render c $ Path [(p2 (0,0), t)]

instance Renderable (Path R2) SVG where
  render _ = R . R.renderPath

instance Renderable Text SVG where
  render _ = R . R.renderText

-- TODO: instance Renderable Image SVG where
