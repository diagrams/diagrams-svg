{-| Attributes that are specific to the SVG backend. The intent
    of this module is to allow adding attributes like class,
    id, and data attributes to an SVG. For those embedding 
    the resulting SVG into a webpage, this allows some 
    interactivity with javascript and stylesheets.
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Diagrams.Backend.SVG.Attributes where

import Data.Text (Text)
import Data.Semigroup
import Diagrams.Core.Style (AttributeClass,HasStyle,applyAttr)
import Data.Typeable (Typeable)

data SvgId = SvgId {getSvgId :: Text}
  deriving (Typeable)
instance Semigroup SvgId where 
  _ <> a = a
instance AttributeClass SvgId

svgId :: HasStyle a => Text -> a -> a
svgId val a = applyAttr (SvgId val) a
