{-| Attributes that are specific to the SVG backend. The intent
    of this module is to allow adding attributes like class,
    id, and data attributes to an SVG. For those embedding 
    the resulting SVG into a webpage, this allows some 
    interactivity with javascript and stylesheets.
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Diagrams.Backend.SVG.Attributes where

import Data.Semigroup
import Diagrams.Core.Style (AttributeClass,HasStyle,applyAttr)
import Data.Typeable (Typeable)

data SvgId = SvgId {getSvgId :: String}
  deriving (Typeable)
instance Semigroup SvgId where 
  _ <> a = a
instance AttributeClass SvgId

svgId :: HasStyle a => String -> a -> a
svgId val a = applyAttr (SvgId val) a


data SvgClass = SvgClass {getSvgClass :: String}
  deriving (Typeable)
instance Semigroup SvgClass where 
  _ <> a = a
instance AttributeClass SvgClass

svgClass :: HasStyle a => String -> a -> a
svgClass val a = applyAttr (SvgClass val) a

