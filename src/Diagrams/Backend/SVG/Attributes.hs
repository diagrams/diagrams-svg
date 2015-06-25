{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.SVG.Attributes
-- Copyright   :  (c) 2015 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Attributes that are specific to the SVG backend. The intent
-- of this module is to allow adding the attributes class,
-- and id attributes to an SVG. For those embedding
-- the resulting SVG into a webpage, this allows some
-- interactivity with javascript and stylesheets.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.SVG.Attributes (
  -- * Id
    SvgId(..)
  , svgId

  -- * Class
  , SvgClass(..)
  , svgClass

  ) where

import         Diagrams.Core.Style (AttributeClass, HasStyle, applyAttr)
import         Data.Semigroup
import         Data.Typeable       (Typeable)

-----------------------------------------------------------------
--  Id
-----------------------------------------------------------------

-- | The SVG id attribute.
newtype SvgId = SvgId {getSvgId :: String}
  deriving Typeable

instance Semigroup SvgId where
  _ <> a = a
instance AttributeClass SvgId

-- | Set the Id attribute.
svgId :: HasStyle a => String -> a -> a
svgId = applyAttr . SvgId

-----------------------------------------------------------------
--  Class
-----------------------------------------------------------------

-- | The SVG class attribute.
newtype SvgClass = SvgClass {getSvgClass :: String}
  deriving Typeable

instance Semigroup SvgClass where
  _ <> a = a

instance AttributeClass SvgClass

-- | Set the class attribute.
svgClass :: HasStyle a => String -> a -> a
svgClass = applyAttr . SvgClass
