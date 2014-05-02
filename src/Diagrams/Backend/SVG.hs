{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

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
  , Options(..), size, svgDefinitions -- for rendering options specific to SVG

  , renderSVG
  ) where

-- for testing
import           Data.Foldable                (foldMap)
import           Data.Tree

-- from base
import           Control.Monad.State
import           Data.Typeable
import           GHC.Generics                 (Generic)
import           Unsafe.Coerce

-- from hashable
import           Data.Hashable                (Hashable (..))

-- from bytestring
import qualified Data.ByteString.Lazy         as BS

-- from lens
import           Control.Lens                 hiding (transform, ( # ))

-- from diagrams-core
import           Diagrams.Core.Compile
import           Diagrams.Core.Types          (Annotation (..))

-- from diagrams-lib
import           Diagrams.Prelude             hiding (view)
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

-- from this package
import qualified Graphics.Rendering.SVG       as R

-- | @SVG@ is simply a token used to identify this rendering backend
--   (to aid type inference).
data SVG = SVG
    deriving (Show, Typeable)

type B = SVG

data SvgRenderState = SvgRenderState { _clipPathId :: Int
                                     , _fillGradId :: Int
                                     , _lineGradId :: Int }

makeLenses ''SvgRenderState

-- Fill gradients ids are even, line gradient ids are odd.
initialSvgRenderState :: SvgRenderState
initialSvgRenderState = SvgRenderState 0 0 1

-- | Monad to keep track of state when rendering an SVG.
--   Currently just keeps a monotonically increasing counter
--   for assiging a unique clip path ID.
type SvgRenderM = State SvgRenderState S.Svg

instance Monoid (Render SVG R2) where
  mempty  = R $ return mempty
  (R r1) `mappend` (R r2_) =
    R $ do
      svg1 <- r1
      svg2 <- r2_
      return (svg1 `mappend` svg2)

-- Handle clip attributes.
renderSvgWithClipping :: S.Svg             -- ^ Input SVG
                      -> Style v           -- ^ Styles
                      -> SvgRenderM        -- ^ Resulting svg
renderSvgWithClipping svg s =
  case (op Clip <$> getAttr s) of
    Nothing -> return $ svg
    Just paths -> renderClips paths
  where
    renderClips :: [Path R2] -> SvgRenderM
    renderClips [] = return $ svg
    renderClips (p:ps) = do
      clipPathId += 1
      id_ <- use clipPathId
      R.renderClip p id_ <$> renderClips ps

-- | Create a new texture defs svg element using the style and the current
--   id number, then increment the gradient id number.
fillTextureDefs :: Style v -> SvgRenderM
fillTextureDefs s = do
  id_ <- use fillGradId
  fillGradId += 2 -- always even
  return $ R.renderFillTextureDefs id_ s

lineTextureDefs :: Style v -> SvgRenderM
lineTextureDefs s = do
  id_ <- use lineGradId
  lineGradId += 2 -- always odd
  return $ R.renderLineTextureDefs id_ s

instance Backend SVG R2 where
  data Render  SVG R2 = R SvgRenderM
  type Result  SVG R2 = S.Svg
  data Options SVG R2 = SVGOptions
                        { _size :: SizeSpec2D   -- ^ The requested size.
                        , _svgDefinitions :: Maybe S.Svg
                          -- ^ Custom definitions that will be added to the @defs@
                          --   section of the output.
                        }

  renderRTree _ opts rt = evalState svgOutput initialSvgRenderState
    where
      svgOutput = do
        let R r = toRender rt
            (w,h) = sizePair (opts^.size)
        svg <- r
        return $ R.svgHeader w h (opts^.svgDefinitions) $ svg

  adjustDia c opts d = adjustDia2D size c opts (d # reflectY)

toRender :: RTree R2 Annotation -> Render SVG R2
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
      fromRTree (Node (RPrim p) _) = renderPrim p
      fromRTree (Node (RStyle sty) ts)
        = R $ do
            let R r = foldMap fromRTree ts
            svg <- r
            idFill <- use fillGradId
            idLine <- use lineGradId
            clippedSvg <- renderSvgWithClipping svg sty
            lineGradDefs <- lineTextureDefs sty
            fillGradDefs <- fillTextureDefs sty
            let textureDefs = fillGradDefs `mappend` lineGradDefs
            return $ (S.g ! R.renderStyles idFill idLine sty)
                     (textureDefs `mappend` clippedSvg)
      fromRTree (Node _ rs) = foldMap fromRTree rs

getSize :: Options SVG R2 -> SizeSpec2D
getSize (SVGOptions {_size = s}) = s

setSize :: Options SVG R2 -> SizeSpec2D -> Options SVG R2
setSize o s = o {_size = s}

size :: Lens' (Options SVG R2) SizeSpec2D
size = lens getSize setSize

getSVGDefs :: Options SVG R2 -> Maybe S.Svg
getSVGDefs (SVGOptions {_svgDefinitions = d}) = d

setSVGDefs :: Options SVG R2 -> Maybe S.Svg -> Options SVG R2
setSVGDefs o d = o {_svgDefinitions = d}

svgDefinitions :: Lens' (Options SVG R2) (Maybe S.Svg)
svgDefinitions = lens getSVGDefs setSVGDefs

instance Hashable (Options SVG R2) where
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

renderPrim :: Prim R2 -> Render SVG R2
renderPrim (Prim p)
  | q == (typeOf (undefined :: Path R2)) = R . return . R.renderPath $ unsafeCoerce p
  | q == (typeOf (undefined :: Text)) = R . return . R.renderText $ unsafeCoerce p
  | otherwise = error $ "Primitive " ++ show q ++ " no supported by backend."
  where
    q = typeOf p

-- | Render a diagram as an SVG, writing to the specified output file
--   and using the requested size.
renderSVG :: FilePath -> SizeSpec2D -> Diagram R2 -> IO ()
renderSVG outFile sizeSpec
  = BS.writeFile outFile
  . renderSvg
  . renderDia SVG (SVGOptions sizeSpec Nothing)
