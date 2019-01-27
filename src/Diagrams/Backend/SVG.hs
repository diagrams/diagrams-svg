{-# LANGUAGE CPP                        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NondecreasingIndentation   #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
  -- UndecidableInstances needed for ghc < 707

----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.SVG
-- Copyright   :  (c) 2011-2015 diagrams-svg team (see LICENSE)
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
-- > renderDia :: b -> Options b v n -> QDiagram b v n m -> Result b v n
--
-- (omitting a few type class constraints).  @b@ represents the
-- backend type, @v@ the vector space, @n@ the numerical field, and @m@ the
-- type of monoidal query annotations on the diagram.  'Options' and 'Result'
-- are associated data and type families, respectively, which yield the
-- type of option records and rendering results specific to any
-- particular backend.  For @b ~ SVG@, @v ~ V2@, we have
--
-- >data    Options SVG V2 n = SVGOptions
-- >    { _size            :: SizeSpec V2 n   -- ^ The requested size.
-- >    , _svgDefinitions  :: Maybe Element
-- >                          -- ^ Custom definitions that will be added to the @defs@
-- >                          --   section of the output.
-- >    , _idPrefix        :: T.Text
-- >    , _svgAttributes   :: [Attribute]
-- >                          -- ^ Attriubtes to apply to the entire svg element.
-- >    , _generateDoctype :: Bool
-- >    }
--
-- @
-- data family Render SVG V2 n = R 'SvgRenderM n'
-- @
--
-- @
-- type family Result SVG V2 n = 'Element'
-- @
--
-- So the type of 'renderDia' resolves to
--
-- @
-- renderDia :: SVG -> Options SVG V2 n -> QDiagram SVG V2 n m -> 'Graphics.Rendering.SVG.Element'
-- @
--
-- which you could call like @renderDia SVG (SVGOptions (mkWidth 250)
-- Nothing "" [] True) myDiagram@ (if you have the 'OverloadedStrings' extension
-- enabled; otherwise you can use 'Text.pack ""').  (In some
-- situations GHC may not be able to infer the type @m@, in which case
-- you can use a type annotation to specify it; it may be useful to
-- simply use the type synonym @Diagram SVG = QDiagram SVG V2 Double
-- Any@.) This returns an 'Graphics.Svg.Core.Element' value, which
-- you can, /e.g./ render to a 'ByteString' using 'Graphics.Svg.Core.renderBS'
-- from the 'svg-builder' package.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.SVG
  ( SVG(..) -- rendering token
  , B
    -- for rendering options specific to SVG
  , Options(..), sizeSpec, svgDefinitions, idPrefix, svgAttributes, generateDoctype

  , renderSVG
  , renderSVG'
  , loadImageSVG
  ) where

import qualified Data.Foldable as F
-- from JuicyPixels
import           Codec.Picture hiding (Traversal)
import           Codec.Picture.Types      (dynamicMap)

#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable            as F (foldMap)
#endif
import qualified Data.Text                as T
-- import           Data.Text.Lazy.IO        as LT
-- import           Data.Tree
import           System.FilePath

-- from base
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import           Data.Typeable

-- from hashable
import           Data.Hashable            (Hashable (), hashWithSalt)

-- from bytestring
import qualified Data.ByteString          as SBS
import qualified Data.ByteString.Lazy     as BS

-- from lens
import           Control.Lens             hiding (transform, ( # ))

-- from diagrams-lib
import           Diagrams.Types               hiding (local)
import           Diagrams.TwoD.Text
import           Diagrams.TwoD.Image
import Geometry
import Diagrams.Prelude hiding (local)
import Diagrams.Backend.Compile

-- from svg-builder
-- import           Graphics.Svg             hiding ((<>))
import           Graphics.Svg             (Element, Attribute, g_, a_, renderBS, (<<-))
import qualified Graphics.Svg             as S

-- from this package
-- import           Graphics.Rendering.SVG   (SVGFloat)
import qualified Graphics.Rendering.SVG   as R

-- | @SVG@ is simply a token used to identify this rendering backend
--   (to aid type inference).
data SVG = SVG
  deriving (Show, Typeable)

type B = SVG

type instance V SVG = V2
type instance N SVG = Double

data Environment = Environment
  { _attributes :: Attributes
  , __pre :: T.Text
  }

makeLenses ''Environment

data SvgRenderState = SvgRenderState
  { _clipPathId :: Int
  , _fillGradId :: Int
  , _lineGradId :: Int
  }

makeLenses ''SvgRenderState

initialEnvironment :: T.Text -> Environment
-- initialEnvironment = Environment (mempty # recommendFillColor transparent)
initialEnvironment = Environment mempty

-- Fill gradients ids are even, line gradient ids are odd.
initialSvgRenderState :: SvgRenderState
initialSvgRenderState = SvgRenderState 0 0 1

-- | Monad to keep track of environment and state when rendering an SVG.
type SvgRenderM = ReaderT Environment (State SvgRenderState) Element

runRenderM :: T.Text -> SvgRenderM -> Element
runRenderM o s = flip evalState initialSvgRenderState
               $ runReaderT  s (initialEnvironment o)

newtype R = R SvgRenderM

instance Semigroup R where
  R ra <> R rb = R $ do
    svg1 <- ra
    svg2 <- rb
    return (svg1 `mappend` svg2)

instance Monoid R where
  mempty  = R $ return mempty
  mappend = (<>)

-- | Create a new texture defs svg element using the style and the current
--   id number, then increment the gradient id number.
fillTextureDefs :: Attributes -> SvgRenderM
fillTextureDefs s = do
  ident <- use fillGradId
  fillGradId += 2 -- always even
  return $ R.renderFillTextureDefs ident s

lineTextureDefs :: Attributes -> SvgRenderM
lineTextureDefs s = do
  ident <- use lineGradId
  lineGradId += 2 -- always odd
  return $ R.renderLineTextureDefs ident s

-- instance Backend SVG where
--   type Result  SVG = Element
--   data Options SVG = PGFOptions
--     { _svgDefinitions  :: Maybe Element
--                           -- ^ Custom definitions that will be added to the @defs@
--                           --   section of the output.
--     , _svgSizeSpec :: SizeSpec V2 Int -- ^ The requested size.
--     , _idPrefix        :: T.Text
--     , _svgAttributes   :: [Attribute]
--                           -- ^ Attriubtes to apply to the entire svg element.
--     , _generateDoctype :: Bool
--     -- , _readable    :: Bool            -- ^ Indented lines for @.tex@ output.
--     }

--   renderDiaT opts dia = (b, t2) where
--     (sz, t2, dia') = adjustSize2D (opts^.sizeSpec) dia
--     b = P.renderWith (opts^.surface) (opts^.readable) (opts^.standalone) sz r
--     r = toRender t2 dia'

--   renderRTree _ opts rt = runRenderM (opts ^.idPrefix) svgOutput
--     where
--       svgOutput = do
--         let R r    = rtree (splitTextureFills rt)
--             V2 w h = specToSize 100 (opts^.sizeSpec)
--         svg <- r
--         return $ R.svgHeader w h (opts^.svgDefinitions)
--                                  (opts^.svgAttributes)
--                                  (opts^.generateDoctype) svg

default2DAttrs :: Diagram V2 -> Diagram V2
default2DAttrs
  = lineWidth medium
  . lineTexture black
  . fillTexture transparent

instance Backend SVG where
  -- newtype Render  SVG V2 n = R (SvgRenderM n)
  type    Result  SVG = Element
  data    Options SVG = SVGOptions
    { _size            :: SizeSpec V2 Int   -- ^ The requested size.
    , _svgDefinitions  :: Maybe Element
                          -- ^ Custom definitions that will be added to the @defs@
                          --   section of the output.
    , _idPrefix        :: T.Text
    , _svgAttributes   :: [Attribute]
                          -- ^ Attriubtes to apply to the entire svg element.
    , _generateDoctype :: Bool
    }

  backendInfo _ = svgInfo
  renderDiaT opts dia = (sz, t2 <> reflectionY, b) where
    (sz, t2, dia') = adjustSize2D (opts^.sizeSpec) (default2DAttrs dia # reflectY)
    b = runRenderM (opts ^.idPrefix) $ do
      let R r    = toRender t2 dia'
          V2 w h = fromIntegral <$> specToSize 100 (opts^.sizeSpec)
      svg <- r
      return $ R.svgHeader w h (opts^.svgDefinitions)
                               (opts^.svgAttributes)
                               (opts^.generateDoctype) svg

-- | Lens onto the size of the svg options.
svgSizeSpec :: Lens' (Options SVG) (SizeSpec V2 Int)
svgSizeSpec f opts = f (_size opts) <&> \s -> opts { _size = s }

-- | Lens onto the svg definitions of the svg options.
svgDefinitions :: Lens' (Options SVG) (Maybe Element)
svgDefinitions f opts =
  f (_svgDefinitions opts) <&> \ds -> opts { _svgDefinitions = ds }

-- | Lens onto the idPrefix of the svg options. This is the prefix given
--   to clipping paths to distinguish them from other svg files in the
--   same web page.
idPrefix :: Lens' (Options SVG) T.Text
idPrefix f opts = f (_idPrefix opts) <&> \i -> opts { _idPrefix = i }

-- | Lens onto the svgAttributes field of the svg options. This field
--   is provided to supply SVG attributes to the entire diagram.
svgAttributes :: Lens' (Options SVG) [Attribute]
svgAttributes f opts =
  f (_svgAttributes opts) <&> \ds -> opts { _svgAttributes = ds }

-- | Lens onto the generateDoctype field of the svg options. Set
--   to False if you don't want a doctype tag included in the output.
generateDoctype :: Lens' (Options SVG) Bool
generateDoctype f opts =
  f (_generateDoctype opts) <&> \ds -> opts { _generateDoctype = ds }


deriving instance Show (Options SVG)


instance Default (Options SVG) where
  def = SVGOptions
    { _size            = absolute
    , _svgDefinitions  = Nothing
    , _idPrefix        = mempty
    , _svgAttributes   = []
    , _generateDoctype = True
    }


  -- renderRTree :: SVG -> Options SVG V2 n -> RTree SVG V2 n Annotation -> Result SVG V2 n
  -- renderRTree _ opts rt = runRenderM (opts ^.idPrefix) svgOutput
  --   where
  --     svgOutput = do
  --       let R r    = rtree (splitTextureFills rt)
  --           V2 w h = specToSize 100 (opts^.sizeSpec)
  --       svg <- r
  --       return $ R.svgHeader w h (opts^.svgDefinitions)
  --                                (opts^.svgAttributes)
  --                                (opts^.generateDoctype) svg
  -- adjustDia c opts d = ( sz, t <> reflectionY, d' ) where
  --   (sz, t, d') = adjustDia2D sizeSpec c opts (d # reflectY)

-- newtype OptionsParser = PrettyOpt {isPretty :: Bool}

-- prettyOpt :: OP.Parser PrettyOpt
-- prettyOpt = PrettyOpt <$> OP.switch (long "pretty"
--                      <> OP.short 'p'
--                      <> OP.help "Pretty print the SVG output")


instance RenderOutcome SVG (Diagram V2) where
  type MainOpts SVG (Diagram V2) = (FilePath, SizeSpec V2 Int)

  resultParser _ _ = (,) <$> outputParser <*> sizeParser
  renderOutcome _ (path, sz) = saveDiagram' path (mkOptions @ SVG sz)

renderPrimitive
  :: T2 Double -> Attributes -> Prim V2 Double -> Maybe R -- SvgRenderM
renderPrimitive t2 attrs = \case
  Path_ path -> Just $ renderPath attrs (path # transform t2)
  Text_ t    -> Just . R $ renderText t2 attrs t
  Prim _     -> Nothing

renderAnnot :: Annotation V2 Double -> R -> R
renderAnnot = \case
  GroupOpacity_ o -> \(R r) -> R $ g_ [S.Opacity_ <<- S.toText o] <$> r
  Clip_ p         -> \(R r) -> R $ r >>= renderSvgWithClipping (F.toList p)
  HRef_ uri       -> \(R r) -> R $ a_ [S.XlinkHref_ <<- T.pack uri] <$> r
  _                         -> id

renderSvgWithClipping
  :: [Path V2 Double]
  -> Element     -- ^ Input SVG
  -> SvgRenderM  -- ^ Resulting svg

renderSvgWithClipping path svg = do
  Environment _ prefix <- ask
  let renderClips :: [Path V2 Double] -> SvgRenderM
      renderClips []     = return svg
      renderClips (p:ps) = do
        clipPathId += 1
        ident <- use clipPathId
        R.renderClip p prefix ident <$> renderClips ps

  renderClips path

instance BackendBuild SVG where
  saveDiagram' = renderSVG'

  mkOptions sz = def & sizeSpec .~ sz
  sizeSpec     = svgSizeSpec
  showOptions  = show

toRender :: T2 Double -> Diagram V2 -> R
toRender = foldDia renderPrim renderAnnot
  where
    renderPrim t2 attrs prim =
      case renderPrimitive t2 attrs prim of
        Just (R r) -> R $ local (attributes .~ attrs) r
        Nothing    -> error $ "Unknown primitive"

-- paths ---------------------------------------------------------------

attributedRender :: Bool -> Element -> R -- SvgRenderM
attributedRender shouldFill svg = R $ do
  SvgRenderState _idClip idFill idLine <- get
  Environment sty _preT <- ask
  lineGradDefs <- lineTextureDefs sty
  if shouldFill
     then do fillGradDefs <- fillTextureDefs sty
             let gDefs = mappend fillGradDefs lineGradDefs
             return $ gDefs `mappend` g_ (R.renderStyles idFill idLine sty) svg
     else return $ lineGradDefs `mappend` g_ (R.renderStyles idFill idLine sty) svg

renderPath :: Attributes -> Path V2 Double -> R -- SvgRenderM
renderPath attrs =
  foldMap (either (attributedRender False . R.renderLines)
                  (attributedRender True . R.renderLoops))
  . collateTrails

collateTrails :: Path V2 Double -> [Either [Located (Line V2 Double)] [Located (Loop V2 Double)]]
collateTrails = collate . toListOf (each . _LocTrail)

mappingLoc
  :: (SameSpace s a, SameSpace t b)
  => AnIso s t a b
  -> Iso (Located s) (Located t) (Located a) (Located b)
mappingLoc k = withIso k $ \sa bt -> iso (over located sa) (over located bt)

-- group together consecutive as and bs
collate :: [Either a b] -> [Either [a] [b]]
collate = \case
  []           -> []
  Left a : es  -> goL (a:) es
  Right b : es -> goR (b:) es
  where
    goL f = \case
      []           -> [Left (f [])]
      Left a : es  -> goL (f . (a:)) es
      Right b : es -> Left (f []) : goR (b:) es
    goR f = \case
      []           -> [Right (f [])]
      Left a : es  -> Right (f []) : goL (a:) es
      Right b : es -> goR (f . (b:)) es


-- instance SVGFloat n => Renderable (Text n) SVG where
renderText :: T2 Double -> Attributes -> Text Double -> SvgRenderM
renderText tTxt attrs t = do
  let svg = R.renderText tTxt t
  SvgRenderState _idClip idFill idLine <- get
  Environment sty preT <- ask
  -- clippedSvg           <- renderSvgWithClipping preT svg sty

  -- SVG applies the text transform to the gradient before rendering.
  -- This means we need to apply the inverse of the text transform
  -- first, being careful about how we use reflectionY to handle SVG's
  -- coordinates.
  -- let adjustTrans :: Maybe FillTexture -> Maybe FillTexture
  --     adjustTrans = _Just . _FillTexture . committed . _LG . lGradTrans %~
  --       \tGrad -> inv (tTxt <> reflectionY) <> tGrad <> reflectionY

  -- fillGradDefs <- fillTextureDefs (sty & atAttr %~ adjustTrans)
  return $
    -- fillGradDefs `mappend` g_ (R.renderStyles idFill idLine sty) clippedSvg
    g_ (R.renderStyles idFill idLine sty) svg

-- instance SVGFloat n => Renderable (DImage n Embedded) SVG where
--   render _ = R . return . R.renderDImageEmb

-- | Render a diagram as an SVG, writing to the specified output file
--   and using the requested size.
renderSVG :: FilePath -> SizeSpec V2 Int -> Diagram V2 -> IO ()
renderSVG outFile spec = renderSVG' outFile (SVGOptions spec Nothing (mkPrefix outFile) [] True)

-- | Render a diagram as a pretty printed SVG.
-- renderPretty :: FilePath -> SizeSpec V2 Int -> Diagram V2 -> IO ()
-- renderPretty outFile spec = renderPretty' outFile (SVGOptions spec Nothing (mkPrefix outFile)[] True)

-- Create a prefile using the basename of the output file. Only standard
-- letters are considered.
mkPrefix :: FilePath -> T.Text
mkPrefix = T.filter isAlpha . T.pack . takeBaseName

-- | Render a diagram as an SVG, writing to the specified output file
--   and using the backend options. The id prefix is derived from the
--   basename of the output file.
renderSVG' :: FilePath -> Options SVG -> Diagram V2 -> IO ()
renderSVG' outFile opts = BS.writeFile outFile . renderBS . view _3 . renderDiaT opts

-- | Render a diagram as a pretty printed SVG to the specified output
--   file and using the backend options. The id prefix is derived from the
--   basename of the output file.
-- renderPretty' :: FilePath -> Options SVG -> Diagram V2 -> IO ()
-- renderPretty' outFile opts = LT.writeFile outFile . prettyText . renderDia SVG opts

data Img = Img !Char !BS.ByteString deriving Typeable

-- | Load images (JPG/PNG/...) in a SVG specific way.
loadImageSVG :: FilePath -> IO (Diagram V2)
loadImageSVG fp = do
    raw <- SBS.readFile fp
    dyn <- eIO $ decodeImage raw
    let dat = BS.fromChunks [raw]
        w = dynamicMap imageWidth dyn
        h = dynamicMap imageHeight dyn
    let pic t d = return $ dimage (ImageNative (V2 w h) (Img t d))
    if | pngHeader `SBS.isPrefixOf` raw -> pic 'P' dat
       | jpgHeader `SBS.isPrefixOf` raw -> pic 'J' dat
       | otherwise -> case dyn of
           (ImageYCbCr8 _) -> pic 'J' dat
           _               -> pic 'P' =<< eIO (encodeDynamicPng dyn)
  where pngHeader :: SBS.ByteString
        pngHeader = SBS.pack [137, 80, 78, 71, 13, 10, 26, 10]
        jpgHeader :: SBS.ByteString
        jpgHeader = SBS.pack [0xFF, 0xD8]
        eIO :: Either String a -> IO a
        eIO = either fail return

-- instance SVGFloat n => Renderable (DImage n (Native Img)) SVG where
--   render _ di@(DImage (ImageNative (Img t d)) _ _ _) = R $ do
--     mime <- case t of
--           'J' -> return "image/jpeg"
--           'P' -> return "image/png"
--           _   -> fail   "Unknown mime type while rendering image"
--     return $ R.renderDImage di $ R.dataUri mime d

instance Hashable (Options SVG) where
  hashWithSalt s  (SVGOptions sz defs ia sa gd) =
    s  `hashWithSalt`
    sz `hashWithSalt`
    ds `hashWithSalt`
    ia `hashWithSalt`
    sa `hashWithSalt`
    gd
      where ds = fmap renderBS defs
