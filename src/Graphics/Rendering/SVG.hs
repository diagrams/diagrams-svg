{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ViewPatterns, OverloadedStrings #-}
module Graphics.Rendering.SVG
    ( svgHeader
    , renderPath
    , renderText
    ) where

-- from base
import Data.List (intersperse)
import Data.Monoid ((<>))

-- from diagrams-lib
import Diagrams.Prelude hiding (Render, Attribute, close, e, (<>))
import Diagrams.TwoD.Text
import Diagrams.TwoD.Path

import Text.Blaze.Svg11 ((!), mkPath, m, cr, hr, vr, lr, z)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

svgHeader :: Double -> Double -> S.Svg -> S.Svg
svgHeader w h_ s =  S.docTypeSvg
  ! A.version "1.1"
  ! A.width   (S.toValue w)
  ! A.height  (S.toValue h_)
  ! A.viewbox (S.toValue $ concat . intersperse " " $ map show [0, 0, round w, round h_]) $
    topLevelGroup $ s

topLevelGroup :: S.Svg -> S.Svg
topLevelGroup = S.g
  ! A.fill "rgb(0,0,0)"
  ! A.fillOpacity "0"
  ! A.fillRule "nonzero"
  ! A.fontFamily "Sans"
  ! A.fontSize "1"
  ! A.fontStyle "normal"
  ! A.opacity "1"
  ! A.stroke "rgb(0,0,0)"
  ! A.strokeOpacity "1"
  ! A.strokeWidth "0.5"
  ! A.strokeLinecap "butt"
  ! A.strokeLinejoin "miter"
  ! A.textAnchor "middle"

renderPath :: Path R2 -> S.Svg
renderPath (Path trs)  = S.path ! A.d makePath
 where
  makePath = mkPath $ mapM_ renderTrail trs

renderTrail (unp2 -> (x,y), Trail segs closed) = do
  m x y
  mapM_ renderSeg segs
  if closed then z else return ()

renderSeg (Linear (unr2 -> (x,0))) = hr x
renderSeg (Linear (unr2 -> (0,y))) = vr y
renderSeg (Linear (unr2 -> (x,y))) = lr x y
renderSeg (Cubic  (unr2 -> (x0,y0)) (unr2 -> (x1,y1)) (unr2 -> (x2,y2))) = cr x0 y0 x1 y1 x2 y2


renderText :: Text -> S.Svg
renderText _ = mempty

applyTransform :: T2 -> S.Svg -> S.Svg
applyTransform t s = S.g ! A.transform (matrix t) $ s

matrix t = case (unr2 (apply t unitX), unr2 (apply t unitY), unr2 (transl t)) of
  ((1,0), (0,1), (0,0)) -> mempty

  ((1,0), (0,1), (e,0)) ->    "translate("
                           <> S.toValue e
                           <> close

  ((1,0), (0,1), (e,f)) ->    "translate("
                           <> S.toValue e <> " "
                           <> S.toValue f
                           <> close

  ((a,0), (0,d), (0,0)) ->    "scale("
                           <> S.toValue a <> " "
                           <> S.toValue d
                           <> close

  ((a,b), (c,d), (e,f)) ->    "matrix("
                           <> S.toValue a <>  " "
                           <> S.toValue b <>  " "
                           <> S.toValue c <>  " "
                           <> S.toValue d <>  " "
                           <> S.toValue e <>  " "
                           <> S.toValue f
                           <> close
 where close = ")"