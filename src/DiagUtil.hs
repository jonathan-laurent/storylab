--------------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module DiagUtil
  ( module DiagUtil,
    module Diagrams.Prelude,
    module Diagrams.Backend.SVG,
    module Diagrams.Backend.SVG.CmdLine
  ) where

import Diagrams.Prelude hiding (Trace)
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine

--------------------------------------------------------------------------------

type Diag = Diagram B

data Grid = Grid { content :: [[Diag]] ,  -- List of lines
                   xlegend :: [Diag]   ,
                   ylegend :: [Diag]   }

data GridOptions = GridOptions
  { showXLegend :: Bool ,
    showYLegend :: Bool }

instance Default GridOptions where
  def = GridOptions
    { showXLegend = True ,
      showYLegend = True }


addCoords ls = map mkPoint xynum
  where
    xnum   = zip [0..] (reverse ls)
    xynum  = concat [ zip (zip (repeat i) [0..]) l | (i, l) <- xnum]
    mkPoint ((x, y), e) =
      (p2 (fromInteger y, fromInteger x) :: P2 Double, e)


gridDiag :: GridOptions -> Grid -> Diag

gridDiag opts g =

  (if showXLegend opts then xleg else mempty) <>
  
  (if showYLegend opts then yleg else mempty) <>
  
  position (addCoords (content g)) <> lines
  
  where

    sup = 0.5
    
    xsize  = fromIntegral $ length (content g !! 0) - 1 :: Double
    
    ysize  = fromIntegral $ length (content g) - 1      :: Double

    lineAlong v p len =
      (p .-^ sup *^ v) ~~ (p .+^ (len + sup) *^ v)

    hlines = mconcat $ 
      [ lineAlong unitX (0.0 ^& y) xsize | y <- [0.0 .. ysize] ]
       
    vlines = mconcat $
      [ lineAlong unitY (x ^& 0.0) ysize | x <- [0.0 .. xsize] ]

    pointsAlong v p len = [ p .+^ t *^ v | t <- [0.0 .. len] ]
    
    xleg = atPoints (pointsAlong unitX (0 ^& (ysize + 1)) xsize) $ xlegend g
      
    yleg = atPoints (pointsAlong unitY (-1 ^& 0) ysize) $ reverse (ylegend g)
    
    lines = (vlines <> hlines)  # lwG 0.02 # lc grey -- # dashingG [0.05, 0.02] 0

gridDiag' :: Grid -> Diag
gridDiag' = gridDiag def


renderText a s =
  alignedText a 0.5 s
  # fontSizeG 0.4
  # font "CMU Typewriter Text"

xtext = renderText 0.5

ytext = renderText 1

circleBullet = circle 0.2

squareBullet = square 0.4

dotBullet    = circle 0.1 # fc black

--------------------------------------------------------------------------------
