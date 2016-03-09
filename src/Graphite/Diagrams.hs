{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphite.Diagrams where

import           Control.Monad
import           Data.Colour.Palette.BrewerSet (ColorCat (Set1), brewerSet)
import           Data.Foldable                 (foldl')
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Vinyl.Core
import           Data.Vinyl.Lens               hiding ((:~:))
import           Data.Vinyl.TypeLevel
import           Diagrams.Backend.SVG
import           Diagrams.Prelude              hiding ((:&))
import           Lucid.Base                    (renderText)
import           Prelude
import           Safe.Foldable
import           Text.Blaze.Html               (Html, preEscapedToHtml)
import           Data.Type.Equality
import           Data.Proxy
import           Graphite

bar :: Double -> (res -> Double) -> Graphite res (Diagram B) values '[]
bar width resToHeight = Graphite $ \vals getRes RNil -> 
  rect width (resToHeight (getRes RNil))

singlePoint :: Double -> (res -> Double) -> Graphite res (Diagram B, Double) values '[]
singlePoint radius resToHeight = Graphite $ \vals getRes RNil ->
  (circle radius, resToHeight (getRes RNil))

-- labelTop :: Graphite res (Diagram B) values list -> Graphite res (Diagram B) values list
-- labelTop =

barTopLabel :: Double -> (res -> Double) -> (Rec Identity values -> String)
  -> Graphite res (Diagram B) values '[]
barTopLabel width resToHeight getLabel = Graphite $ \vals getRes RNil -> alignB $ 
  alignedText 0.5 0 (getLabel vals) # fontSizeL 2
  === 
  rect width (resToHeight (getRes RNil))

stacked :: Graphite res (Diagram B) (v ': values) lists -> Graphite res (Diagram B) values (v ': lists)
stacked = mergeGraphite vcat

labelTop :: (Rec Identity values -> String) -> Graphite res (Diagram B) values lists -> Graphite res (Diagram B) values lists
labelTop getLabel = modifyFigure $ \vals fig ->
  alignedText 0.5 0 (getLabel vals) # fontSizeL 2
  === 
  fig

labelBottom :: (Rec Identity values -> String) -> Graphite res (Diagram B) values lists -> Graphite res (Diagram B) values lists
labelBottom getLabel = modifyFigure $ \vals fig ->
  fig
  === 
  alignedText 0.5 1.0 (getLabel vals) # fontSizeL 2

liftFigureChange :: (fig1 -> fig2) -> Graphite res fig1 values lists -> Graphite res fig2 values lists
liftFigureChange f = modifyFigure (\_ a -> f a)

sideBySide :: 
     Double 
  -> Graphite res (Diagram B) (v ': values) lists
  -> Graphite res (Diagram B) values (v ': lists)
sideBySide spacing = mergeGraphite (hsep spacing . map alignB)

overlapped :: 
     Graphite res (Diagram B) (v ': values) lists
  -> Graphite res (Diagram B) values (v ': lists)
overlapped = mergeGraphite mconcat

connected ::
     Double
  -> Graphite res (Diagram B, Double) (v ': values) lists
  -> Graphite res (Diagram B) values (v ': lists)
connected intervalWidth = mergeGraphite $ \pairs -> let 
  positionedDiagrams = flip map (zip [0,1..] pairs) $ \(i :: Int, (diagram,y)) -> 
    (P $ V2 (fromIntegral i * intervalWidth) y, diagram)
  in position positionedDiagrams <> fromVertices (map fst positionedDiagrams)

-- sideBySideTopLabels :: 
--      Double -- set this to something around 1.1
--   -> (v -> String)
--   -> Graphite res (Diagram B) (v ': values) lists
--   -> Graphite res (Diagram B) values (v ': lists)
-- sideBySideTopLabels spacing valToString = 

sideBySideAxis :: 
     Double -- set this to something around 1.1
  -> (v -> String)
  -> Graphite res (Diagram B) (v ': values) lists
  -> Graphite res (Diagram B) values (v ': lists)
sideBySideAxis spacing valToString = 
  mergeGraphite2 $ \pairs -> hcat $ flip map pairs $ \(v,diagram) ->
    let wideDiagram = padX spacing diagram in
    wideDiagram # alignB
    ===
    fromOffsets [scaleX (width wideDiagram) unitX] # centerX
    === 
    alignedText 0.5 1.0 (valToString v) # fontSizeL 2

horizontalLine :: Double -> Diagram B
horizontalLine w = fromOffsets [scaleX w unitX]

verticalLine :: Double -> Diagram B
verticalLine h = fromOffsets [scaleY h unitY]

lineOnBottom :: Diagram B -> Diagram B
lineOnBottom d = alignB d === horizontalLine (width d) # centerX

lineOnRight :: Diagram B -> Diagram B
lineOnRight d = alignBR d ||| verticalLine (height d)

createYAxis :: Enum res => Double -> res -> res -> Int -> (res -> Double) -> (res -> String) -> (Diagram B -> Diagram B) -> Diagram B
createYAxis segmentWidth initial step totalSteps toDouble toLabel editLine = resVals
  where 
  resVals = alignBR $ vsep (toDouble step) $ map 
    (\res -> alignedText 1.0 0.5 (toLabel res) # fontSizeL 2 ||| horizontalLine segmentWidth # editLine # alignL) 
    $ reverse $ take totalSteps $ enumFromThen initial step

createXAxis :: Enum res => Double -> Double -> [res] -> (res -> String) -> (Diagram B -> Diagram B) -> Diagram B
createXAxis segmentHeight intervalWidth vals toLabel editLine = resVals
  where 
  resVals = alignTL $ hsep intervalWidth $ map 
    (\res -> verticalLine segmentHeight # editLine # alignB === alignedText 0.5 1.0 (toLabel res) # fontSizeL 2)
    $ vals


