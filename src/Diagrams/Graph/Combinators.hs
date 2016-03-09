{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Diagrams.Graph.Combinators where

import           Diagrams.Prelude              hiding ((:&))
import           Diagrams.Backend.SVG

-- bar :: Double -> (res -> Double) -> res -> Diagram B
-- bar w getHeight res = rect w h
--   where h = getHeight res

connectHorizontalInterval :: Double -> [(Diagram B,Double)] -> Diagram B
connectHorizontalInterval intervalWidth pairs = let 
  positionedDiagrams = flip map (zip [0,1..] pairs) $ \(i :: Int, (diagram,y)) -> 
    (P $ V2 (fromIntegral i * intervalWidth) y, diagram)
  in position positionedDiagrams <> fromVertices (map fst positionedDiagrams)

besideText :: V2 Double -> String -> Diagram B -> Diagram B
besideText v' text diagram = 
  beside v diagram (alignedText textX textY text)
  where v@(V2 x y) = normalize v'
        textX = (-0.5) * x + 0.5
        textY = (-0.5) * y + 0.5

createYAxis :: Enum res => Double -> res -> res -> Int -> (res -> Double) -> (res -> String) -> (Diagram B -> Diagram B) -> Diagram B
createYAxis segmentWidth initial step totalSteps toDouble toLabel editLine = resVals
  where 
  resVals = alignBR $ vsep (toDouble step) $ map 
    (\res -> alignedText 1.0 0.5 (toLabel res) # fontSizeL 2 ||| horizontalLine segmentWidth # editLine # alignL) 
    $ reverse $ take totalSteps $ enumFromThen initial step

createXAxis :: Double -> Double -> [res] -> (res -> String) -> (Diagram B -> Diagram B) -> Diagram B
createXAxis segmentHeight intervalWidth vals toLabel editLine = resVals
  where 
  resVals = alignTL $ hsep intervalWidth $ flip map vals $ \res -> 
    besideText unit_Y (toLabel res) (verticalLine segmentHeight # editLine # alignB)

horizontalLine :: Double -> Diagram B
horizontalLine w = fromOffsets [scaleX w unitX]

verticalLine :: Double -> Diagram B
verticalLine h = fromOffsets [scaleY h unitY]

lineOnBottom :: Diagram B -> Diagram B
lineOnBottom d = alignB d === horizontalLine (width d) # centerX



