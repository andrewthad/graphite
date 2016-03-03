{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module StackedBarGraph where

import           Control.Monad
import           Data.Colour.Palette.BrewerSet (ColorCat (Set1), brewerSet)
import           Data.Foldable                 (foldl')
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Vinyl.Core
import           Diagrams.Backend.SVG
import           Diagrams.Prelude              hiding ((:&))
import           Lucid.Base                    (renderText)
import           Prelude
import           Safe.Foldable
import           Text.Blaze.Html               (Html, preEscapedToHtml)

-- Things we need:
-- Rec Grande rs
-- Rec [] rs
-- (Rec Identity rs -> Double)
-- newtype Grande a = Grande (a -> Diagram B -> Diagram B)

newtype Graphite rs a = Graphite ((Rec Identity rs -> a) -> Diagram B)

modifyGraphite :: ([Diagram B] -> Diagram B) -> [r] -> Graphite rs a -> Graphite (r ': rs) a
modifyGraphite combine vals (Graphite g) = Graphite $ \getData ->
  combine $ flip map vals $ \val -> g (curryRecFunc getData (Identity val))

modifyGraphiteWith :: ([Diagram B] -> Diagram B) -> [(r, Diagram B -> Diagram B)] -> Graphite rs a -> Graphite (r ': rs) a
modifyGraphiteWith combine vals (Graphite g) = Graphite $ \getData ->
  combine $ flip map vals $ \(val,edit) -> g (curryRecFunc getData (Identity val)) & edit

bar :: (a -> Double) -> Double -> (Diagram B -> Diagram B) -> Graphite '[] a
bar f width mods = Graphite (\g -> rect width (f (g RNil)) & mods)

lineGraph :: (a -> Double) -> Double -> [r] -> (Diagram B -> Diagram B) -> Graphite '[r] a
lineGraph f spacing vals mods = Graphite $ \g -> id
  $ fromVertices
  $ map (\(x,y) -> P $ V2 x y)
  $ zip (enumFromThen 0 spacing)
  $ flip map vals $ \val -> f (g (Identity val :& RNil))

-- The numeric argument refers to the amount of space
-- between each bar.
grouped :: Double -> [r] -> Graphite rs a -> Graphite (r ': rs) a
grouped innerSpacing vals (Graphite g) = Graphite $ \getData ->
  hsep innerSpacing $ flip map vals $ \val -> alignB $ g (curryRecFunc getData (Identity val))

stacked :: [r] -> Graphite rs a -> Graphite (r ': rs) a
stacked = modifyGraphite (alignB . vcat)

-- stackedB :: [r] -> Graphite (r ': rs) a -> Graphite (r ': rs) a

-- stacked :: [r] -> Graphite rs a -> Graphite (r ': rs) a
-- stacked vals (Graphite g) = Graphite $ \getData ->
--   alignB $ vcat $ flip map vals $ \val -> g (curryRecFunc getData (Identity val))

stackedWith :: [(r,Diagram B -> Diagram B)] -> Graphite rs a -> Graphite (r ': rs) a
stackedWith vals (Graphite g) = Graphite $ \getData ->
  alignB $ vcat $ flip map vals $ \(val,edit) -> g (curryRecFunc getData (Identity val)) & edit

-- We don't actually want to use hcat. We need to space everything
-- by a constant amount, and there should be some extra space on the left and right.
intervals :: Double -> [r] -> Graphite rs a -> Graphite (r ': rs) a
intervals spacing vals (Graphite g) = Graphite $ \getData ->
  atPoints (intervalPoints spacing (length vals))
  $ surroundWith mempty $ flip map vals $ \val -> alignB $ g (curryRecFunc getData (Identity val))

intervalsWith :: Double -> [(r,Diagram B -> Diagram B)] -> Graphite rs a -> Graphite (r ': rs) a
intervalsWith spacing = modifyGraphiteWith
  (\diagrams -> atPoints (intervalPoints spacing (length diagrams)) (surroundWith mempty diagrams))

intervalsWithAxis :: Double -> (r -> String) -> [(r,Diagram B -> Diagram B)] -> Graphite rs a -> Graphite (r ': rs) a
intervalsWithAxis spacing toLabel vals g = modifyGraphiteWith (\diagrams ->
  atPoints (intervalPoints spacing (length diagrams)) (surroundWith mempty diagrams)
  ===
  axisX spacing (map (toLabel . fst) vals)
  ) vals g

build :: Graphite rs a -> (Rec Identity rs -> a) -> Diagram B
build (Graphite f) g = f g

axisX :: Double -> [String] -> Diagram B
axisX interval strs =
  fromOffsets [scaleX (interval * fromIntegral (length strs)) unitX]
  ===
  atPoints (intervalPoints interval (length strs))
    (surroundWith mempty $ map (\str -> alignedText 0.5 1.0 str # fontSizeL 2) strs)

axisY :: Double -> [String] -> Diagram B
axisY interval strs =
  fromOffsets [scaleY (interval * fromIntegral (length strs)) unitY]
  |||
  atPoints (intervalPointsY interval (length strs))
    ((mempty:) $ map (\str -> alignedText 1.0 0.5 str # fontSizeL 2) strs)

intervalPointsY :: Double -> Int -> [Point V2 Double]
intervalPointsY spacing n =
  (fromOffsets $ (scaleY spacing unitY :) $
    (replicate n (scaleY spacing unitY)))

intervalPoints :: Double -> Int -> [Point V2 Double]
intervalPoints spacing n =
  (fromOffsets $ surroundWith (scaleX (spacing / 2) unitX) $
    (replicate n (scaleX spacing unitX)))

increasingX :: Double -> [Point V2 Double]
increasingX inc = go 0
  where
  go n = scaleX n unitX : go (n + inc)

surroundWith :: a -> [a] -> [a]
surroundWith a as = [a] ++ as ++ [a]

-- Move this into vinyl-plus
-- Can we show that
-- (Rec f (a ': as) -> b) -> f a -> Rec f as -> b
-- its like curry for records
curryRecFunc :: (Rec f (a ': as) -> b) -> f a -> Rec f as -> b
curryRecFunc f r rs = f (r :& rs)

renderStackedBarGraph :: StackedBarGraphSettings -> [(String,[Int])] -> Html
renderStackedBarGraph bgs pairs = preEscapedToHtml . renderText
  $ renderDia SVG (SVGOptions (mkWidth 400) Nothing "bargraph")
  $ buildStackedBarGraph bgs pairs

buildStackedBarGraph :: StackedBarGraphSettings -> [(String,[Int])] -> Diagram B
buildStackedBarGraph bgs pairs =
  let maxY = fromMaybe 100 $ maximumMay $ map (sum . snd) pairs
      (yInterval, intervalCount) = chooseOptimalInterval maxY
      yIntervalHeight = bgsHeight bgs / (fromIntegral intervalCount + 0.3)
      scaleUnit i = fromIntegral i * yIntervalHeight / fromIntegral yInterval
  in buildYAxis bgs yIntervalHeight intervalCount yInterval # alignBR
     <> buildStackedContent bgs scaleUnit pairs # alignL

chooseOptimalInterval :: Int -> (Int,Int)
chooseOptimalInterval maxY =
  fromMaybe (1000000,8)
  $ fmap (\(interval',ct,_) -> (interval',ct))
  $ minimumByMay (\(_,_,a) (_,_,b) -> compare a b)
  $ do
    x <- acceptableIntervals
    y <- acceptableCounts
    let diff' = x * y - maxY
    guard $ diff' >= 0
    return (x,y,diff')
  where acceptableIntervals = [1,2,3,4,5,10,20,50,100,200,500,1000,10000,100000]
        acceptableCounts = [4,5,6,7]


buildYAxis :: StackedBarGraphSettings -> Double -> Int -> Int -> Diagram B
buildYAxis bgs yIntervalHeight yIntervalCount i =
  let nums = reverse $ take yIntervalCount $ map (*i) [1..]
      -- shiftCenter x = translate (r2 (0, x))
      ticks = foldl' (===) mempty $ flip map nums $ \n ->
        hsep 3 [ alignedText 1 0.5 (show n) # fontSizeL (bgsAxisFontSize bgs)
               , hrule (bgsTickSize bgs)
               ] <> rect (bgsIntervalSpace bgs) yIntervalHeight # lw none # alignTR
  in hcat $ map alignB $
     [ ticks
     , vrule (bgsHeight bgs)
     ]

buildStackedContent :: StackedBarGraphSettings -> (Int -> Double) -> [(String,[Int])] -> Diagram B
buildStackedContent bgs scaleUnit pairs =
  let pairCount = length pairs
      pieceWidth = (bgsWidth bgs) / fromIntegral pairCount
      barWidth = pieceWidth * 0.9
      infiniteColors = cycle $ bgsColors bgs
      mkPiece label vals = mempty
        === vcat (reverse $ flip map (zip vals infiniteColors) $ \(val,color) ->
                  rect barWidth (scaleUnit val) # fc color # lw none) # alignB
        === hrule pieceWidth
        === alignedText 0.5 1.0 label # fontSizeL (bgsAxisFontSize bgs)
  in hcat (map (uncurry mkPiece) pairs) === rect (bgsAxisFontSize bgs) (bgsAxisFontSize bgs) # lw none

data StackedBarGraphSettings = StackedBarGraphSettings
  { bgsColors        :: [Colour Double]
  , bgsTickSize      :: Double
  , bgsAxisFontSize  :: Double
  , bgsHeight        :: Double
  , bgsWidth         :: Double
  , bgsIntervalSpace :: Double
  }

defStackedBarGraphSettings :: StackedBarGraphSettings
defStackedBarGraphSettings = StackedBarGraphSettings
  (brewerSet Set1 8)
  5 16 200 400 40

