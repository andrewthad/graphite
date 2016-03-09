{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module StackedBarGraph where

import           Control.Monad
import           Data.Colour.Palette.BrewerSet (ColorCat (Set1), brewerSet)
import           Data.Foldable                 (foldl')
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Vinyl.Core
import           Data.Vinyl.Lens
import           Diagrams.Backend.SVG
import           Diagrams.Prelude              hiding ((:&))
import           Lucid.Base                    (renderText)
import           Prelude
import           Safe.Foldable
import           Text.Blaze.Html               (Html, preEscapedToHtml)


newtype Graphite b r rs a = Graphite
  { runGraphite
    :: (a -> Double)
    -> (Rec Identity rs -> a)
    -> Rec [] rs
    -> r
    -> b
  }

graphiteCast :: (rs :~: ss) => proxy rs -> proxy ss -> Graphite b r rs a -> Graphite b r ss a
graphiteCast _ _ (Graphite g) = Graphite $ \fromVal getVal rsRec r ->
  g fromVal (getVal . rcast) (rcast rsRec) r

-- modifyGraphite :: ([Diagram B] -> Diagram B) -> [r] -> Graphite rs a -> Graphite (r ': rs) a
-- modifyGraphite combine vals (Graphite g) = Graphite $ \getData ->
--   combine $ flip map vals $ \val -> g (curryRecFunc getData (Identity val))
--
-- modifyGraphiteWith :: ([Diagram B] -> Diagram B) -> [(r, Diagram B -> Diagram B)] -> Graphite rs a -> Graphite (r ': rs) a
-- modifyGraphiteWith combine vals (Graphite g) = Graphite $ \getData ->
--   combine $ flip map vals $ \(val,edit) -> g (curryRecFunc getData (Identity val)) & edit

single :: (r -> a -> m) -> Graphite m r '[] a
single f = Graphite (\fromVal getVal RNil r -> f r (getVal RNil))

gvalue :: Graphite Double r '[] a
gvalue = Graphite (\fromVal getVal RNil _ -> fromVal (getVal RNil))

-- figureAt :: Graphite (Double, Diagram B) r '[] a
-- figureAt = Graphite (\fromVal getVal RNil _ -> (fromVal (getVal RNil), hexagon 1.0))

figureAt :: (a -> r -> Diagram B) -> Graphite (Double, Diagram B) r '[] a
figureAt f = Graphite $ \fromVal getVal RNil r -> let v = getVal RNil in
  (fromVal v, f v r)

basicConnectedFigures ::
     (s -> r -> a -> Diagram B)
  -> (s -> Int -> Int -> Diagram B -> Diagram B)
  -> Graphite (Diagram B) s '[r] a
basicConnectedFigures makeMarker connectMarker = Graphite $ \fromVal getVal (rVals :& RNil) s ->
  let nums = take (length rVals) [0,1..] :: [Int]
      spacing = 100.0 / fromIntegral (length rVals - 1) :: Double
  in id
    $ localize
    $ applyAll (flip map (zip nums (drop 1 nums)) $ \(n1,n2) -> connectMarker s n1 n2)
    $ position
    $ flip map (zip nums rVals)
    $ \(i, r) -> let a = getVal (Identity r :& RNil) in
        (P $ V2 (spacing * fromIntegral i) (fromVal a), makeMarker s r a & named i)

xAxisFromZero :: HorizontalDirection -> (r -> String)
              -> Graphite (Diagram B) s (r ': rs) a
              -> Graphite (Diagram B) s (r ': rs) a
xAxisFromZero dir makeLabel (Graphite g) = Graphite $ \fromVal getVal (rVals :& rsNext) s -> let
  spacing = 100.0 / fromIntegral (length rVals - 1) :: Double
  translateDir = case dir of
    HorizontalLeft -> id
    HorizontalRight -> translateX (-100.0) -- ((-spacing) * fromIntegral (length rVals - 1))
  in translateDir $ mconcat
      [ g fromVal getVal (rVals :& rsNext) s
      , axisX2 spacing (map makeLabel rVals)
      ]

yAxisFromZero :: HorizontalDirection -> Int -> a -> (a -> Int -> a) -> (a -> String) -> Graphite (Diagram B) s rs a -> Graphite (Diagram B) s rs a
yAxisFromZero dir numSegments aInterval mult valToString (Graphite g) = Graphite $ \fromVal getVal rsNext s ->
  mconcat
    [ g fromVal getVal rsNext s
    , axisY2 dir (fromVal $ mult aInterval 1) (map (\i -> valToString (mult aInterval i)) (enumFromTo 0 numSegments))
    ]

data HorizontalDirection = HorizontalLeft | HorizontalRight

axisY2 :: HorizontalDirection -> Double -> [String] -> Diagram B
axisY2 dir interval strs =
  fromOffsets [scaleY (interval * fromIntegral (length strs - 1)) unitY]
  |||.
  atPoints (intervalPointsFromZeroY interval (length strs))
    (map (\str -> fromOffsets [unitDir] === alignedText 0 0.5 str # fontSizeL 2) strs)
  where
  (|||.) = beside (-unitDir)
  unitDir = case dir of
    HorizontalLeft -> unitX
    HorizontalRight -> unit_X

intervalPointsFromZeroY :: Double -> Int -> [Point V2 Double]
intervalPointsFromZeroY spacing n = fromOffsets $ replicate n (scaleY spacing unitY)

axisX2 :: Double -> [String] -> Diagram B
axisX2 interval strs =
  fromOffsets [scaleX (interval * fromIntegral (length strs - 1)) unitX]
  ===
  atPoints (intervalPointsFromZero interval (length strs))
    (map (\str -> fromOffsets [unitY] === alignedText 0.5 1.0 str # fontSizeL 2) strs)

intervalPointsFromZero :: Double -> Int -> [Point V2 Double]
intervalPointsFromZero spacing n =
  (fromOffsets $ replicate n (scaleX spacing unitX))

bar :: Double -> Graphite (Diagram B) r '[] a
bar width = Graphite (\fromVal getVal RNil _ -> rect width (fromVal (getVal RNil)))

barColoured :: Double -> (r -> Colour Double) -> Graphite (Diagram B) r '[] a
barColoured width getColour =
  Graphite (\fromVal getVal RNil r -> rect width (fromVal (getVal RNil)) & fc (getColour r))

-- stacked :: [r] -> Graphite rs a -> Graphite (r ': rs) a
-- stacked = modifyGraphite (alignB . vcat)

stacked :: (m -> Diagram B) -> Graphite m r rs a -> Graphite (Diagram B) s (r ': rs) a
stacked f (Graphite g) = Graphite $ \fromVal getVal (vals :& valsNext) s ->
  alignB $ vcat $ flip map vals $ \r ->
    f $ g fromVal (curryRecFunc getVal (Identity r)) valsNext r

paddedBy :: Double -> (m -> Diagram B) -> Graphite m r rs a -> Graphite (Diagram B) s (r ': rs) a
paddedBy spacing f (Graphite g) = Graphite $ \fromVal getVal (vals :& valsNext) s ->
  hsep spacing $ flip map vals $ \r ->
    f $ g fromVal (curryRecFunc getVal (Identity r)) valsNext r

connectedFigures :: Double -> Graphite (Double,Diagram B) r rs a -> Graphite (Diagram B) s (r ': rs) a
connectedFigures spacing (Graphite g) = Graphite $ \fromVal getVal (vals :& valsNext) s -> let
  nums = take (length vals) [0,1..] :: [Int]
  in id
    $ localize
    $ applyAll (flip map (zip nums (drop 1 nums)) $ \(n1,n2) -> connect n1 n2)
    $ position
    $ flip map (zip nums vals)
    $ \(i :: Int,r) -> let (y,diag) = g fromVal (curryRecFunc getVal (Identity r)) valsNext r in
          (P $ V2 (spacing * fromIntegral i) y, diag & named i)

connectedFiguresMany :: Double -> Graphite (Double,Diagram B) r rs a -> Graphite (Diagram B) t (s ': r ': rs) a
connectedFiguresMany spacing (Graphite g) = Graphite $ \fromVal getVal (sVals :& rVals :& valsNext) t -> let
  nums = take (length sVals) [0,1..] :: [Int]
  in id
    $ mconcat
    $ flip map rVals
    $ \r -> localize
    $ applyAll (flip map (zip nums (drop 1 nums)) $ \(n1,n2) -> connect n1 n2)
    $ position
    $ flip map (zip nums sVals)
    $ \(i :: Int,s) -> let (y,diag) = g fromVal (curryRecFunc (curryRecFunc getVal (Identity s)) (Identity r)) valsNext r in
          (P $ V2 (spacing * fromIntegral i) y, diag & named i)

connected :: Double -> Graphite Double r rs a -> Graphite (Diagram B) s (r ': rs) a
connected spacing (Graphite g) = Graphite $ \fromVal getVal (vals :& valsNext) s -> id
  $ fromVertices
  $ flip map (zip [0,1..] vals)
  $ \(i :: Int,r) -> P $ V2
        (spacing * fromIntegral i)
        (g fromVal (curryRecFunc getVal (Identity r)) valsNext r)

overlapped :: Graphite (Diagram B) r rs a -> Graphite (Diagram B) s (r ': rs) a
overlapped (Graphite g) = Graphite $ \fromVal getVal (vals :& valsNext) s ->
  mconcat $ flip map vals $ \r -> g fromVal (curryRecFunc getVal (Identity r)) valsNext r

curryRecFunc :: (Rec f (a ': as) -> b) -> f a -> Rec f as -> b
curryRecFunc f r rs = f (r :& rs)

