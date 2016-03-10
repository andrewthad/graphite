{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Functor.Identity
import           Data.Proxy
import           Data.Vinyl
import           Diagrams.Backend.SVG
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude             hiding ((:&))
-- import           Graphite
-- import           Graphite.Diagrams
import           Graphite.Types (Graphite)
import qualified Graphite.Combinators as Graphite
import           Diagrams.Graph.Combinators
import           Control.Arrow
-- import           StackedBarGraph

main :: IO ()
-- main = mainWith $ sideBySideAxis 
main = mainWith $ 
  (pad 1.2 $ centerXY $ Graphite.build (allAges :& allGenders :& RNil) funcData2 graphC)
  -- ===
  -- (pad 1.2 $ centerXY $ build (allGenders :& allAges :& RNil) (funcData2 . rcast) graphD)

-- main = mainWith $ pad 1.4 $ centerXY $ showOrigin $ build (allGenders :& RNil) funcDataGender graphB

test :: Diagram B
test = hcat (replicate 5 myRect) # center # showOrigin

myRect :: Diagram B
myRect = rect 44 66 # padX 1.1

-- graphA :: Graphite Int (Diagram B) '[] '[Gender]
-- graphA = id
--   $ sideBySideAxis 1.2 show
--   $ bar 10 fromIntegral
-- 
-- graphB :: Graphite Int (Diagram B) '[] '[Gender]
-- graphB = id
--   $ sideBySide 0
--   -- $ barTopLabel 10 fromIntegral (show . runIdentity . rget (Proxy :: Proxy Gender))
--   $ liftFigureChange (lineOnBottom . padX 1.1)
--   $ labelTop (show . runIdentity . rget (Proxy :: Proxy Gender))
--   $ bar 10 fromIntegral

graphC :: Graphite Int (Diagram B) '[] '[Age,Gender]
graphC = id
  $ Graphite.figure (\_ _ ds -> let d = alignL (hcat ds) in
      mappend d $ alignBL $ createYAxis (width d) 0 3 12 fromIntegral show
        (lc (blend 0.3 white purple) . lw thin . dashingG [0.7,0.7] 0) id
    )
  $ Graphite.pop

  $ Graphite.figure (\r _ d -> let age = runIdentity $ rget (Proxy :: Proxy Age) r in d
      & hsep 0 . map alignB
      & center
      & padX 1.1
      & lineOnBottom
      & besideText unit_Y (show age)
    )
  $ Graphite.pop

  $ Graphite.figure (\r _ d -> let gender = runIdentity $ rget (Proxy :: Proxy Gender) r in d 
      & fc (genderColour gender) 
      & besideText unitY (show gender) 
      & center 
      & padX 1.1
      & fontSizeL 2
    )
  $ Graphite.start' (\res -> rect 10 (fromIntegral res))

-- graphD :: Graphite Int (Diagram B) '[] '[Gender, Age]
-- graphD = id
--   $ liftFigureChange (\fig -> mappend fig $ lw thin $ lineOnRight (createYAxis 1.0 0 3 12 fromIntegral show (lw thin)))
--   -- $ liftFigureChange 
--   --     (\f -> mappend f (alignBL $ createYAxis (width f) 0 3 12 fromIntegral show
--   --            (lc (blend 0.3 white purple) . lw thin . dashingG [0.7,0.7] 0)
--   --     ))
--   $ modifyFigureFull 
--       (\r l fig -> mappend fig $ 
--           createXAxis 1.0 10.0 (rget (Proxy :: Proxy Age) l) show (lw thin)
--       )
--   $ modifyFigureFull 
--       (\r l fig -> mappend fig
--           $ lw thin
--           $ horizontalLine (10.0 * fromIntegral (length (rget (Proxy :: Proxy Age) l) - 1))
--       )
--   $ overlapped
-- 
--   $ modifyFigure (\r -> lc $ darken 0.6 $ genderColour $ runIdentity $ rget (Proxy :: Proxy Gender) r)
--   $ connected 10.0
-- 
--   $ modifyFigure (\r -> first $ fc $ genderColour $ runIdentity $ rget (Proxy :: Proxy Gender) r)
--   $ singlePoint 1 fromIntegral

data Gender = Male | Female | Unknown
  deriving (Eq,Ord,Bounded,Enum,Show)
data Age = Young | Middle | Old | VeryOld
  deriving (Eq,Ord,Bounded,Enum,Show)

allAges :: [Age]
allAges = enumFromTo minBound maxBound

allGenders :: [Gender]
allGenders = enumFromTo minBound maxBound


genderColour :: Gender -> Colour Double
genderColour g = case g of
  Male -> blue
  Female -> magenta
  Unknown -> grey

-- myGraphA,myGraphB :: Graphite '[Age,Gender] Int
-- myGraphA = intervals 16 allAges $ grouped 1 allGenders $ bar fromIntegral 4 id
-- myGraphB = id
--   $ intervalsWithAxis 16 show (zip allAges (repeat id))
--   -- $ stacked allGenders
--   $ stackedWith [ (Male,fc purple . atop (text "Male" & fontSizeL 2 & fc white))
--                 , (Female, fc blue . atop (text "Female" & fontSizeL 2 & fc white))
--                 , (Unknown, fc green . atop (text "N/A" & fontSizeL 2 & fc white))
--                 ]
--   $ bar fromIntegral 6.0 id
--
-- myGraphC :: Graphite '[Gender] Int
-- myGraphC = intervals 16 allGenders $ bar fromIntegral 4.0 (fc blue)
--
-- myGraphD :: Graphite '[Gender] Int
-- myGraphD = lineGraph fromIntegral 16.0 allGenders (fc blue)

funcDataGender :: Rec Identity '[Gender] -> Int
funcDataGender (Identity gender :& RNil) = case gender of
  Male    -> 11
  Female  -> 14
  Unknown -> 2

funcData :: Rec Identity '[Age,Gender] -> Int
funcData (Identity age :& Identity gender :& RNil) = fAge age * fGender gender
  where
  fAge Young = 6
  fAge Middle = 7
  fAge Old = 8
  fGender Male   = 4
  fGender Female = 5
  fGender Unknown = 1

funcData2 :: Rec Identity '[Age,Gender] -> Int
funcData2 (Identity age :& Identity gender :& RNil) = case age of
  Young -> case gender of
    Male -> 18
    Female -> 16
    Unknown -> 30
  Middle -> case gender of
    Male -> 12
    Female -> 18
    Unknown -> 22
  Old -> case gender of
    Male -> 4
    Female -> 26
    Unknown -> 20
  VeryOld -> case gender of
    Male -> 2
    Female -> 22
    Unknown -> 10


-- myGraphE :: Graphite (Diagram B) r '[Gender] Int
-- myGraphE = stacked id $ bar 4.0
-- 
-- myGraphF :: Graphite (Diagram B) r '[Age,Gender] Int
-- myGraphF = paddedBy 12.0 id $ stacked id $ barColoured 4.0 genderColour
-- 
-- myGraphH :: Graphite (Diagram B) r '[Gender] Int
-- myGraphH = connected 4 $ gvalue
-- 
-- myGraphI :: Graphite (Diagram B) r '[Age,Gender] Int
-- myGraphI = overlapped $ connected 16.0 $ gvalue
-- 
-- myGraphJ :: Graphite (Diagram B) r '[Age,Gender] Int
-- myGraphJ = id
--   -- $ overlapped
--   $ connectedFiguresMany 16.0
--   $ figureAt (\val gender -> mconcat
--       [ text (show val)
--       , hexagon 1.0 # fc (genderColour gender)
--       ]
--     )
-- 
-- myGraphK :: Graphite (Diagram B) r '[Gender,Age] Int
-- myGraphK = id
--   -- $ overlapped
--   $ connectedFiguresMany 16.0
--   $ figureAt (\val _ -> mconcat
--       [ text (show val)
--       , hexagon 1.0
--       ]
--     )
-- 
-- myGraphL :: Graphite (Diagram B) r '[Age,Gender] Int
-- myGraphL = id
--   $ yAxisFromZero HorizontalRight 10 4 (*) show
--   $ xAxisFromZero HorizontalRight show
--   $ graphiteCast (Proxy :: Proxy '[Gender,Age]) (Proxy :: Proxy '[Age,Gender])
--   $ overlapped
--   $ basicConnectedFigures
--       (\g v a -> mconcat [text (show a) & fc white & fontSize 10, hexagon 2.0 & fc (genderColour g)])
--       (\g -> connectOutside' (with { _arrowHead = noHead, _shaftStyle = lc purple mempty} ))

-- padX :: (Metric v, OrderedField n, Monoid' m, R2 v)
--   => n -> QDiagram b v n m -> QDiagram b v n m
-- padX s d = withEnvelope (d # scaleX s) d


-- main = mainWith $ showOrigin $ runGraphite myGraphH fromIntegral funcDataGender (allGenders :& RNil) ()
-- main = mainWith $ showOrigin $ runGraphite myGraphF fromIntegral funcData2 (allAges :& allGenders :& RNil) ()
-- main = mainWith $ pad 1.3 $ centerXY $ showOrigin
--   $ runGraphite myGraphL (\i -> fromIntegral i * 2) (funcData2 . rcast)
--     (rcast $ allAges :& allGenders :& RNil) ()

-- ( build myGraphA funcData
--   ===
--   axisX 16 (map show allAges)
--   ===
--   square 4
-- ) |||
-- ( axisY 16 ["1","2","3","4","5"] ||| build myGraphB funcData) |||
-- ( build myGraphD funcDataGender
--   ===
--   axisX 16 (map show allGenders)
--   ===
--   square 4
-- )
