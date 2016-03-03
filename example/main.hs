{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Functor.Identity
import           Data.Vinyl
import           Diagrams.Backend.SVG
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude             hiding ((:&))
import           StackedBarGraph

main :: IO ()
-- main = mainWith $ showOrigin $ runGraphite myGraphH fromIntegral funcDataGender (allGenders :& RNil) ()
main = mainWith $ showOrigin $ runGraphite myGraphI fromIntegral funcData2 (allAges :& allGenders :& RNil) ()

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

data Gender = Male | Female | Unknown
  deriving (Eq,Ord,Bounded,Enum,Show)
data Age    = Young | Middle | Old
  deriving (Eq,Ord,Bounded,Enum,Show)

allAges :: [Age]
allAges = enumFromTo minBound maxBound

allGenders :: [Gender]
allGenders = enumFromTo minBound maxBound

myGraphE :: Graphite (Diagram B) r '[Gender] Int
myGraphE = stacked id $ bar 4.0

myGraphF :: Graphite (Diagram B) r '[Age,Gender] Int
myGraphF = paddedBy 12.0 id $ stacked id $ barColoured 4.0 genderColour

myGraphH :: Graphite (Diagram B) r '[Gender] Int
myGraphH = connected 4 $ gvalue

myGraphI :: Graphite (Diagram B) r '[Age,Gender] Int
myGraphI = overlapped $ connected 16.0 $ gvalue


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
    Male -> 2
    Female -> 26
    Unknown -> 20



