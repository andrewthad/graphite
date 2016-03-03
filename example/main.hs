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
main = mainWith $
  ( build myGraphA funcData
    ===
    axisX 16 (map show allAges)
    ===
    square 4
  ) |||
  ( axisY 16 ["1","2","3","4","5"] ||| build myGraphB funcData) |||
  ( build myGraphD funcDataGender
    ===
    axisX 16 (map show allGenders)
    ===
    square 4
  )

data Gender = Male | Female | Unknown
  deriving (Eq,Ord,Bounded,Enum,Show)
data Age    = Young | Middle | Old
  deriving (Eq,Ord,Bounded,Enum,Show)

allAges :: [Age]
allAges = enumFromTo minBound maxBound

allGenders :: [Gender]
allGenders = enumFromTo minBound maxBound

myGraphA,myGraphB :: Graphite '[Age,Gender] Int
myGraphA = intervals 16 allAges $ grouped 1 allGenders $ bar fromIntegral 4 id
myGraphB = id
  $ intervalsWithAxis 16 show (zip allAges (repeat id))
  -- $ stacked allGenders
  $ stackedWith [ (Male,fc purple . atop (text "Male" & fontSizeL 2 & fc white))
                , (Female, fc blue . atop (text "Female" & fontSizeL 2 & fc white))
                , (Unknown, fc green . atop (text "N/A" & fontSizeL 2 & fc white))
                ]
  $ bar fromIntegral 6.0 id

myGraphC :: Graphite '[Gender] Int
myGraphC = intervals 16 allGenders $ bar fromIntegral 4.0 (fc blue)

myGraphD :: Graphite '[Gender] Int
myGraphD = lineGraph fromIntegral 16.0 allGenders (fc blue)

funcDataGender :: Rec Identity '[Gender] -> Int
funcDataGender (Identity gender :& RNil) = case gender of
  Male -> 11
  Female -> 14
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



