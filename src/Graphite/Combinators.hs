{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
module Graphite.Combinators 
  ( build
  , start
  , start'
  , figure
  , figure'
  , pop
  ) where

import Graphite.Types
import Data.Vinyl.Core
import Data.Functor.Identity

build :: Rec [] ls -> (Rec Identity ls -> res) -> Graphite res b '[] ls -> b
build vals getVal (Graphite g) = g RNil getVal vals

start :: (Rec Identity ks -> res -> fig) -> Graphite res fig ks '[]
start f = Graphite $ \knowns getRes RNil -> f knowns (getRes RNil)

start' :: (res -> fig) -> Graphite res fig ks '[]
start' f = start (\_ res -> f res)

figure :: (Rec Identity vs -> Rec [] ls -> fig1 -> fig2) -> Graphite res fig1 vs ls -> Graphite res fig2 vs ls
figure f (Graphite g) = Graphite $ \vals getRes valLists -> 
  f vals valLists $ g vals getRes valLists

figure' :: (fig1 -> fig2) -> Graphite res fig1 vs ls -> Graphite res fig2 vs ls
figure' f = figure (\_ _ fig1 -> f fig1)

figureKnownHead :: (x -> fig1 -> fig2) -> Graphite res fig1 (x ': ks) us -> Graphite res fig2 (x ': ks) us
figureKnownHead f = figure (\(Identity x :& _) _ fig -> f x fig)

figureUnknownHead :: ([x] -> fig1 -> fig2) -> Graphite res fig1 ks (x ': us) -> Graphite res fig2 ks (x ': us)
figureUnknownHead f = figure (\_ (xs :& _) fig -> f xs fig)

pop :: Graphite res fig (x ': vs) ls -> Graphite res [fig] vs (x ': ls)
pop (Graphite g) = Graphite $ \vals getVal (valList :& valLists) ->
  flip map valList $ \val -> g 
    (Identity val :& vals) 
    (curryRecFunc getVal (Identity val)) 
    valLists 

popPlus :: Graphite res fig (x ': ks) us -> Graphite res (x -> fig) ks (x ': us)
popPlus (Graphite g) = Graphite $ \vals getVal (_valList :& valLists) valToCheck ->
  g (Identity valToCheck :& vals) (curryRecFunc getVal (Identity valToCheck)) valLists 

-- concat :: Graphite res fig1 ks us -> Graphite res fig2 fig1 fig2 

curryRecFunc :: (Rec f (a ': as) -> b) -> f a -> Rec f as -> b
curryRecFunc f r rs = f (r :& rs)

