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

pop :: Graphite res fig (x ': vs) ls -> Graphite res [fig] vs (x ': ls)
pop (Graphite g) = Graphite $ \vals getVal (valList :& valLists) ->
  flip map valList $ \val -> g 
    (Identity val :& vals) 
    (curryRecFunc getVal (Identity val)) 
    valLists 

curryRecFunc :: (Rec f (a ': as) -> b) -> f a -> Rec f as -> b
curryRecFunc f r rs = f (r :& rs)

