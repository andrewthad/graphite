{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE GADTs               #-}

module Graphite where

import           Control.Monad
import           Data.Colour.Palette.BrewerSet (ColorCat (Set1), brewerSet)
import           Data.Foldable                 (foldl')
import           Data.Functor.Identity
import           Data.Vinyl.Core
import           Data.Vinyl.TypeLevel
import           Lucid.Base                    (renderText)
import           Prelude

newtype Graphite res b values lists = Graphite
  { runGraphite
    :: Rec Identity values
    -> (Rec Identity lists -> res)
    -> Rec [] lists
    -> b
  }

build :: Rec [] rs -> (Rec Identity rs -> res) -> Graphite res b '[] rs -> b
build vals getVal (Graphite g) = g RNil getVal vals

modifyFigure :: (Rec Identity values -> fig1 -> fig2) -> Graphite res fig1 values lists -> Graphite res fig2 values lists
modifyFigure f (Graphite g) = Graphite $ \vals getRes valLists -> 
  f vals $ g vals getRes valLists

modifyFigureFull :: (Rec Identity values -> Rec [] lists -> fig1 -> fig2) -> Graphite res fig1 values lists -> Graphite res fig2 values lists
modifyFigureFull f (Graphite g) = Graphite $ \vals getRes valLists -> 
  f vals valLists $ g vals getRes valLists

mergeGraphite :: ([c] -> b) -> Graphite res c (v ': values) lists -> Graphite res b values (v ': lists)
mergeGraphite combine (Graphite g) = Graphite $ \vals getVal (valList :& valLists) ->
  combine $ flip map valList $ \val -> g 
    (Identity val :& vals) 
    (curryRecFunc getVal (Identity val)) 
    valLists 

mergeGraphite2 :: ([(v,c)] -> b) -> Graphite res c (v ': values) lists -> Graphite res b values (v ': lists)
mergeGraphite2 combine (Graphite g) = Graphite $ \vals getVal (valList :& valLists) ->
  combine $ flip map valList $ \val -> (,) val $ g 
    (Identity val :& vals) 
    (curryRecFunc getVal (Identity val)) 
    valLists 

curryRecFunc :: (Rec f (a ': as) -> b) -> f a -> Rec f as -> b
curryRecFunc f r rs = f (r :& rs)



-- reverseRec :: 

-- takeOneMore :: Natty n -> Rec proxy rs -> (Take ('S n) rs :~: (Take n rs ++ '[RecAt n rs]))
-- takeOneMore Zy (_ :& _) = Refl
-- takeOneMore (Sy n) (_ :& _) = 
--
-- n ~ 'S m
-- Take ('S ('S m)) rs
-- Take ('S m) rs ~ (Take m rs ++ '[RecAt m rs])
-- 
--
--

-- takeOneMore (Sy n) (_ :& rs) = case takeOneMore n rs of
--   Refl -> Refl

-- If we takeReverse (n + 1) elements to get a,
-- and we drop n elements to get b,
-- then head a == head b.
-- someProof :: Natty n -> Rec proxy rs
--   -> (Head (TakeReverseHelper '[] ('S n) rs) :~: Head (Drop n rs))
-- someProof Zy (_ :& _) = Refl
-- someProof (Sy n) (r :& rs) = case someProof n rs of
--   Refl -> Refl

-- takeProof :: Natty n -> Rec proxy (r ': rs) -> ((r ': TakeReverse n rs) :~: TakeReverse ('S n) (r ': rs))
-- -- takeProof Zy RNil = error "impossible"
-- takeProof Zy (_ :& _) = Refl
-- takeProof (Sy n) (_ :& rs) = case takeProof n rs of
--   Refl -> Refl

-- takeReverseHelperProof :: Rec proxy as -> Natty n -> Rec proxy (r ': rs) 
--   -> (Head (TakeReverseHelper as n rs) :~: Head (TakeReverseHelper as ('S n) (r ': rs)))
-- takeReverseHelperProof (a :& asNext) Zy (_ :& _ :& _) = Refl
-- takeReverseHelperProof as (Sy n) (r :& s :& rs) = 
--   case takeReverseHelperProof (r :& as) n (s :& rs) of
--     Refl -> Refl
  -- case takeReverseHelperProof (r :& as) n rs of
  -- Refl -> Refl



-- shrinkAt :: Natty n -> Proxy rs -> (RecAt n rs :~: RecAt ('S n) (r ': rs))
-- shrinkAt Zy _ = Refl
-- -- shrinkAt (Sy n) p = case shrinkAt n 
-- 
-- dropRec :: Natty n -> Rec f rs -> Rec f (Drop n rs)
-- dropRec Zy     r         = r
-- dropRec (Sy n) (_ :& rs) = dropRec n rs
-- 
-- takeRec :: Natty n -> Rec f rs -> Rec f (Take n rs)
-- takeRec Zy     _         = RNil
-- takeRec (Sy n) (r :& rs) = r :& takeRec n rs
-- 
-- takeReverseRec :: Natty n -> Rec f rs -> Rec f (TakeReverse n rs)
-- takeReverseRec n r = takeReverseRecHelper RNil n r
-- 
-- takeReverseRecHelper :: (TakeReverseHelper as n bs ~ cs) 
--   => Rec f as -> Natty n -> Rec f bs -> Rec f cs
-- takeReverseRecHelper r Zy xs = r
-- takeReverseRecHelper r (Sy n) (x :& xs) = takeReverseRecHelper (x :& r) n xs


