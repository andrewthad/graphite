{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE UndecidableInstances #-}
module Minimal where

import Data.Type.Equality

data Nat = Z | S Nat

data Natty (n :: Nat) where
  Zy :: Natty 'Z
  Sy :: Natty n -> Natty ('S n)

data HRec (vs :: [*]) where
  HRecNil  :: HRec '[]
  HRecCons :: x -> HRec xs -> HRec (x ': xs)

data HProxy (vs :: [*]) where
  HProxyNil  :: HProxy '[]
  HProxyCons :: HProxy xs -> HProxy (x ': xs)

data Parts n rs = Parts
  { partLeft  :: HRec (Drop n rs) 
  , partRight :: HRec (TakeReverse n rs)
  , partNatty :: Natty n
  , partProxy :: HProxy rs
  }

-- The type families Drop, Take, and TakeReverse
-- are all partial.
type family Drop (n :: Nat) (xs :: [k]) :: [k] where
  Drop 'Z xs = xs
  Drop ('S n) (x ': xs) = Drop n xs

type family Take (n :: Nat) (xs :: [k]) :: [k] where
  Take 'Z xs = '[]
  Take ('S n) (x ': xs) = x ': Take n xs

type family TakeReverse (n :: Nat) (xs :: [k]) :: [k] where
  TakeReverse n xs = TakeReverseHelper '[] n xs

type family TakeReverseHelper (ys :: [k]) (n :: Nat) (xs :: [k]) :: [k] where
  TakeReverseHelper res 'Z xs = res
  TakeReverseHelper res ('S n) (x ': xs) = TakeReverseHelper (x ': res) n xs

-- moveRight :: Parts n rs -> Parts (S n) rs
-- moveRight (Parts pleft@(HRecCons pleftHead _) pright natty proxy) = 
--   case dropOneProof natty proxy of
--     Refl -> Parts (dropOne pleft) (HRecCons pleftHead pright) (Sy natty) proxy

dropOneProof :: Natty n -> HProxy rs -> (Drop ('S n) rs :~: Drop ('S 'Z) (Drop n rs))
dropOneProof Zy _ = Refl
dropOneProof (Sy n) (HProxyCons rs) = case dropOneProof n rs of
  Refl -> Refl

dropOne :: HRec rs -> HRec (Drop ('S 'Z) rs)
dropOne (HRecCons _ rs) = rs



