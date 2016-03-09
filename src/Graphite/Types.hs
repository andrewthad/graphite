module Graphite.Types where

import Data.Vinyl.Core
import Data.Functor.Identity

-- | The pneumonic for the types is: result, figure, knowns, unknowns.
newtype Graphite res fig ks us = Graphite
  { runGraphite
    :: Rec Identity ks
    -> (Rec Identity us -> res)
    -> Rec [] us
    -> fig
  }
