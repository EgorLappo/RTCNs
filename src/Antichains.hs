{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Antichains where

import           Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IMap

import           Data.IntSet        (IntSet, member, (\\))
import qualified Data.IntSet        as ISet

import           RPN

type Antichain = IntSet -- ^ A set of indices into the nodes of an RPN

maximalAntichains :: RPN -> [Antichain]
maximalAntichains = undefined



