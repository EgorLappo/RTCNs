{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module RTCN where

import           Data.IntMap.Strict                (IntMap, (!))
import qualified Data.IntMap.Strict                as IMap

import           Data.IntSet                       (IntSet)
import qualified Data.IntSet                       as ISet
import           Data.List                         (sort)

import           Data.Graph.Inductive              (mkGraph)
import qualified Data.Graph.Inductive.PatriciaTree as G

-- backward construction (proper)

data BWNode' a = B a a | R a a a
     deriving (Eq, Show)

type BWNode = BWNode' Int

type BWRTCN = [BWNode]

data Event = BranchingEvent | ReticulationEvent
    deriving (Eq, Show)

type Profile = [Event]

-- for n >= 2 and 0 <= b <= n-1,
-- produce a list of length n-1, with exactly b reticulations
qProfiles :: Int -> Int -> [Profile]
qProfiles n b
    | n < 2 || b <= 0 || b >= n = []
    | n == 2 = [[BranchingEvent]]
    | otherwise = fmap (reverse . (BranchingEvent :)) (qProfiles' (n-2) (b-1))
  where qProfiles' n' b'
         | n' <= 0 = [[]]
         | b' == 0 = fmap (ReticulationEvent :) (qProfiles' (n'-1) 0)
         | b' == n' = [[BranchingEvent | _ <- [1..n']]]
         | otherwise = fmap (BranchingEvent :) (qProfiles' (n'-1) (b'-1)) ++ fmap (ReticulationEvent :) (qProfiles' (n'-1) b')

bwtcnsWithProfile :: Int -> Profile -> [BWRTCN]
bwtcnsWithProfile _ [BranchingEvent] = [[B 1 2]]
bwtcnsWithProfile n (e:es)
  | e == BranchingEvent = [B i j : r | (i,j) <- pairs n, r <- bwtcnsWithProfile (n-1) es]
  | e == ReticulationEvent = [R i j k : r | (i,j,k) <- triples n, r <- bwtcnsWithProfile (n-1) es]
  where
    pairs n = [(i,j) | i <- [1..n], j <- [1..n], i < j]
    triples n = [(i, j, k) | (i,j) <- pairs n, k <- [1..n], k /= i, k /= j]

 -- RTCNs with n leaves and b branchings
bwrtcns' :: Int -> Int -> [BWRTCN]
bwrtcns' n b = concat [bwtcnsWithProfile n p | p <- qProfiles n b]

-- RTCNs with n leaves and r reticulations
bwrtcns :: Int -> Int -> [BWRTCN]
bwrtcns n r = bwrtcns' n (n-r-1)

-- now let's generate something that looks like a DAG
-- this would "forget the ranking", but the purpose is to look at what rankings are possible, so it's okay
-- one could say we are generating all "rankable" TCNs

type RTCN = IntMap IntSet

rtcnsWithProfile' :: Int -> Profile -> [RTCN]
rtcnsWithProfile' n' p' = go (n'+1) p' nodes' net'
  where
    nodes' = ISet.fromAscList [1..n']
    net' = IMap.fromList [(i, ISet.empty) | i <- [1..n']]
    -- insert a new node with two tree lineages for the two remaining labels in ls (always going to me two, min and max)
    go n [BranchingEvent] nodes net = let
        (v1,v2) = (ISet.findMin nodes, ISet.findMax nodes)
      in [IMap.insert n (ISet.fromList [v1, v2]) net]
    go n (x:xs) nodes net = case x of
      BranchingEvent    -> do
        (i,j) <- pairs nodes
        let newnodes = ISet.insert n $ ISet.delete i $ ISet.delete j nodes
            newnet   = IMap.insert n (ISet.fromList[i, j]) net
        go (n+1) xs newnodes newnet
      ReticulationEvent -> do
        (i,j,k) <- triples nodes
        let newnodes = ISet.insert (n+2) $ ISet.insert (n+1) $ ISet.delete i $ ISet.delete j $ ISet.delete k nodes
            newnet   = IMap.insert (n+2) (ISet.fromList [n]) $ IMap.insert (n+1) (ISet.fromList [n]) $ IMap.insert n (ISet.fromList [i,j,k]) net
        go (n+3) xs newnodes newnet
    pairs ls = [(i,j) | i <- ISet.toList ls, j <- ISet.toList ls, i < j]
    triples ls = [(i, j, k) | (i,j) <- pairs ls, k <- ISet.toList ls, k /= i, k /= j]


outDegree :: RTCN -> Int -> Int
outDegree net i = ISet.size $ net ! i

tryCollapse :: RTCN -> Int -> RTCN
tryCollapse net i
  | not $ IMap.member i net = net
  | outDegree net i /= 1 = net
  | otherwise = let
      j = ISet.findMin $ net ! i
      net' = IMap.delete i net
      net'' = IMap.map (\es -> if ISet.member i es then ISet.insert j $ ISet.delete i es else es) net'
    in tryCollapse net'' j

collapse :: RTCN -> RTCN
collapse net = foldl tryCollapse net (IMap.keys net)

rtcns' :: Int -> Int -> [RTCN]
rtcns' n b = concat [rtcnsWithProfile' n p | p <- qProfiles n b]

rtcns :: Int -> Int -> [RTCN]
rtcns n r = rtcns' n (n-r-1)

rtcnToGraph :: RTCN -> G.Gr Int ()
rtcnToGraph net = mkGraph nodes edges
  where nodes = [(i,i) | i <- IMap.keys net]
        edges = [(i, j, ()) | (i,_) <- nodes, j <- ISet.toList $ net ! i]

