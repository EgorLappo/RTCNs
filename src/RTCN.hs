{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module RTCN (RTCN, Lineages, Lineages'(..), rtcns, rtcnToGraph) where

import           Data.IntMap.Strict                (IntMap, (!))
import qualified Data.IntMap.Strict                as IMap
import qualified Data.IntSet                       as ISet

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

data Lineages' a = Leaf | Reticulation a a a | Branching a a | Subdiv a deriving (Eq, Show)

type Lineages = Lineages' Int

type RTCN = IntMap Lineages

rtcnsWithProfile :: Int -> Profile -> [RTCN]
rtcnsWithProfile n' p' = go (n'+1) p' nodes' net'
  where
    nodes' = ISet.fromAscList [1..n']
    net' = IMap.fromList [(i, Leaf) | i <- [1..n']]
    go n [BranchingEvent] nodes net = let
        (v1,v2) = (ISet.findMin nodes, ISet.findMax nodes)
      in [IMap.insert n (Branching v1 v2) net]
    go n (x:xs) nodes net = case x of
      BranchingEvent -> do
        (i,j) <- pairs nodes
        let newnodes = ISet.insert n $ ISet.delete i $ ISet.delete j nodes
            newnet   = IMap.insert n (Branching i j) net
        go (n+1) xs newnodes newnet
      ReticulationEvent -> do
        (i,j,k) <- triples nodes
        let newnodes = ISet.insert (n+2) $ ISet.insert (n+1) $ ISet.delete i $ ISet.delete j $ ISet.delete k nodes
            newnet   = IMap.insert (n+2) (Subdiv n) $ IMap.insert (n+1) (Subdiv n) $ IMap.insert n (Reticulation i j k) net
        go (n+3) xs newnodes newnet

    pairs ls = [(i,j) | i <- ISet.toList ls, j <- ISet.toList ls, i < j]
    triples ls = [(i, j, k) | (i,j) <- pairs ls, k <- ISet.toList ls, k /= i, k /= j]

tryCollapse :: RTCN -> Int -> RTCN
tryCollapse net i
  | not $ IMap.member i net = net
  | otherwise = case net ! i of
      Subdiv j -> let
          net' = IMap.delete i net
          net'' = IMap.map (replaceLineageLabel i j) net'
        in tryCollapse net'' j
      _ -> net

collapse :: RTCN -> RTCN
collapse net = foldl tryCollapse net (IMap.keys net)

rtcns' :: Int -> Int -> [RTCN]
rtcns' n b = concat [rtcnsWithProfile n p | p <- qProfiles n b]

rtcns :: Int -> Int -> [RTCN]
rtcns n r = rtcns' n (n-r-1)

keepNonIsomorphic :: [RTCN] -> [RTCN]
keepNonIsomorphic [] = []
keepNonIsomorphic (x:xs) = x : keepNonIsomorphic (filter (not . isIsomorphic x) xs)

isIsomorphic :: RTCN -> RTCN -> Bool
isIsomorphic _ _ = False -- TODO!

-- helper functions

rtcnToGraph :: RTCN -> G.Gr Int ()
rtcnToGraph net = mkGraph nodes edges
  where nodes = [(i,i) | i <- IMap.keys net]
        edges = [(i, j, ()) | (i,_) <- nodes, j <- getLineages $ net ! i]

getLineages :: Lineages' a -> [a]
getLineages Leaf                 = []
getLineages (Reticulation i j k) = [i,j,k]
getLineages (Branching i j)      = [i,j]
getLineages (Subdiv i)           = [i]

replaceLineageLabel :: Eq a => a -> a -> Lineages' a -> Lineages' a
replaceLineageLabel x y l = case l of
    Leaf               -> Leaf
    Reticulation i j k -> Reticulation (f i) (f j) (f k)
    Branching i j      -> Branching (f i) (f j)
    Subdiv i           -> Subdiv (f i)
  where f z = if z == x then y else z
