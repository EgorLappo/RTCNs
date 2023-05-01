{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase #-}

module RTCN (RTCN, Lineages, Lineages'(..), rtcns, rtcnTopologies, rtcnToGraph, edgeList, maximalAntichains, antichainLatticeGraph) where

import           Data.IntMap                       (IntMap, (!))
import qualified Data.IntMap                       as IMap
import           Data.IntSet                       (IntSet, (\\))
import qualified Data.IntSet                       as ISet

import           Data.Graph.Inductive              (mkGraph)
import qualified Data.Graph.Inductive.PatriciaTree as G
import           Data.List                         (nub)
import           Data.Maybe                        (fromJust)
import           Data.Tuple                        (swap)

--- DEBUG CODE: very useful, can be used with `seq` as
-- `seq (dbg x) y` to print out the value of x and return y
import           System.IO.Unsafe                  (unsafePerformIO)

dbg :: Show a => a -> a
dbg a = unsafePerformIO $ do
  print a
  return a


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
isIsomorphic net1 net2 = tryMatch net1 net2 (root net1) (root net2)

tryMatch :: RTCN -> RTCN -> Int -> Int -> Bool
tryMatch net1 net2 v1 v2
  | isLeaf net1 v1 && isLeaf net2 v2 = True
  | not $ sameNodeType (net1 ! v1) (net2 ! v2) = False
  | otherwise = case (net1 ! v1, net2 ! v2) of
      (Branching l1 r1, Branching l2 r2) -> tryMatch net1 net2 l1 l2 && tryMatch net1 net2 r1 r2 || tryMatch net1 net2 l1 r2 && tryMatch net1 net2 r1 l2

      (Reticulation i1 j1 k1, Reticulation i2 j2 k2) ->
           tryMatch net1 net2 i1 i2 && tryMatch net1 net2 j1 j2 && tryMatch net1 net2 k1 k2
        || tryMatch net1 net2 i1 j2 && tryMatch net1 net2 j1 i2 && tryMatch net1 net2 k1 k2
        || tryMatch net1 net2 i1 k2 && tryMatch net1 net2 j1 j2 && tryMatch net1 net2 k1 i2
        || tryMatch net1 net2 i1 j2 && tryMatch net1 net2 j1 k2 && tryMatch net1 net2 k1 i2
        || tryMatch net1 net2 i1 k2 && tryMatch net1 net2 j1 i2 && tryMatch net1 net2 k1 j2
        || tryMatch net1 net2 i1 i2 && tryMatch net1 net2 j1 k2 && tryMatch net1 net2 k1 j2

      _ -> error "tryMatch: please don't compute isomorphisms with subdivision edges present in the RTCNs"

rtcnTopologies :: Int -> Int -> [RTCN]
rtcnTopologies = ((keepNonIsomorphic . map collapse) .) . rtcns

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

isLeaf :: RTCN -> Int -> Bool
isLeaf net v = case net ! v of
  Leaf -> True
  _    -> False

sameNodeType :: Lineages -> Lineages -> Bool
sameNodeType Leaf Leaf                                 = True
sameNodeType (Reticulation _ _ _) (Reticulation _ _ _) = True
sameNodeType (Branching _ _) (Branching _ _)           = True
sameNodeType (Subdiv _) (Subdiv _)                     = True
sameNodeType _ _                                       = False

root :: RTCN -> Int
root net = head $ filter (isRoot net) (labels net)
  where
    labels = IMap.keys

isRoot :: RTCN -> Int -> Bool
isRoot n v = null $ parents n v

isChild :: RTCN -> Int -> Int -> Bool
isChild n i j = j `elem` children n i

children :: RTCN -> Int -> [Int]
children n v = case n ! v of
  Leaf               -> []
  Reticulation i j k -> [i,j,k]
  Branching i j      -> [i,j]
  Subdiv i           -> [i]

parents :: RTCN -> Int -> [Int]
parents n i = filter (\j -> isChild n j i) $ labels n
  where
    labels = IMap.keys

ancestors :: RTCN -> Int -> [Int]
ancestors n v = case parents n v of
  [] -> []
  ps -> ps ++ concatMap (ancestors n) ps

-- *** MAXIMAL ANTICHAINS ***

type Edge = (Int, Int)

edgeList :: RTCN -> [Edge]
edgeList n = [(i,j) | i <- IMap.keys n, j <- children n i]

isParentEdge :: Edge -> Edge -> Bool
isParentEdge (_,j) (i',_) = j == i'

isChildEdge :: Edge -> Edge -> Bool
isChildEdge (i,_) (_,j') = i == j'

parentEdges :: Edge -> [Edge] -> [Int]
parentEdges e = map fst . filter (\(_, e') -> e' `isParentEdge` e) . zip [0..]

childEdges :: Edge -> [Edge] -> [Int]
childEdges e = map fst . filter (\(_, e') -> e' `isChildEdge` e) . zip [0..]

-- the idea is that any finite DAG is a (multifurcating) tree with some edges identified
-- so we generate all max antichains for that implicit tree, and then filter out the wrong ones
maximalAntichains :: RTCN -> ([(IntSet, Int)], [Edge])
maximalAntichains net = (antichains, coverings)
  where
    -- get the list of edges in an RTCN DAG, collapsing parallel edges
    -- this should be all we need to compute antichains in the poset of edges
    elist = nub $ edgeList net

    -- get the set of children *edges* of each edge
    allChildren = IMap.fromList $ zip [0..] $ map (`childEdges` elist) elist
    allParents = IMap.fromList $ zip [0..] $ map (`parentEdges` elist) elist
    -- root edges are those with no children (there will be one or two of these since we could have collapsed a parallel pair)
    rootEdges = ISet.fromList $ filter (\e -> null $ allParents ! e) [0..(length elist - 1)]

    -- main function
    -- given a maximal antichain, for each lineage in the chain, we can replace that lineage by a set of its children to get another maximal antichain
    -- but there are several caveats: first, this only works if we get rid of parallel edges (see above)
    -- second, we generate some sets which are not antichains, and we need to filter those out
    go :: IntSet -> [IntSet]
    go eset
      | ISet.null eset = []
      | otherwise = let
          childrenLineages = IMap.filterWithKey (\k _ -> k `ISet.member` eset) allChildren
          res = flip concatMap (IMap.toAscList childrenLineages) $ \case
              (_, [])      -> []
              (i, [a])     -> go $ ISet.delete i $ ISet.insert a eset
              (i, [a,b])   -> go $ ISet.delete i $ ISet.insert a $ ISet.insert b eset
              (i, [a,b,c]) -> go $ ISet.delete i $ ISet.insert a $ ISet.insert b $ ISet.insert c eset
              _            -> error "maximalAntichains: unreachable!"
        in eset : res

    allDescendents = IMap.fromList $ zip [0..] $ flip map elist $ \e ->
      let cs = childEdges e elist
      in ISet.fromList . concat $ cs : map (\c -> (elist !! c) `childEdges` elist) cs

    -- self-explanatory
    isDesc :: Int -> Int -> Bool
    isDesc i j = i `ISet.member` (allDescendents ! j)

    hasComparableLineages :: IntSet -> Bool
    hasComparableLineages eset = any (\(i,j) -> i == j || isDesc i j || isDesc j i) [(i,j) | i <- ISet.toList eset, j <- ISet.toList eset, i /= j]

    -- finally we get the antichains
    antichains :: [(IntSet, Int)]
    antichains = zipWith (curry swap) [0..] . filter (not . hasComparableLineages) . nub $ go rootEdges

    -- now we compute the "edges" in the lattice of maximal antichains
    -- if we were working with trees, this could have been done together with generating antichains (ancestral configurations)
    -- now, however, my way of generating antichains is stupid, and i need to compute the coverings separately by checking each pair of antichains

    coverings = concatMap getEdgesForAntichain antichains

    getEdgesForAntichain (eset, idx) =
      -- as above, gen potential children lineages, and then filter out the ones that are not antichains in the list above
      -- the difference is that we are also keeping track of antichain indices
      let childrenLineages = IMap.filterWithKey (\k _ -> k `ISet.member` eset) allChildren
          candidates = flip concatMap (IMap.toAscList childrenLineages) $ \case
              (_, [])      -> []
              (i, [a])     -> [ISet.delete i $ ISet.insert a eset]
              (i, [a,b])   -> [ISet.delete i $ ISet.insert a $ ISet.insert b eset]
              (i, [a,b,c]) -> [ISet.delete i $ ISet.insert a $ ISet.insert b $ ISet.insert c eset]
              _            -> error "maximalAntichains: unreachable!"
        in [(fromJust $ lookup c antichains, idx) | c <- candidates, c `elem` map fst antichains]

antichainLatticeGraph :: RTCN -> G.Gr Int ()
antichainLatticeGraph net = mkGraph nodes edges
  where
    (antichains, coverings) = maximalAntichains net
    nodes = map (\(_, i) -> (i,i)) antichains
    edges = [(i, j, ()) | (i,j) <- coverings]
