{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module RPN (RPN, Node, Node'(..), ocNormalNetworks, ocTCNList, ocTCNListWithTrees, ocTCNs, ocTCNTops, ocTCNTopList, ocTCNTopListWithTrees, planeTrees, trees, treesList, unlabeledTrees, unlabeledTreesList, keepNonIsomorphic, isIsomorphic, keepReduced, isReduced, comparable, tcn, isNormal) where
-- code for RPNs (rooted phylogenetic networks)
-- an attempt to start a codebase that can help work with phylogenetic network maths

import           Control.Monad.State.Strict
import           Data.IntMap.Strict         (IntMap, (!))
import           Data.List                  (intercalate, tails)

import qualified Data.Array                 as A
import qualified Data.IntMap.Strict         as IMap
import qualified Data.IntSet                as ISet

--- DEBUG CODE: very useful, can be used with `seq` as
-- `seq (dbg x) y` to print out the value of x and return y
import           System.IO.Unsafe           (unsafePerformIO)

dbg :: Show a => a -> a
dbg a = unsafePerformIO $ do
  print a
  return a

-- non-recursive type for nodes in adjacency lists
-- allows us to use IntMap (Node Int) as a main type, rather than [[Int]]
data Node' a
  = TreeNode a a    -- left child label, right child label
  | SubdivNode a    -- child label
  | RetNode a       -- child label
  | LeafNode
    deriving (Show, Eq)

instance Functor Node' where
  fmap f (TreeNode l r) = TreeNode (f l) (f r)
  fmap f (SubdivNode c) = SubdivNode (f c)
  fmap f (RetNode c)    = RetNode (f c)
  fmap _ LeafNode       = LeafNode

type Node = Node' Int

-- an adjacency list encoding of a rooted phylogenetic network
type RPN = IntMap Node

type AdjacencyMatrix = A.Array (Int, Int) Bool

type Isomorphism = [(Int, Int)]

-- ********************** generating networks **********************

ocNormalNetworks :: Int -> Int -> [RPN]
ocNormalNetworks n k = filter isNormal $ ocTCNs n k

isNormal :: RPN -> Bool
isNormal net = all (\(i, node) -> case node of
          RetNode _ -> let [p1, p2] = parents net i in not $ comparable net p1 p2
          _         -> True) $ IMap.toList net

-- almost Theorem 13 in https://doi.org/10.1016/j.jcss.2020.06.001
-- but without the (n choose k) factor
ocTCNs' :: Int -> Int -> [RPN]
ocTCNs' = ocTCNsFromTrees trees

-- main function, with f being the tree-generation function
-- unmemoized
ocTCNsFromTrees :: (Int -> [RPN]) -> Int -> Int -> [RPN]
ocTCNsFromTrees f n k
  | n < 0 || k < 0 = error "ocTCNs: n and k must be non-negative"
  | n == 0 = [IMap.empty]
  | n == 1 = [IMap.fromList [(1, LeafNode)]]
  | k == 0 = f n
  | k > n - 1 = error "ocTCNs: k must be less than n - 1"
  | otherwise = let
      -- we will insert a new reticulation node whose child is a leaf into each of the ocTCNs with n-1 leaves and k-1 reticulations
      nets = ocTCNsFromTrees f (n-1) (k-1)
      -- possible locations of where to insert a reticulation node
      pss = pairs . labels . filterLeaves  <$> nets
    in concatMap (\(net, ps) -> concatMap (insertReticulation net) ps) $ zip nets pss
  where
    -- number of nodes in each source network is the same
    -- proposition 1 in https://doi.org/10.1016/j.jcss.2020.06.001
    nNodes = n-1 + (k-1) + (n + k - 3)

    -- all pairs of labels in a network (n + n choose 2)
    pairs l = zip l l ++ [(x,y) | (x:ys) <- tails l, y <- ys]

    -- filter out the leaves that are children of reticulation nodes
    filterLeaves :: RPN -> RPN
    filterLeaves net = IMap.filterWithKey (\key _ ->
      isRoot net key || not (isLeaf net key) || not (isRetNode net $ head $ parents net key)) net

    insertReticulation :: RPN -> (Int, Int) -> [RPN]
    insertReticulation net (i, j) = do
      afterOne <- insertAbove net i (nNodes + 1) (nNodes + 3)
      afterTwo <- insertAbove afterOne j (nNodes + 2) (nNodes + 3)
      return $ IMap.union afterTwo (IMap.fromList [(nNodes + 3, RetNode (nNodes + 4)), (nNodes + 4, LeafNode)])

ocTCNs :: Int -> Int -> [RPN]
ocTCNs n k = ocTCNList n k !! k

ocTCNList :: Int -> Int -> [[RPN]]
ocTCNList n = ocTCNListWithTrees (treesList n) n

ocTCNListWithTrees :: [[RPN]] -> Int -> Int -> [[RPN]]
ocTCNListWithTrees ts n k =
-- the key here is that we only refer to the (n-1) (k-1) case in the recursion, so we only need to memoize the "diagonal"
  let recNets = [ocTCNsMemo ts recNets (n-k+i) i  | i <- [0..k]] in recNets

ocTCNsMemo :: [[RPN]] -> [[RPN]] -> Int -> Int -> [RPN]
ocTCNsMemo ts recNets n k
  | n < 0 || k < 0 = error "ocTCNs: n and k must be non-negative"
  | n == 0 = [IMap.empty]
  | n == 1 = [IMap.fromList [(1, LeafNode)]]
  | k == 0 = ts !! (n - 1)
  | k > n - 1 = error "ocTCNs: k must be less than n - 1"
  | otherwise =  let
        nets = recNets !! (k-1)
        pss = pairs . labels . filterLeaves  <$> nets
      in concatMap (\(net, ps) -> concatMap (insertReticulation net) ps) $ zip nets pss
    where
      nNodes = n-1 + (k-1) + (n + k - 3)
      pairs l = zip l l ++ [(x,y) | (x:ys) <- tails l, y <- ys]

      filterLeaves :: RPN -> RPN
      filterLeaves net = IMap.filterWithKey (\key _ ->
        isRoot net key || not (isLeaf net key) || not (isRetNode net $ head $ parents net key)) net

      insertReticulation :: RPN -> (Int, Int) -> [RPN]
      insertReticulation net (i, j) = do
        afterOne <- insertAbove net i (nNodes + 1) (nNodes + 3)
        afterTwo <- insertAbove afterOne j (nNodes + 2) (nNodes + 3)
        return $ IMap.union afterTwo (IMap.fromList [(nNodes + 3, RetNode (nNodes + 4)), (nNodes + 4, LeafNode)])

-- generates nonisomorphic one-component TCNs (i.e. topologies) with n leaves and k reticulations
ocTCNTops' :: Int -> Int -> [RPN]
ocTCNTops' n k
  | n < 0 || k < 0 = error "ocTCNs: n and k must be non-negative"
  | n == 0 = [IMap.empty]
  | n == 1 = [IMap.fromList [(1, LeafNode)]]
  | k == 0 = unlabeledTrees n
  | k > n - 1 = error "ocTCNs: k must be less than n - 1"
  | otherwise = let
      -- we will insert a new reticulation node whose child is a leaf into each of the ocTCNs with n-1 leaves and k-1 reticulations
      nets = ocTCNTops' (n-1) (k-1)
      -- possible locations of where to insert a reticulation node
      pss = pairs . labels . filterLeaves  <$> nets
    in keepNonIsomorphic $ concatMap (\(net, ps) -> concatMap (insertReticulation net) ps) $ zip nets pss
  where
    -- number of nodes in each source network is the same
    -- proposition 1 in https://doi.org/10.1016/j.jcss.2020.06.001
    nNodes = n-1 + (k-1) + (n + k - 3)

    -- all pairs of labels in a network (n + n choose 2)
    pairs l = zip l l ++ [(x,y) | (x:ys) <- tails l, y <- ys]

    -- filter out the leaves that are children of reticulation nodes
    filterLeaves :: RPN -> RPN
    filterLeaves net = IMap.filterWithKey (\key _ ->
      isRoot net key || not (isLeaf net key) || not (isRetNode net $ head $ parents net key)) net

    insertReticulation :: RPN -> (Int, Int) -> [RPN]
    insertReticulation net (i, j) = do
      afterOne <- insertAbove net i (nNodes + 1) (nNodes + 3)
      afterTwo <- insertAbove afterOne j (nNodes + 2) (nNodes + 3)
      return $ IMap.union afterTwo (IMap.fromList [(nNodes + 3, RetNode (nNodes + 4)), (nNodes + 4, LeafNode)])

ocTCNTops :: Int -> Int -> [RPN]
ocTCNTops n k = ocTCNTopList n k !! k

ocTCNTopList :: Int -> Int -> [[RPN]]
ocTCNTopList n = ocTCNTopListWithTrees (unlabeledTreesList n) n

ocTCNTopListWithTrees :: [[RPN]] -> Int -> Int -> [[RPN]]
ocTCNTopListWithTrees ts n k =
  let recNets = [ocTCNTopsMemo ts recNets (n-k+i) i  | i <- [0..k]] in recNets

ocTCNTopsMemo :: [[RPN]] -> [[RPN]] -> Int -> Int -> [RPN]
ocTCNTopsMemo ts recNets n k
  | n < 0 || k < 0 = error "ocTCNs: n and k must be non-negative"
  | n == 0 = [IMap.empty]
  | n == 1 = [IMap.fromList [(1, LeafNode)]]
  | k == 0 = ts !! (n - 1)
  | k > n - 1 = error "ocTCNs: k must be less than n - 1"
  | otherwise =  let
        nets = recNets !! (k-1)
        pss = pairs . labels . filterLeaves  <$> nets
      in keepNonIsomorphic $ concatMap (\(net, ps) -> concatMap (insertReticulation net) ps) $ zip nets pss
    where
      nNodes = n-1 + (k-1) + (n + k - 3)
      pairs l = zip l l ++ [(x,y) | (x:ys) <- tails l, y <- ys]

      filterLeaves :: RPN -> RPN
      filterLeaves net = IMap.filterWithKey (\key _ ->
        isRoot net key || not (isLeaf net key) || not (isRetNode net $ head $ parents net key)) net

      insertReticulation :: RPN -> (Int, Int) -> [RPN]
      insertReticulation net (i, j) = do
        afterOne <- insertAbove net i (nNodes + 1) (nNodes + 3)
        afterTwo <- insertAbove afterOne j (nNodes + 2) (nNodes + 3)
        return $ IMap.union afterTwo (IMap.fromList [(nNodes + 3, RetNode (nNodes + 4)), (nNodes + 4, LeafNode)])

-- insert a tree node with a specified label above a given node
-- if we are inserting above the root, we need to create a new root
-- if we are inserting above the reticulation node, we have two possibilities
-- labels are taken care of by the caller
insertAbove :: RPN -> Int -> Int -> Int -> [RPN]
insertAbove net i treeNodeLabel retNodeLabel
  | isRoot net i = [IMap.insert treeNodeLabel (TreeNode i retNodeLabel) net]
  | isRetNode net i = let
      [parentLeft, parentRight] = parents net i
      (TreeNode plLeftChild plRightChild) =  net ! parentLeft
      (TreeNode prLeftChild prRightChild) =  net ! parentRight
      newTCNLeft = if plLeftChild == i
        then IMap.insert parentLeft (TreeNode treeNodeLabel plRightChild) net
        else IMap.insert parentLeft (TreeNode plLeftChild treeNodeLabel) net
      newTCNRight = if prLeftChild == i
        then IMap.insert parentRight (TreeNode treeNodeLabel prRightChild) net
        else IMap.insert parentRight (TreeNode prLeftChild treeNodeLabel) net
    in IMap.insert treeNodeLabel (TreeNode i retNodeLabel) <$> [newTCNLeft, newTCNRight]
  | otherwise = let
      [parent] = parents net i
      (TreeNode leftChild rightChild) = net ! parent
      newTCN = if leftChild == i
        then IMap.insert parent (TreeNode treeNodeLabel rightChild) net
        else IMap.insert parent (TreeNode leftChild treeNodeLabel) net
    in [IMap.insert treeNodeLabel (TreeNode i retNodeLabel) newTCN]

-- ********************** generating trees **********************

-- labeled trees with n leaves
-- map (length.trees) [1..] = [1,1,3,15,105,945,10395,..] = (2n - 3)!!
trees' :: Int -> [RPN]
trees' n
  | n < 0 = error "trees: n must be non-negative"
  | n == 0 = [IMap.empty]
  | n == 1 = [IMap.fromList [(1, LeafNode)]]
  | otherwise = concatMap (\t -> insertAbove' t <$> labels t) $ trees' (n-1)
  where
    insertAbove' t i
      | isRoot t i = IMap.union (IMap.fromList [(2*n-2, LeafNode), (2*n-1, TreeNode i (2*n-2))]) t
      | otherwise =
        let p = head (parents t i)
            (TreeNode l r) = t ! p
        in if l == i
              then IMap.union (IMap.fromList [(2*n-2, LeafNode), (2*n-1, TreeNode i (2*n-2)), (p, TreeNode (2*n-1) r)]) t
              else IMap.union (IMap.fromList [(2*n-2, LeafNode), (2*n-1, TreeNode (2*n-2) i), (p, TreeNode l (2*n-1))]) t

-- this is an appropriate memoized version of trees'
trees :: Int -> [RPN]
trees n = treesList n !! (n-1)

treesList :: Int -> [[RPN]]
treesList nmax =
  let ts = [treesMemo ts n | n <- [1..nmax]]
  in ts

treesMemo :: [[RPN]] -> Int -> [RPN]
treesMemo ts n
  | n < 0 = error "trees: n must be non-negative"
  | n == 0 = [IMap.empty]
  | n == 1 = [IMap.fromList [(1, LeafNode)]]
  | otherwise = concatMap (\t -> insertAbove' t <$> labels t) $ ts !! (n-2)
  where
    insertAbove' t i
      | isRoot t i = IMap.union (IMap.fromList [(2*n-2, LeafNode), (2*n-1, TreeNode i (2*n-2))]) t
      | otherwise =
        let p = head (parents t i)
            (TreeNode l r) = t ! p
        in if l == i
              then IMap.union (IMap.fromList [(2*n-2, LeafNode), (2*n-1, TreeNode i (2*n-2)), (p, TreeNode (2*n-1) r)]) t
              else IMap.union (IMap.fromList [(2*n-2, LeafNode), (2*n-1, TreeNode (2*n-2) i), (p, TreeNode l (2*n-1))]) t

-- labeled trees with n leaves
-- map (length.planeTrees) [1..] = 1,1,2,5,14,42,132,429,1430,4862 = C_{n-1}, Catalan numbers
planeTrees :: Int -> [RPN]
planeTrees n
  | n < 0 = error "planeTrees: n must be non-negative"
  | n == 0 = [IMap.empty]
  | n == 1 = [IMap.fromList [(1, LeafNode)]]
  | otherwise = do
      i <- [1..n-1]
      l <- planeTrees i
      r <- planeTrees (n-i)
      return $ IMap.insert (2*n-1) (TreeNode (2*i-1) (2*n-2)) $ IMap.union l (incrementLabelsBy (2*i - 1) r)
  where incrementLabelsBy i = fmap (fmap (+i)) . IMap.mapKeys (+i)

-- tree topologies with n leaves
-- map (length.unlabeledTrees) [1..] = [1,1,1,2,3,6,11,23,46,98,207,451,...]
unlabeledTrees' :: Int -> [RPN]
unlabeledTrees' n
  | n < 0 = error "unlabeledTrees: n must be non-negative"
  | n == 0 = [IMap.empty]
  | n == 1 = [IMap.fromList [(1, LeafNode)]]
  | n == 2 = [IMap.fromList [(1,LeafNode),(2,LeafNode),(3,TreeNode 1 2)]]
  | n == 3 = [IMap.fromList [(1,LeafNode),(2,LeafNode),(3,LeafNode),(4,TreeNode 2 3),(5,TreeNode 1 4)]]
  | even n =
      let
        i = (n - 2) `div` 2
        unbalancedTrees = do
          k <- [1..i]
          l <- unlabeledTrees' k
          r <- unlabeledTrees' (n-k)
          return $ IMap.insert (2*n-1) (TreeNode (2*k-1) (2*n-2)) $ IMap.union l (incrementLabelsBy (2*k - 1) r)

        halfTrees = unlabeledTrees' (n `div` 2)
        balancedTrees = do
          (l, r) <- pairs halfTrees
          return $ IMap.insert (2*n-1) (TreeNode (2*(n `div` 2)-1) (2*n-2)) $ IMap.union l (incrementLabelsBy (2*(n `div` 2)-1) r)
      in unbalancedTrees ++ balancedTrees
  | otherwise =
      let
        i = (n - 1) `div` 2
      in do
        k <- [1..i]
        l <- unlabeledTrees' k
        r <- unlabeledTrees' (n-k)
        return $ IMap.insert (2*n-1) (TreeNode (2*k-1) (2*n-2)) $ IMap.union l (incrementLabelsBy (2*k - 1) r)
  where incrementLabelsBy i = fmap (fmap (+i)) . IMap.mapKeys (+i)
        pairs l = zip l l ++ [(x,y) | (x:ys) <- tails l, y <- ys]

-- this is an appropriate memoized version of unlabeledTrees'
unlabeledTrees :: Int -> [RPN]
unlabeledTrees n = unlabeledTreesList n !! (n-1)

unlabeledTreesList :: Int -> [[RPN]]
unlabeledTreesList nmax =
  let ts = [unlabeledTreesMemo ts n | n <- [1..nmax]]
  in ts

unlabeledTreesMemo :: [[RPN]] -> Int -> [RPN]
unlabeledTreesMemo ts n
  | n < 0 = error "unlabeledTrees: n must be non-negative"
  | n == 0 = [IMap.empty]
  | n == 1 = [IMap.fromList [(1, LeafNode)]]
  | n == 2 = [IMap.fromList [(1,LeafNode),(2,LeafNode),(3,TreeNode 1 2)]]
  | n == 3 = [IMap.fromList [(1,LeafNode),(2,LeafNode),(3,LeafNode),(4,TreeNode 2 3),(5,TreeNode 1 4)]]
  | even n =
      let
        i = (n - 2) `div` 2
        unbalancedTrees = do
          k <- [1..i]
          l <- ts !! (k-1)
          r <- ts !! (n-k-1)
          return $ IMap.insert (2*n-1) (TreeNode (2*k-1) (2*n-2)) $ IMap.union l (incrementLabelsBy (2*k - 1) r)

        halfTrees = ts !! ((n `div` 2) - 1)
        balancedTrees = do
          (l, r) <- pairs halfTrees
          return $ IMap.insert (2*n-1) (TreeNode (2*(n `div` 2)-1) (2*n-2)) $ IMap.union l (incrementLabelsBy (2*(n `div` 2)-1) r)
      in unbalancedTrees ++ balancedTrees
  | otherwise =
      let
        i = (n - 1) `div` 2
      in do
        k <- [1..i]
        l <- ts !! (k-1)
        r <- ts !! (n-k-1)
        return $ IMap.insert (2*n-1) (TreeNode (2*k-1) (2*n-2)) $ IMap.union l (incrementLabelsBy (2*k - 1) r)
  where incrementLabelsBy i = fmap (fmap (+i)) . IMap.mapKeys (+i)
        pairs l = zip l l ++ [(x,y) | (x:ys) <- tails l, y <- ys]


-- ********************** RPN isomorphism **********************

isIsomorphic :: RPN -> RPN -> Bool
isIsomorphic net1 net2 = tryMatch net1 net2 (root net1) (root net2)

tryMatch :: RPN -> RPN -> Int -> Int -> Bool
tryMatch net1 net2 v1 v2
  | isLeaf net1 v1 && isLeaf net2 v2 = True
  | not $ sameNodeType (net1 ! v1) (net2 ! v2) = False
  | otherwise = case (net1 ! v1, net2 ! v2) of
    (TreeNode l1 r1, TreeNode l2 r2) -> tryMatch net1 net2 l1 l2 && tryMatch net1 net2 r1 r2 || tryMatch net1 net2 l1 r2 && tryMatch net1 net2 r1 l2
    (RetNode i1, RetNode i2) -> tryMatch net1 net2 i1 i2
    _ -> error "tryMatch: please don't compute isomorphisms with subdivision edges present in the RPNs"

keepNonIsomorphic :: [RPN] -> [RPN]
keepNonIsomorphic [] = []
keepNonIsomorphic (x:xs) = x : keepNonIsomorphic (filter (not . isIsomorphic x) xs)

-- ********************** useful predicates **********************

-- an RPN is a tree when it has no subdiv and no ret nodes
tree :: RPN -> Bool
tree = all
  (\case
    TreeNode _ _ -> True
    LeafNode     -> True
    _            -> False)

-- an RPN is a tree-child network when children of RetNodes are not RetNodes
tcn :: RPN -> Bool
tcn d = all (\case
  RetNode i -> (case d ! i of
    RetNode _ -> False
    _         -> True)
  _ -> True) d

-- an RPN is one-component when children of RetNodes are LeafNodes
isoc :: RPN -> Bool
isoc d = all (\case
  RetNode i -> (case d ! i of
    LeafNode -> True
    _        -> False)
  _ -> True) d

-- two nodes are comparable if one is a descendant of the other
comparable :: RPN -> Int -> Int -> Bool
comparable d i j = i `elem` descendants d j || j `elem` descendants d i


-- ********************** helper functions  **********************

keepReduced :: [RPN] -> [RPN]
keepReduced = filter isReduced

isReduced :: RPN -> Bool
-- an RPN is reduced if either it is of size 1 or 2, or if both descendants of the root are not leaves
isReduced d
  | nLeaves d < 3 = True
  | otherwise = case d ! root d of
      TreeNode l r -> not (isLeaf d l || isLeaf d r)
      _            -> False

adjacencyMatrix :: RPN -> AdjacencyMatrix
adjacencyMatrix d = A.array ((1, 1), (n, n)) [((i, j), isChild d i j) | i <- [1..n], j <- [1..n]]
  where n = length d

-- get an RPN from adjacency list
-- each node can be identified by looking at its children and parents
--   leaf nodes have no children and one parent
--   tree nodes have two children and one parent
--   subdivided nodes have one child and one parent
--   reticulation nodes have one child and two parents
rpnFromMatrix :: AdjacencyMatrix -> RPN
rpnFromMatrix m = IMap.fromList $ zipWith3 (\i cs ps
  -> (case (cs, ps) of
        ([], [_])     -> (i, LeafNode)
        ([l, r], [_]) -> (i, TreeNode l r)
        ([c], [_])    -> (i, SubdivNode c)
        ([c], [_, _]) -> (i, RetNode c)
        _             -> error "adjacency matrix does not represent an RPN")) [1..] childrenList parentsList
  where
    n = snd $ snd $ A.bounds m
    childrenList = fmap (\i -> filter (\j -> m A.! (i, j)) [1..n]) [1..n]
    parentsList = fmap (\j -> filter (\i -> m A.! (i, j)) [1..n]) [1..n]

-- get all descendants of a node
descendants :: RPN -> Int -> [Int]
descendants d i = case d ! i of
  TreeNode l r -> l : r : descendants d l ++ descendants d r
  SubdivNode c -> c : descendants d c
  RetNode c    -> c : descendants d c
  LeafNode     -> []

-- get all descendants of a node as a sub-RPN
descendantRPN :: RPN -> Int -> RPN
descendantRPN d i = IMap.filterWithKey (\k _ -> k `elem` descendants d i) d

-- get all children of a node
children :: RPN -> Int -> [Int]
children d i = case d ! i of
  TreeNode l r -> [l, r]
  SubdivNode c -> [c]
  RetNode c    -> [c]
  LeafNode     -> []

-- check if a node is a child of another node
isChild :: RPN -> Int -> Int -> Bool
isChild d i j = j `elem` children d i

-- get all ancestors of a node
ancestors :: RPN -> Int -> [Int]
ancestors d i = filter (\n -> isChild d n i) (labels d)

-- get all parents of a node
parents :: RPN -> Int -> [Int]
parents d i = filter (\j -> isChild d j i) $ ancestors d i

root :: RPN -> Int
root d = head $ filter (isRoot d) (labels d)

isRoot :: RPN -> Int -> Bool
isRoot d i = null $ parents d i

-- get the number of leaves
nLeaves :: RPN -> Int
nLeaves = length . leafNodes

-- get all labels of nodes in an RPN
labels :: RPN -> [Int]
labels = IMap.keys

-- get the number of reticulations
nReticulations :: RPN -> Int
nReticulations = length . retNodes

retNodes :: RPN -> RPN
retNodes = IMap.filter
  (\case
    RetNode _ -> True
    _         -> False)

subdivNodes :: RPN -> RPN
subdivNodes = IMap.filter
  (\case
    SubdivNode _ -> True
    _            -> False)

leafNodes :: RPN -> RPN
leafNodes = IMap.filter
  (\case
    LeafNode -> True
    _        -> False)

treeNodes :: RPN -> RPN
treeNodes = IMap.filter
  (\case
    TreeNode _ _ -> True
    _            -> False)

isRetNode :: RPN -> Int -> Bool
isRetNode d i = case d ! i of
  RetNode _ -> True
  _         -> False

isLeaf :: RPN -> Int -> Bool
isLeaf d i = case d ! i of
  LeafNode -> True
  _        -> False

isTreeNode :: RPN -> Int -> Bool
isTreeNode d i = case d ! i of
  TreeNode _ _ -> True
  _            -> False

isSubdivNode :: RPN -> Int -> Bool
isSubdivNode d i = case d ! i of
  SubdivNode _ -> True
  _            -> False

sameNodeType :: Node -> Node -> Bool
sameNodeType (RetNode _) (RetNode _)       = True
sameNodeType LeafNode LeafNode             = True
sameNodeType (TreeNode _ _) (TreeNode _ _) = True
sameNodeType (SubdivNode _) (SubdivNode _) = True
sameNodeType _ _                           = False


relabelTree :: RPN -> RPN
relabelTree t
  | not (tree t) = error "relabelTree: input is not a tree"
  | otherwise = snd $ execState (go t (root t)) (0, IMap.empty)
  where go :: RPN -> Int -> State (Int, RPN) ()
        go t' i = case t' ! i of
          LeafNode -> do
            (n, m) <- get
            put (n+1, IMap.insert n LeafNode m)
          TreeNode l r -> do
            (n, m) <- get
            go t' l
            go t' r
            put (n+1, IMap.insert n LeafNode m)
          _ -> error "relabelTree: unreachable"

relabelRPN :: RPN -> RPN
relabelRPN network = let (_, resultNet, _) = execState (go network (root network)) (0, IMap.empty, ISet.empty)
  in resultNet
  where go :: RPN -> Int -> State (Int, RPN, ISet.IntSet) ()
        go net node = do
          (n, nResult, visited) <- get
          if node `ISet.member` visited
            then return ()
            else do
              put (n+1, IMap.insert n (net ! node) nResult, node `ISet.insert` visited)
              case net ! node of
                LeafNode -> return ()
                TreeNode l r -> do
                  go net l
                  go net r
                SubdivNode c -> go net c
                RetNode c -> go net c

-- ????
collapseSubdivisions :: RPN -> RPN
collapseSubdivisions network =
  let (_, resultNet, _) = execState (go network (root network)) (0, IMap.empty, ISet.empty)
  in resultNet
  where go :: RPN -> Int -> State (Int, RPN, ISet.IntSet) ()
        go net node = do
          (n, nResult, visited) <- get
          if node `ISet.member` visited
            then return ()
            else do
              put (n+1, IMap.insert n (net ! node) nResult, node `ISet.insert` visited)
              case net ! node of
                LeafNode -> return ()
                TreeNode l r -> do
                  go net l
                  go net r
                SubdivNode c -> do
                  go net c
                  let (n', nResult', visited') = execState (go net c) (n, nResult, visited)
                  put (n', IMap.insert n (net ! c) nResult', visited')
                RetNode c -> go net c

printRPN :: RPN -> IO ()
printRPN net =
      let
        x = ((\(i, node) -> case node of
          LeafNode     -> show i ++ ": " ++ "Leaf"
          TreeNode l r -> show i ++ ": " ++ "T " ++ show l ++ " " ++ show r
          SubdivNode c -> show i ++ ": " ++ "S " ++ show c
          RetNode c    -> show i ++ ": " ++ "R " ++ show c) <$> IMap.toList net)
        y = "[" ++ intercalate ", " x ++ "]"
      in print y

-- all partitions of the list xs
-- https://stackoverflow.com/questions/35423903/haskell-all-possible-partitions-of-a-list
partitions :: [a] -> [[[a]]]
partitions []  = [[]]
partitions (x:xs) = do
    yss <- partitions xs
    bloat x yss
  where
    bloat :: a -> [[a]] -> [[[a]]]
    bloat z  []      = [[[z]]]
    bloat z (zs:zss) = ((z:zs):zss) : map (zs:) (bloat z zss)
