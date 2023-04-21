module Lib where
-- code related to ranked tree-child network
-- an attempt to start a codebase that can help work with phylogenetic network maths

import           Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IMap

-- non-recursive type for nodes in adjacency lists
-- allows us to use IntMap (Node Int) as a main type, rather than [[Int]]
data Node' a
  = TreeNode a a    -- left child label, right child label
  | SubdivNode a    -- child label
  | RetNode a       -- child label
  | LeafNode
    deriving (Show, Eq)

type Node = Node' Int

-- an adjacency list encoding of a rooted phylogenetic network
type RPN = IntMap Node

-- an RPN is a tree when it has no subdiv and no ret nodes
tree :: RPN -> Bool
tree = all (\n -> case n of
    TreeNode _ _ -> True
    LeafNode     -> True
    _            -> False)

-- an RPN is a tree-child network when children of RetNodes are not RetNodes
tcn :: RPN -> Bool
tcn d = all (\(RetNode i) -> case d ! i of
    RetNode _ -> False
    _         -> True) . retNodes $ d

-- an RPN is one-component when children of RetNodes are LeafNodes
oneComponent :: RPN -> Bool
oneComponent d = all (\(RetNode i) -> case d ! i of
    LeafNode -> True
    _        -> False) . retNodes $ d

-- two nodes are comparable if one is a descendant of the other
comparable :: RPN -> Int -> Int -> Bool
comparable d i j = i `elem` (descendants d j) || j `elem` (descendants d i)

-- two nodes are equivalent if they can be interchanged by a symmetry of the directed acyclic graph
equivalent :: RPN -> Int -> Int -> Bool
equivalent d i j = undefined



-- helper functions

labels :: RPN -> [Int]
labels = IMap.keys


-- get all descendants of a node
descendants :: RPN -> Int -> [Int]
descendants d i = case d ! i of
    TreeNode l r -> l : r : descendants d l ++ descendants d r
    SubdivNode c -> c : descendants d c
    RetNode c    -> c : descendants d c
    LeafNode     -> []

-- get the number of leaves
nLeaves :: RPN -> Int
nLeaves = length . leafNodes

-- get the number of reticulations
nReticulations :: RPN -> Int
nReticulations = length . retNodes

retNodes = IMap.filter (\n -> case n of
    RetNode _ -> True
    _         -> False)

subdivNodes = IMap.filter (\n -> case n of
    SubdivNode _ -> True
    _            -> False)

leafNodes = IMap.filter (\n -> case n of
    LeafNode -> True
    _        -> False)

treeNodes = IMap.filter (\n -> case n of
    TreeNode _ _ -> True
    _            -> False)
