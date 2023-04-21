module Lib where
-- code related to ranked tree-child network
-- an attempt to start a codebase that can help work with phylogenetic network maths


import           Data.IntMap.Strict (IntMap)


data Node' a = TreeNode a (Node' a) (Node' a) | SubdivNode a (Node' a) | RetNode a (Node' a) | LeafNode a
    deriving (Show, Eq)

type Node = Node' Int

type DAG = IntMap Node
