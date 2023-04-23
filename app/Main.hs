{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main (main) where

import qualified Data.Text          as T
import qualified Data.Text.IO       as T

import qualified Data.IntMap.Strict as IMap

import           RPN

maxN :: Int
maxN = 4

maxK::Int
maxK = 3

main :: IO ()
main = do
  let nets = generateOCTCNs
  sequence_ $ flip map nets $ \(n, k, nets) ->
    (let netStr = T.unlines $ T.pack . show . IMap.toList <$> nets
    in do
      T.writeFile ("rpns/one-component-tcn-topologies/" ++ show n ++ "_" ++ show k ++ ".txt") netStr
      T.putStrLn $ T.pack $ show n ++ "_" ++ show k ++ ".txt")

  let normalNets = generateNormalNets nets
  sequence_ $ flip map normalNets $ \(n, k, nets) ->
    let netStr = T.unlines $ T.pack . show . IMap.toList <$> nets
    in do
      T.writeFile ("rpns/one-component-normal-network-topologies/" ++ show n ++ "_" ++ show k ++ ".txt") netStr
      T.putStrLn $ T.pack $ show n ++ "_" ++ show k ++ ".txt"

generateOCTCNs :: [(Int, Int, [RPN])]
generateOCTCNs = let
        diagonals = [(n',maxK) | n' <- [(maxK+1)..maxN]] ++ [(maxN, k') | k' <- [0..(maxK-1)]]
        trees = unlabeledTreesList maxN
    in flip concatMap diagonals $ \(n,k) ->
        map (\i -> (n-k+i,i, ocTCNTopListWithTrees trees n k !! i)) [0..k]

generateNormalNets :: [(Int, Int, [RPN])] -> [(Int, Int, [RPN])]
generateNormalNets = map (\(n,k,nets) -> (n,k,filter isNormal nets))
