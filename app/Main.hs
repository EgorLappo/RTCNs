{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text          as T
import qualified Data.Text.IO       as T

import qualified Data.IntMap.Strict as IMap

import           TCN

maxN :: Int
maxN = 7

maxK::Int
maxK = 5

main :: IO ()
main = do
  let nets = generateOCTCNs
  sequence_ $ flip map nets $ \(n, k, nets) ->
    (let netStr = T.unlines $ T.pack . show . IMap.toList <$> nets
    in do
      T.writeFile ("rpns/one-component-tcn-topologies/" ++ show n ++ "_" ++ show k ++ ".txt") netStr
      T.putStrLn $ T.pack $ show n ++ ", " ++ show k)

  let normalNets = generateNormalNets nets
  sequence_ $ flip map normalNets $ \(n, k, nets) ->
    (let netStr = T.unlines $ T.pack . show . IMap.toList <$> nets
    in do
      T.writeFile ("rpns/one-component-normal-network-topologies/" ++ show n ++ "_" ++ show k ++ ".txt") netStr
      T.putStrLn $ T.pack $ show n ++ ", " ++ show k)

  let reducedNets = generateReducedNets nets
  sequence_ $ flip map reducedNets $ \(n, k, nets) ->
    (let netStr = T.unlines $ T.pack . show . IMap.toList <$> nets
    in do
      T.writeFile ("rpns/one-component-reduced-tcn-topologies/" ++ show n ++ "_" ++ show k ++ ".txt") netStr
      T.putStrLn $ T.pack $ show n ++ ", " ++ show k)

  let reducedNormalNets = generateReducedNets normalNets
  sequence_ $ flip map reducedNormalNets $ \(n, k, nets) ->
    (let netStr = T.unlines $ T.pack . show . IMap.toList <$> nets
    in do
      T.writeFile ("rpns/one-component-reduced-normal-network-topologies/" ++ show n ++ "_" ++ show k ++ ".txt") netStr
      T.putStrLn $ T.pack $ show n ++ ", " ++ show k)

  let netCounts = flip map normalNets $ \(n, k, nets) -> (n, k, length nets)
  let netCountStr = "n,k,count\n" <> T.unlines ((\(n, k, c) -> T.pack $ show n ++ ", " ++ show k ++ ", " ++ show c) <$> netCounts)
  T.writeFile "rpns/one-component-normal-network-topologies/counts.csv" netCountStr

  let normalNetCounts = flip map nets $ \(n, k, nets) -> (n, k, length nets)
  let normalNetCountStr = "n,k,count\n" <> T.unlines ((\(n, k, c) -> T.pack $ show n ++ ", " ++ show k ++ ", " ++ show c) <$> normalNetCounts)
  T.writeFile "rpns/one-component-tcn-topologies/counts.csv" normalNetCountStr

  let reducedNetCounts = flip map reducedNets $ \(n, k, nets) -> (n, k, length nets)
  let reducedNetCountStr = "n,k,count\n" <> T.unlines ((\(n, k, c) -> T.pack $ show n ++ ", " ++ show k ++ ", " ++ show c) <$> reducedNetCounts)
  T.writeFile "rpns/one-component-reduced-tcn-topologies/counts.csv" reducedNetCountStr

  let reducedNormalNetCounts = flip map reducedNormalNets $ \(n, k, nets) -> (n, k, length nets)
  let reducedNormalNetCountStr = "n,k,count\n" <> T.unlines ((\(n, k, c) -> T.pack $ show n ++ ", " ++ show k ++ ", " ++ show c) <$> reducedNormalNetCounts)
  T.writeFile "rpns/one-component-reduced-normal-network-topologies/counts.csv" reducedNormalNetCountStr


generateOCTCNs :: [(Int, Int, [RPN])]
generateOCTCNs = let
        diagonals = [(n',maxK) | n' <- [(maxK+1)..maxN]] ++ [(maxN, k') | k' <- [0..(maxK-1)]]
        trees = unlabeledTreesList maxN
    in flip concatMap diagonals $ \(n,k) ->
        map (\i -> (n-k+i,i, ocTCNTopListWithTrees trees n k !! i)) [0..k]

generateNormalNets :: [(Int, Int, [RPN])] -> [(Int, Int, [RPN])]
generateNormalNets = map (\(n,k,nets) -> (n,k,filter isNormal nets))

generateReducedNets :: [(Int, Int, [RPN])] -> [(Int, Int, [RPN])]
generateReducedNets = map (\(n,k,nets) -> (n,k,filter isReduced nets))
