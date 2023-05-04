{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           GraphViz
import           RTCN

main :: IO ()
main = let
    n=7
    r = 5
    folder = "rtcn-pics"
  in do
      counts <- sequence $ do
        i <- [3..n]
        j <- [0..(min r (i-2))]
        (l, net) <- zip [1..] $ rtcnTopologies i j
        let (lat, nnodes, npaths) = antichainLatticeGraphData net
        return $ do
          rtcnDraw net $ folder <> "/" <> show i <> "_" <> show j <> "_" <> show l <> ".pdf"
          latticeDraw lat $ folder <> "/" <> show i <> "_" <> show j <> "_" <> show l <> "_lattice.pdf"
          return (i, j, l, nnodes, npaths)
      -- write the tuples in counts to a csv
      T.writeFile (folder <> "/counts.csv") $ T.unlines $ "i,j,l,nodes,paths" : do
        (i, j, l, nnodes, npaths) <- counts
        return $ T.intercalate "," $ map (T.pack.show) [i, j, l, nnodes, npaths]
