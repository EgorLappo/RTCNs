{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module GraphViz where

import           Data.Graph.Inductive              (DynGraph, emap)
import           Data.GraphViz                     (GraphvizOutput (Pdf),
                                                    graphToDot,
                                                    nonClusteredParams,
                                                    quickParams, runGraphviz)

import qualified Data.GraphViz                     as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types               as G

import           RTCN

rtcnDraw :: RTCN -> String -> IO ()
rtcnDraw rtcn name = let
    dotGraph = graphToDot quickParams $ emap (const "") $ rtcnToGraph rtcn
  in do
    f <- runGraphviz dotGraph Pdf name
    putStrLn $ "Wrote " ++ f

latticeDraw :: DynGraph g => g Int () -> String -> IO ()
latticeDraw lattice name = let
  graphParams = nonClusteredParams {
    G.globalAttributes = [
      G.NodeAttrs [
        G.Shape G.Circle,
        G.Style [G.SItem G.Filled []],
        G.fillColor G.Purple,
        G.FontSize 8,
        G.toLabel ""
      ]
    ],
    G.fmtEdge = const []
  }
  dotGraph = graphToDot graphParams $ emap (const "") lattice
  in do
    f <- runGraphviz dotGraph Pdf name
    putStrLn $ "Wrote " ++ f

drawAllRtcnTopologies :: FilePath -> Int -> Int -> IO ()
drawAllRtcnTopologies folder n r = sequence_ $ do
  i <- [3..n]
  j <- [0..(min r (i-2))]
  (l, net) <- zip [1..] $ rtcnTopologies i j
  return $ rtcnDraw net $ folder <> "/" <> show i <> "_" <> show j <> "_" <> show l <> ".pdf"

drawAllRtcnTopologiesAndLattices :: FilePath -> Int -> Int -> IO ()
drawAllRtcnTopologiesAndLattices folder n r = sequence_ $ do
  i <- [3..n]
  j <- [0..(min r (i-2))]
  (l, net) <- zip [1..] $ rtcnTopologies i j
  let lat = antichainLatticeGraph net
  return $ do
    rtcnDraw net $ folder <> "/" <> show i <> "_" <> show j <> "_" <> show l <> ".pdf"
    latticeDraw lat $ folder <> "/" <> show i <> "_" <> show j <> "_" <> show l <> "_lattice.pdf"
