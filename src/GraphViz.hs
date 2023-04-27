{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module GraphViz where

import           Data.Graph.Inductive
import           Data.GraphViz

import           RTCN

rtcnDraw :: RTCN -> String -> IO ()
rtcnDraw rtcn name = let
    dotGraph = graphToDot quickParams $ emap (const "") $ rtcnToGraph rtcn
  in do
    f <- runGraphviz dotGraph Pdf name
    putStrLn $ "Wrote " ++ f

drawAll:: FilePath -> Int -> Int -> IO ()
drawAll folder n r = sequence_ $ do
  i <- [3..n]
  j <- [0..(min r (i-1))]
  (l, net) <- zip [1..] $ rtcns i j
  return $ rtcnDraw net $ folder <> "/" <> show i <> "_" <> show j <> "_" <> show l <> ".pdf"

