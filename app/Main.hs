{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main (main) where

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           GraphViz
import           RTCN

main :: IO ()
main = do
  drawAllRtcnTopologiesAndLattices "rtcn-pics" 7 5

