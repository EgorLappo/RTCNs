{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module RTCN where

-- first lets implement the backwards construction of RTCNs

-- the idea is that after ith events we have l - i lineages that we can re-label from left to right
-- B is a coalescence of two lineges, and R is a reticulation of three lineages
data Node' a = B a a | R a a a
    deriving (Eq, Show)

type Node = Node' Int

type RTCN = [Node]

data Event = Branching | Reticulation
    deriving (Eq, Show)

type Profile = [Event]

-- for n >= 2 and 0 <= b <= n-1,
-- produce a list of length n-1, with exactly b reticulations
qProfiles :: Int -> Int -> [Profile]
qProfiles n b
    | n < 2 || b <= 0 || b >= n = []
    | n == 2 = [[Branching]]
    | otherwise = fmap (reverse . (Branching :)) (qProfiles' (n-2) (b-1))
  where qProfiles' n' b'
         | n' <= 0 = [[]]
         | b' == 0 = fmap (Reticulation :) (qProfiles' (n'-1) 0)
         | b' == n' = [[Branching | _ <- [1..n']]]
         | otherwise = fmap (Branching :) (qProfiles' (n'-1) (b'-1)) ++ fmap (Reticulation :) (qProfiles' (n'-1) b')

-- now lets generate RTCNs given a profile

rtcnsWithProfile :: Int -> Profile -> [RTCN]
rtcnsWithProfile _ [Branching] = [[B 1 2]]
rtcnsWithProfile n (e:es)
  | e == Branching = [B i j : r | (i,j) <- pairs n, r <- rtcnsWithProfile (n-1) es]
  | e == Reticulation = [R i j k : r | (i,j,k) <- triples n, r <- rtcnsWithProfile (n-1) es]

pairs :: Int -> [(Int, Int)]
pairs n = [(i,j) | i <- [1..n], j <- [1..n], i < j]

triples :: Int -> [(Int, Int, Int)]
triples n = [(i, j, k) | (i,j) <- pairs n, k <- [1..n], k /= i, k /= j]

-- RTCNs with n leaves and b branchings
rtcns' :: Int -> Int -> [RTCN]
rtcns' n b = concat [rtcnsWithProfile n p | p <- qProfiles n b]

-- RTCNs with n leaves and r reticulations
rtcns :: Int -> Int -> [RTCN]
rtcns n r = rtcns' n (n-r-1)
