{-# LANGUAGE FlexibleInstances #-}

module Jet where
import Data.AdditiveGroup
import Data.Ord (comparing)
import Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Maybe
import Data.List
import LorentzVector

type Cluster = Vec4

-- Graph of the clusters
-- Nodes encode the cluster 4-vectors. Edges the dij between clusters.
type ClusterGraph a = Gr a Double

instance (LorentzVector a) => LorentzVector (Node, a) where
    xV (_, w) = xV w
    yV (_, w) = yV w
    zV (_, w) = zV w
    tV (_, w) = tV w


toNodes :: [a] -> [LNode a]
toNodes xs = zip [0,1..] xs

-- the usual distance measures...
dij :: (LorentzVector a, Eq a) => Double -> Int -> a -> a -> Double
dij r p v w = mpt * (d^2)
    where
        d = if v == w then 1 else dRV v w / r
        mpt = if v == w then (ptV v)^^(2*p) else min ((ptV v)^^(2*p)) ((ptV w)^^(2*p))


-- ClusterGraph creation
makeClusterGraph :: (LorentzVector a) => (a -> a -> Double) -> [a] -> (ClusterGraph a)
makeClusterGraph dist vs = mkGraph ns $ edgesDij dist ns
    where ns = toNodes vs


-- edge creation functions
edgesDij :: (LorentzVector a) => (a -> a -> Double) -> [LNode a] -> [LEdge Double]
edgesDij dist ns = [(n, m, dist x y) | (n, x) <- ns, (m, y) <- ns, n <= m]


edgeDij :: (LorentzVector a) => (a -> a -> Double) -> LNode a -> [LNode a] -> [LEdge Double]
edgeDij dist (n, x) ns = [(n, m, dist x y) | (m, y) <- ns]


minDij :: (LorentzVector a) => (a -> a -> Double) -> ClusterGraph a -> LEdge Double
minDij dist gr = foldr1 (\ln@(_, _, x) rn@(_, _, y) -> if x < y then ln else rn) $ labEdges gr


combineClusters :: (LorentzVector a) => (a -> a -> Double) -> ClusterGraph a -> LEdge Double -> ClusterGraph a
combineClusters dist gr (n, m, _) = insEdges newEdges newGr
    where
        newN = (n, (getNodeLabel gr n) ^+^ (getNodeLabel gr m))
        newGr = insNode newN $ delNodes [m, n] gr
        newEdges = edgeDij dist newN $ labNodes newGr


getNodeLabel :: ClusterGraph a -> Node -> a
getNodeLabel gr n = let (_, _, x, _) = context gr n in x


-- inputs: distance measure, list of jets, current graph -> output jets
clusterJets :: (LorentzVector a) => (a -> a -> Double) -> [a] -> (ClusterGraph a) -> [a]
clusterJets dist jets gr
    | isEmpty gr = jets
    | otherwise = if (minI == minJ)
        then clusterJets dist ((getNodeLabel gr minI):jets) $ delNode minI gr
        else clusterJets dist jets $ combineClusters dist gr minE
        where minE@(minI, minJ, _) = minDij dist gr


ptSort :: (LorentzVector a) => [a] -> [a]
ptSort = reverse . (sortBy (comparing ptV))

testGraph = (labNodes gr, labEdges gr)
    where gr = makeClusterGraph (dij 0.4 (-1)) $ map fromPtEtaPhiE [(50, 2, 2, 60), (40, 2.5, 2.5, 45)]


aktJets r cls = clusterJets dist [] $ makeClusterGraph dist cls
    where dist = dij r (-1)
