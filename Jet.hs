module Jet where
import Data.List (sortBy, delete)
import Data.List.Extras (argmin)
import Data.AdditiveGroup
import Data.Ord (comparing)
import LorentzVector

type Cluster = Vec4

aktJets :: Double -> ([BTree Cluster] -> [BTree Cluster])
aktJets r = ptSort . (flip (clusterJets r (-1)) [])

ktJets :: Double -> ([BTree Cluster] -> [BTree Cluster])
ktJets r = ptSort . (flip (clusterJets r (1)) [])

caJets :: Double -> ([BTree Cluster] -> [BTree Cluster])
caJets r = ptSort . (flip (clusterJets r (0)) [])

dij :: (LorentzVector a, Eq a) => Double -> Int -> a -> a -> Double
dij r p v w = min ((ptV v)^^(2*p)) ((ptV w)^^(2*p)) * (d^2)
    where d = if v == w then 1 else dRV v w / r

mindij :: Double -> Int -> [BTree Cluster] -> (BTree Cluster, BTree Cluster)
mindij r p cls = argmin (uncurry (dij r p)) [(x, y) | x <- cls, y <- cls]

clusterJets :: Double -> Int -> [BTree Cluster] -> [BTree Cluster] -> [BTree Cluster]
clusterJets _ _ [] jets = jets
clusterJets r p cls jets
    | x == y = clusterJets r p (delete x cls) (x:jets)
    | otherwise = clusterJets r p (x ^+^ y : (delete y (delete x cls))) jets
    where (x, y) = mindij r p cls

ptSort :: (LorentzVector a) => [a] -> [a]
ptSort = reverse . (sortBy (comparing ptV))
