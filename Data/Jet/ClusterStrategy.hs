module Data.Jet.ClusterStrategy where

import Data.HepMC.FourMomentum
import Data.Jet.PseudoJet

class ClusterStrategy c where
    dij :: (FourMomentum a, FourMomentum b) => c -> a -> b -> Double
    diB :: FourMomentum a => c -> a -> Double


data KTLike = KTLike { r0 :: Double, p :: Int }

kt, camKt, antiKt :: Double -> KTLike

antiKt = flip KTLike (-1)
kt = flip KTLike 1
camKt = flip KTLike 0


instance ClusterStrategy KTLike where
    dij c i j = ((ptV i `min` ptV j) ^ p c) * dRV i j / r0 c
    diB c i = ptV i ^ p c



cluster :: ClusterStrategy c => c -> PseudoJets -> PseudoJets
cluster cs pjs = pjs


-- here
-- data ClusterEvent = ClusterEvent 
