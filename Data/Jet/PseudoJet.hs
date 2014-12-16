module Data.Jet.PseudoJet where

import Data.Vector

import Data.HepMC.FourMomentum
import Data.HepMC.XYZT
import Data.HepMC.Barcoded


data PseudoJet = PseudoJet BC XYZT (Maybe (PseudoJet, PseudoJet))

type PseudoJets = Vector PseudoJet

data PJPair = PJPair Double BC BC


daughters :: PseudoJet -> Maybe (PseudoJet, PseudoJet)
daughters (PseudoJet _ _ d) = d


instance Barcoded PseudoJet where
    bc (PseudoJet b _ _) = b

instance Eq PseudoJet where
    (==) = liftBC2 (==)

instance Ord PseudoJet where
    compare = liftBC2 compare


instance HasFourMom PseudoJet where
    fourMom (PseudoJet _ v _) = v

instance FourMomentum PseudoJet where
    xV = xV . fourMom
    yV = yV . fourMom
    zV = zV . fourMom
    tV = tV . fourMom
