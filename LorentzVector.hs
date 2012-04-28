module LorentzVector where
import Data.AdditiveGroup

-- 4-vector data type
data Vec4 = Vec4 Double Double Double Double deriving (Eq, Show, Read, Ord)

-- Binary tree to represent combined 4-vectors
data BTree a = BNil | BNode a (BTree a) (BTree a) deriving (Show)

instance (Eq a) => Eq (BTree a) where
    (==) BNil BNil = True
    (==) (BNode x _ _) (BNode y _ _) = (x == y)
    (==) _ _ = False

getData :: (BTree a) -> a
getData BNil = undefined
getData (BNode x _ _) = x

sq :: (Floating a) => a -> a
sq x = x*x

sqrt' :: (Ord a, Floating a) => a -> a
sqrt' x = if x >= 0.0 then sqrt x else (-sqrt (-x))

-- minimum definition: xV, yV, zV, tV
class (AdditiveGroup v) => LorentzVector v where
    xV :: v -> Double
    yV :: v -> Double
    zV :: v -> Double
    tV :: v -> Double

    pxV :: v -> Double
    pxV = xV

    pyV :: v -> Double
    pyV = yV

    pzV :: v -> Double
    pzV = zV

    eV :: v -> Double
    eV = tV

    pt2V :: v -> Double
    pt2V w = (sq (xV w)) + (sq (yV w))

    p2V :: v -> Double
    p2V w = (pt2V w) + (sq (zV w))

    m2V :: v -> Double
    m2V w = (sq (tV w)) - (p2V w)

    ptV :: v -> Double
    ptV = sqrt' . pt2V

    pV :: v -> Double
    pV = sqrt' . p2V

    mV :: v -> Double
    mV = sqrt' . m2V

    etaV :: v -> Double
    etaV w = acosh $ (pV w) / (ptV w)

    thetaV :: v -> Double
    thetaV w = atan $ (ptV w) / (pzV w)

    phiV :: v -> Double
    phiV w = atan $ (yV w) / (xV w)

instance AdditiveGroup Vec4 where
    zeroV = Vec4 0.0 0.0 0.0 0.0
    (Vec4 x1 y1 z1 t1) ^+^ (Vec4 x2 y2 z2 t2) = Vec4 (x1+x2) (y1+y2) (z1+z2) (t1+t2)
    negateV (Vec4 x y z t) = Vec4 (-x) (-y) (-z) (-t)

instance LorentzVector Vec4 where
    xV (Vec4 x _ _ _) = x
    yV (Vec4 _ y _ _) = y
    zV (Vec4 _ _ z _) = z
    tV (Vec4 _ _ _ t) = t

instance (AdditiveGroup v) => AdditiveGroup (BTree v) where
    zeroV = BNil
    z ^+^ w = BNode (getData z ^+^ (getData w)) z w
    negateV BNil = BNil
    negateV (BNode w b c) = BNode (negateV w) (negateV b) (negateV c)

instance (LorentzVector v) => LorentzVector (BTree v) where
    xV (BNode w _ _) = xV w
    yV (BNode w _ _) = yV w
    zV (BNode w _ _) = zV w
    tV (BNode w _ _) = tV w

dRV :: (LorentzVector a) => a -> a -> Double
dRV v w = sqrt(sq (etaV z) + sq (phiV z))
    where z = v ^-^ w

fromXYZT :: Double -> Double -> Double -> Double -> Vec4
fromXYZT x y z t = Vec4 x y z t

fromPtEtaPhiE :: Double -> Double -> Double -> Double -> Vec4
fromPtEtaPhiE pt eta phi e = Vec4 px py pz e
    where
        px = pt * (cos phi)
        py = pt * (sin phi)
        pz = pt * (sinh eta)
