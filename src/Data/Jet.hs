{-# LANGUAGE TupleSections #-}

module Data.Jet where

import Control.Lens

import Data.List (insert, sort)
import Data.Semigroup (Semigroup(..))

import Data.IntMap.Strict ((!))
import qualified Data.IntMap.Strict as IM

import Data.Tree (Tree(..))

import Data.HEP.LorentzVector


type PJPair a = (a, (Int, Int))

hasInt :: Int -> PJPair a -> Bool
hasInt i (_, (i', j')) | i == i' || i == j' = True
                       | otherwise          = False


data Clustering a =
    Clustering { dij :: XYZT -> XYZT -> a
               , diB :: XYZT -> a
               }

cluster :: (Num a, Ord a, HasLorentzVector b, Semigroup b)
        => Clustering a -> [b] -> [Tree (a, b)]
cluster (Clustering d d0) pjs =
    let ts = IM.fromList . zip [0..] $ fmap leaf pjs
    in  next ts $ mkPairs ts

    where
        leaf pj = Node (0, pj) []

        mkPair ts i j | i == j =
            let pj = view toXYZT . snd . rootLabel $ ts ! i
            in  (d0 pj, (i, i))

                      | otherwise =
            let f = view toXYZT . snd . rootLabel . (ts !)
            in  (d (f i) (f j), (i, j))

        mkPairs ts = let is = IM.keys ts
                     in  sort $ [mkPair ts i j |  i <- is, j <- is, i < j] ++ [mkPair ts i i | i <- is]

        next ts [] = toListOf traverse ts
        next ts ((x, (i, j)):ds) =
            if i == j
                -- if we're merging with the beam
                then
                    -- remove all trace of i from the distance list
                    next ts (filter (not . hasInt i) ds)
                else
                    let tl = ts ! i
                        tr = ts ! j
                        pj = snd (rootLabel tl) <> snd (rootLabel tr)
                        b = Node (x, pj) [tl, tr]
                        ds' = filter (not . ((||) <$> hasInt i <*> hasInt j)) ds
                        ts' = IM.insert i b $ sans j ts
                    in  seq ts' . next ts' . foldr (insert . mkPair ts' i) ds' $ IM.keys ts'


ktLike :: Int -> Double -> Clustering Double
ktLike p r0 = Clustering d d0
    where
        d pj pj' = min (view lvPt pj ^^ (2*p)) (view lvPt pj' ^^ (2*p)) * lvDR pj pj' / r0
        d0 pj = view lvPt pj ^^ (2*p)

kt :: Double -> Clustering Double
kt = ktLike 1

camKt :: Double -> Clustering Double
camKt = ktLike 0

akt :: Double -> Clustering Double
akt = ktLike (-1)
