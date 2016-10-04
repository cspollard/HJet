{-# LANGUAGE TupleSections #-}

module Data.Jet where

import Debug.Trace

import Control.Lens

import Data.List (insert, sort)
import Data.Function (on)
import Data.Semigroup ((<>))

import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IM
import Data.HEP.LorentzVector


type PJPair a = (a, (Int, Int))

hasInt :: Int -> PJPair a -> Bool
hasInt i (_, (i', j')) | i == i' || i == j' = True
                       | otherwise          = False


data Tree a = Branch (Tree a) a (Tree a)
            | Leaf a
            deriving Show

data Clustering a =
    Clustering { dij :: XYZT -> XYZT -> a
               , diB :: XYZT -> a
               }

obj :: Tree a -> a
obj (Branch _ x _) = x
obj (Leaf x) = x

cluster :: (Num a, Ord a, Show a) => Clustering a -> [XYZT] -> [Tree (a, XYZT)]
cluster (Clustering d d0) pjs =
    let ts = IM.fromList . zip [0..] $ fmap (Leaf . (0,)) pjs
    in  next ts $ mkPairs ts

    where
        mkPair ts i j = let f = snd . obj . (ts !)
                        in  (d (f i) (f j), (i, j))

        mkPairs ts = let is = IM.keys ts
                     in  sort $ [mkPair ts i j |  i <- is, j <- is, i < j] ++ fmap (mkBeam ts) is

        mkBeam ts i = (d0 (snd . obj $ ts ! i), (i, -1))

        next ts [] = toListOf traverse ts
        next ts ((x, (i, j)):ds) =
            if j < 0
                -- if we're merging with the beam
                then
                    -- remove all trace of i from the distance list
                    traceShow "mergeIB" $ next ts (filter (not . hasInt i) ds)
                else
                    let tl = ts ! i
                        tr = ts ! j
                        pj = snd (obj tl) <> snd (obj tr)
                        b = Branch tl (x, pj) tr
                        ds' = filter (not . ((||) <$> hasInt i <*> hasInt j)) ds
                        ts' = sans i $ sans j ts
                        ts'' = traceShow "mergeIJ" $ IM.insert i b ts'
                    in  seq ts' . next ts'' . insert (mkBeam ts'' i) . foldr (insert . mkPair ts'' i) ds' $ IM.keys ts'


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
