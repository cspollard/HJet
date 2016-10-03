{-# LANGUAGE TupleSections #-}

module Data.Jet where

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

obj :: Tree a -> a
obj (Branch _ x _) = x
obj (Leaf x) = x

cluster :: (Num a, Ord a) => (XYZT -> XYZT -> a) -> [XYZT] -> [Tree (a, XYZT)]
cluster d pjs = let ts = IM.fromList . zip [0..] $ fmap (Leaf . (0,)) pjs
                in  next ts $ mkPairs ts
    where
        mkPair ts i j = let f = snd . obj . (ts !)
                        in  (d (f i) (f j), (i, j))

        mkPairs ts = let is = IM.keys ts
                     in  sort [mkPair ts i j | i <- is, j <- is]

        -- next :: (Num a, Ord a)
             -- => IntMap (Tree (a, XYZT)) -> [PJPair a] -> [Tree (a, XYZT)]
        next ts [] = toListOf traverse ts
        next ts ((x, (i, j)):ds) =
            let tl = ts ! i
                tr = ts ! j
                pj = snd (obj tl) <> snd (obj tr)
                b = Branch tl (x, pj) tr
                ds' = filter (not . ((||) <$> hasInt i <*> hasInt j)) ds
                ts' = sans i $ sans j ts
                ts'' = IM.insert i b ts'
            in  next ts'' . foldr (insert . mkPair ts'' i) ds' $ IM.keys ts'
