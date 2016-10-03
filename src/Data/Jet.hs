module Data.Jet where

import Data.Vector
import Data.HEP.LorentzVector


type PJs = Vector XYZT
type PJPair = (Double, (Int, Int))

data Tree a = Branch (Tree a) a (Tree a)
            | Leaf a


cluster :: PJs -> (PJs, [Tree Int])
cluster = undefined
