{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Applicative (many)

import Data.Semigroup

import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Tree

import Data.HEP.LorentzVector
import Data.Jet

newtype MyPJ = MyPJ { _unPJ :: (Maybe Int, PtEtaPhiE) } deriving Show
makeLenses ''MyPJ

instance HasLorentzVector MyPJ where
    toPtEtaPhiE = unPJ . _2

instance Semigroup MyPJ where
    MyPJ (_, pj) <> MyPJ (_, pj') = MyPJ (Nothing, pj <> pj')

isConstit :: MyPJ -> Bool
isConstit (MyPJ (Nothing, _)) = False
isConstit _ = True

parseConstit :: String -> PtEtaPhiE
parseConstit s =
    let [x, y, z, t] = map read $ words s
    in  view toPtEtaPhiE $ XYZT x y z t


main :: IO ()
main = do
    pjs <- map MyPJ . zip (map Just [0..]) . map parseConstit <$> many getLine
    mapM_ (\j -> do
                    print . view toPtEtaPhiE . snd . rootLabel $ j
                    -- putStrLn . drawTree . fmap show $ j
                    traverseOf_ (traverse . _2 . unPJ . _1 . _Just) print j
                    putStrLn ""
          )

        -- . sortOn (Down . view lvPt)
        -- . map (view toPtEtaPhiE . snd . obj)
        $ cluster (akt 0.6) pjs
