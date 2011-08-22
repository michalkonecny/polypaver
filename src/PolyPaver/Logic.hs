{-# LANGUAGE FlexibleInstances #-}

{-|
    Module      :  PolyPaver.Logic
    Description :  interface to decisions based on arithmetic  
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    Loosely coupled interface to decisions based on arithmetic.
-}
module PolyPaver.Logic where

import PolyPaver.PPBox

import Numeric.ER.Misc
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.RnToRm.DefaultRepr

import qualified Data.Map as Map

class TruthValue tv where
    not :: tv -> tv
    (&&) :: tv -> tv -> tv
    (||) :: tv -> tv -> tv
    (~>) :: tv -> tv -> tv
    fromBool :: PPBox BM -> Bool -> tv
    leq :: FAPUOI BM -> FAPUOI BM -> tv
    includes :: FAPUOI BM -> FAPUOI BM -> tv
    split :: [Int] -> PPBox BM -> tv -> (Int,(PPBox BM, PPBox BM))
    decide :: Int -> tv -> Maybe Bool
    bot :: tv

data TVM =
    TVMDecided Bool | TVMUndecided { tvmDistanceFromDecision :: Double }

instance TruthValue TVM where
    not (TVMDecided x) = TVMDecided (Prelude.not x)
    not tv = tv
    -- and:
    (TVMDecided False) && _ = TVMDecided False
    (TVMDecided True) && tv = tv
    _ && (TVMDecided False) = TVMDecided False
    tv && (TVMDecided True) = tv
    (TVMUndecided m1) && (TVMUndecided m2) = TVMUndecided (max m1 m2)
    -- or: 
    (TVMDecided True) || _ = TVMDecided True
    (TVMDecided False) || tv = tv
    _ || (TVMDecided True) = TVMDecided True
    tv || (TVMDecided False) = tv
    (TVMUndecided m1) || (TVMUndecided m2) = TVMUndecided (max m1 m2)
    -- implication:
    (TVMDecided False) ~> _ = TVMDecided True
    (TVMDecided True) ~> tv = tv
    _ ~> (TVMDecided True) = TVMDecided True
    tv ~> (TVMDecided False) = tv
    (TVMUndecided m1) ~> (TVMUndecided m2) = TVMUndecided (max m1 m2)

    fromBool _ = TVMDecided
    a `leq` b = 
        case a `RA.leqReals` b of
            Just result -> TVMDecided result
            Nothing -> TVMUndecided measure
        where
        measure = snd $ RA.doubleBounds $ a - b
    a `includes` b = 
        case a `RA.includes` b of
            Just result -> TVMDecided result
            Nothing -> TVMUndecided 1 -- TODO
    split thinvarids box tv =
        (var,(boxL, boxR))
        where
        (_, var)
            =
            foldl findWidestVar (0, err) $ Map.toList widths
        err = 
            error $ "PolyPaver.Logic: split: failed to find a split for " ++ show box 
        findWidestVar (prevWidth, prevRes) (var, currWidth)
            | currWidth `RA.leqReals` prevWidth == Just True = (prevWidth, prevRes)
            | otherwise = (currWidth, var)
        widths 
            = 
            foldl (Map.unionWith (+)) Map.empty $
                map (Map.map abs) $ map snd $ DBox.elems splittablesubbox
        splittablesubbox =
            foldr DBox.delete box thinvarids
        boxL = DBox.map substL box
        boxR = DBox.map substR box
        substL (c, coeffs) =
            (lower $ c - varCoeffHalf, Map.insert var varCoeffHalf coeffs)
            where
            varCoeffHalf
                =
                upper $
                case Map.lookup var coeffs of Just cf -> cf / 2
        substR (c, coeffs) =
            (lower $ c + varCoeffHalf, Map.insert var varCoeffHalf coeffs)
            where
            varCoeffHalf
                =
                upper $
                case Map.lookup var coeffs of Just cf -> cf / 2
        lower i = fst $ RA.bounds i
        upper i = snd $ RA.bounds i
    decide _ (TVMDecided result) = Just result
    decide _ (TVMUndecided _) = Nothing
    bot = TVMUndecided (1/0) -- infinite badness...
    