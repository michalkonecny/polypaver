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

import Prelude hiding (not)
import qualified Prelude

import PolyPaver.PPBox
import PolyPaver.Form

import Numeric.ER.Misc
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.RnToRm.UnitDom.Approx as UFA
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.RnToRm.DefaultRepr

import qualified Data.Map as Map

class TruthValue tv where
    not :: tv -> tv
    (&&) :: tv -> tv -> tv
    (||) :: tv -> tv -> tv
    (~>) :: tv -> tv -> tv
    fromBool :: PPBox BM -> Bool -> tv
    leq :: Form -> FAPUOI BM -> FAPUOI BM -> tv
    includes :: Form -> FAPUOI BM -> FAPUOI BM -> tv
    bot :: Form -> tv
    decide :: Int -> tv -> Maybe Bool
    split :: [Int] -> PPBox BM -> tv -> (Int,(PPBox BM, PPBox BM))

data TVM
    = TVMDecided Bool 
    | TVMUndecided 
        { 
            tvmSimplifiedFormula :: Form
        ,   tvmDistanceFromDecision :: Double
        ,   tvmDecisionDirections :: [BoxDirection BM] 
        }

instance TruthValue TVM where
    not (TVMDecided x) = TVMDecided (Prelude.not x)
    not (TVMUndecided form dist dirs) = TVMUndecided (Not form) dist dirs
    -- and:
    (TVMDecided False) && _ = TVMDecided False
    (TVMDecided True) && tv = tv
    _ && (TVMDecided False) = TVMDecided False
    tv && (TVMDecided True) = tv
    (TVMUndecided form1 m1 dirs1) && (TVMUndecided form2 m2 dirs2) 
        = TVMUndecided (And form1 form2) (max m1 m2) (dirs1 ++ dirs2)
    -- or: 
    (TVMDecided True) || _ = TVMDecided True
    (TVMDecided False) || tv = tv
    _ || (TVMDecided True) = TVMDecided True
    tv || (TVMDecided False) = tv
    (TVMUndecided form1 m1 dirs1) || (TVMUndecided form2 m2 dirs2)
        = TVMUndecided (Or form1 form2) (max m1 m2) (dirs1 ++ dirs2)
    -- implication:
    (TVMDecided False) ~> _ = TVMDecided True
    (TVMDecided True) ~> tv = tv
    _ ~> (TVMDecided True) = TVMDecided True
    tv ~> (TVMDecided False) = not tv
    (TVMUndecided form1 m1 dirs1) ~> (TVMUndecided form2 m2 dirs2)
        = TVMUndecided (Implies form1 form2) (max m1 m2) (dirs1 ++ dirs2)

    fromBool _ = TVMDecided
    leq form a b = 
        case a `RA.leqReals` b of
            Just result -> TVMDecided result
            Nothing -> TVMUndecided form measure [dir]
        where
        measure = snd $ RA.doubleBounds $ a - b
        dir = (t c, Map.map t coeffs)
        t [a] = a
        (c, coeffs) = UFA.getAffineUpperBound $ a - b
    includes form a b = 
        case a `RA.includes` b of
            Just result -> TVMDecided result
            Nothing -> TVMUndecided form 1 [] -- TODO
    bot form = TVMUndecided form (1/0) [] -- infinite badness...
    decide _ (TVMDecided result) = Just result
    decide _ (TVMUndecided _ _ _) = Nothing
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
    