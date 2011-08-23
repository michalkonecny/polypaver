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
import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.RnToRm.UnitDom.Approx as UFA
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.RnToRm.DefaultRepr

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.IntMap as IMap

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
    split :: 
        [Int] -> -- vars that must not be split
        PPBox BM -> -- box to split
        Bool -> -- True to forbid skewing 
        tv -> -- undecided truth value that may be used to help guide splitting and/or skewing 
        (Bool, -- whether split succeeded in providing two proper sub-boxes 
         Maybe (BoxHyperPlane BM), -- whether box skewing has been used
         (PPBox BM, PPBox BM))

data TVM
    = TVMDecided Bool 
    | TVMUndecided 
        { 
            tvmSimplifiedFormula :: Form
        ,   tvmDistanceFromDecision :: Double
        ,   tvmDecisionHyperPlanes :: [(Double, BoxHyperPlane BM)] -- the first one is the best one, keeping its measure 
        }

instance TruthValue TVM where
    not (TVMDecided x) = TVMDecided (Prelude.not x)
    not (TVMUndecided form dist hps) = TVMUndecided (Not form) dist hps
    -- and:
    (TVMDecided False) && _ = TVMDecided False
    (TVMDecided True) && tv = tv
    _ && (TVMDecided False) = TVMDecided False
    tv && (TVMDecided True) = tv
    (TVMUndecided form1 dist1 hps1) && (TVMUndecided form2 dist2 hps2) 
        = TVMUndecided (And form1 form2) (max dist1 dist2) (combineHPs hps1 hps2)
    -- or: 
    (TVMDecided True) || _ = TVMDecided True
    (TVMDecided False) || tv = tv
    _ || (TVMDecided True) = TVMDecided True
    tv || (TVMDecided False) = tv
    (TVMUndecided form1 dist1 hps1) || (TVMUndecided form2 dist2 hps2)
        = TVMUndecided (Or form1 form2) (max dist1 dist2) (combineHPs hps1 hps2)
    -- implication:
    (TVMDecided False) ~> _ = TVMDecided True
    (TVMDecided True) ~> tv = tv
    _ ~> (TVMDecided True) = TVMDecided True
    tv ~> (TVMDecided False) = not tv
    (TVMUndecided form1 dist1 hps1) ~> (TVMUndecided form2 dist2 hps2)
        = TVMUndecided (Implies form1 form2) (max dist1 dist2) (combineHPs hps1 hps2)

    fromBool _ = TVMDecided
    leq form a b = 
        case a `RA.leqReals` b of
            Just result -> TVMDecided result
            Nothing -> TVMUndecided form distance [(distance, hyperplane)]
        where
        distance = snd $ RA.doubleBounds $ a - b
        hyperplane = (t c, IMap.map t $ IMap.fromList $ Map.toList coeffs)
        t [a] = a
        (c, coeffs) = UFA.getAffineUpperBound $ a - b
    includes form a b = 
        case a `RA.includes` b of
            Just result -> TVMDecided result
            Nothing -> TVMUndecided form 1 [] -- TODO
    bot form = TVMUndecided form (1/0) [] -- infinite badness...
    decide _ (TVMDecided result) = Just result
    decide _ (TVMUndecided _ _ _) = Nothing
    
    split varsNotToSplit prebox noBoxSkewing tv 
        = 
        (success, maybeHP, (boxL, boxR))
        where
        -- investigate need for skewing and possibly skew:
        (box, maybeHP, maybeSkewVar)
            | noBoxSkewing 
                Prelude.|| (Prelude.not hyperplaneClose) 
                = (prebox, Nothing, Nothing)
            | otherwise = (skewedBox, Just hyperplane, maybeSkewVar)
            where
            hyperplaneClose
                | gotHyperPlane 
                    = 
                    (isecPtDistance `RA.leqReals` 2) == Just True
                | otherwise = False
            (isecPtDistance,  maybeSkewVar, skewedBox) = ppSkewAlongHyperPlane prebox hyperplane
            (gotHyperPlane, hyperplane)
                = case tv of
                    (TVMUndecided _ _ ((_, hyperplane) : _)) -> (True, hyperplane)
                    _ -> (False, error "PolyPaver.Logic: split: internal error")
            
        
        -- perform split (potentially after skewing):
        success = Prelude.not $ (box `ppEqual` boxL) Prelude.|| (box `ppEqual` boxR)
        (_, var)
            =
            case maybeSkewVar of
                Just var -> (0, var)
                _ ->
                    foldl findWidestVar (0, err) $ IMap.toList widths
        err = 
            error $ "PolyPaver.Logic: split: failed to find a split for " ++ show box 
        findWidestVar (prevWidth, prevRes) (var, currWidth)
            | currWidth `RA.leqReals` prevWidth == Just True = (prevWidth, prevRes)
            | otherwise = (currWidth, var)
        widths 
            = 
            foldl (IMap.unionWith (+)) IMap.empty $
                map (IMap.map abs) $ map snd $ IMap.elems splittablesubbox
        splittablesubbox =
            foldr IMap.delete box varsNotToSplit
        boxL = IMap.map substL box
        boxR = IMap.map substR box
        substL (c, coeffs) =
            (lower $ c - varCoeffHalf, IMap.insert var varCoeffHalf coeffs)
            where
            varCoeffHalf
                =
                upper $
                case IMap.lookup var coeffs of Just cf -> cf / 2
        substR (c, coeffs) =
            (lower $ c + varCoeffHalf, IMap.insert var varCoeffHalf coeffs)
            where
            varCoeffHalf
                =
                upper $
                case IMap.lookup var coeffs of Just cf -> cf / 2
        lower i = fst $ RA.bounds i
        upper i = snd $ RA.bounds i
    
    
combineHPs hps1 hps2
    = List.sortBy (\(m1, _) (m2, _) -> compare m1 m2) $ hps1 ++ hps2 
    