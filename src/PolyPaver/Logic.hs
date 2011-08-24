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
module PolyPaver.Logic 
(
    TruthValue(..),
    TVM(..),
    TVDebugReport(..)
)
where

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
        Bool -> -- True to allow skewing 
        Bool -> -- True to allow split direction guessing 
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
    deriving (Show)

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
    tv ~> (TVMDecided False) = PolyPaver.Logic.not tv
    (TVMUndecided form1 dist1 hps1) ~> (TVMUndecided form2 dist2 hps2)
        = TVMUndecided (Implies form1 form2) (max dist1 dist2) (combineHPs hps1 hps2)

    fromBool _ = TVMDecided
    leq form a b = 
        case maybeResult of
            Just result -> TVMDecided result
            Nothing ->
                case maybeHyperplane of
                    Just hyperplane -> TVMUndecided form distance [(distance, hyperplane)]
                    _ -> TVMUndecided form distance []
        where
        (maybeResult, distance, maybeHyperplane) = analyseLeq a b
    includes form a b = -- b `Ni` a
        case (maybeResultL, maybeResultR, maybeResultU, maybeResultD) of
            (Just True, Just True, _, _) -> TVMDecided True
            (_, _, Just False, _) -> TVMDecided False
            (_, _, _, Just False) -> TVMDecided False
            _ -> TVMUndecided form distance hyperplanes 
        where
        distance = foldl1 max [distanceL, distanceR, distanceU, distanceD]
        hyperplanes =
            case (maybeHyperplaneL, maybeHyperplaneR) of
                (Just hyperplaneL, Just hyperplaneR) -> [(distanceL, hyperplaneL), (distanceR, hyperplaneR)]
                (Just hyperplaneL, _) -> [(distanceL, hyperplaneL)]
                (_, Just hyperplaneR) -> [(distanceR, hyperplaneR)]
                _ -> []
        (maybeResultL, distanceL, maybeHyperplaneL) = analyseLeq aL bL -- testing for truth (part 1)
        (maybeResultR, distanceR, maybeHyperplaneR) = analyseLeq bR aR -- testing for truth (part 2)
        (maybeResultU, distanceU, maybeHyperplaneU) = analyseLeq bL aR -- testing for falsity due to b > a
        (maybeResultD, distanceD, maybeHyperplaneD) = analyseLeq aL bR -- testing for falsity due to b < a
        (_,(aL,aR)) = RA.oiBounds a
        ((bL,bR),_) = RA.oiBounds b 
    bot form = TVMUndecided form (1/0) [] -- infinite badness...
    decide _ (TVMDecided result) = Just result
    decide _ (TVMUndecided _ _ _) = Nothing
    
    split varsNotToSplit prebox boxSkewing splitGuessing tv 
        = 
        (success, maybeHP, boxes)
        where
        -- investigate need for skewing and possibly skew:
        (box, maybeHP, maybeSkewVar)
            = tryToSkew boxSkewing splitGuessing prebox tv
        (success, boxes)    
            = makeSplit splitGuessing varsNotToSplit box maybeSkewVar
            
tryToSkew boxSkewing splitGuessing prebox tv
    | Prelude.not boxSkewing 
        Prelude.|| 
        (Prelude.not hyperplanesClose) 
        = 
        case splitGuessing Prelude.&& gotHyperPlane of
            True -> (prebox, Nothing, maybeSkewVar)
            False -> (prebox, Nothing, Nothing)
    | otherwise
        = (skewedBox, Just hyperplane1, maybeSkewVar)
    where
    hyperplanesClose
        | gotHyperPlane 
            = 
            ((isecPtDistance `RA.leqReals` 2) == Just True)
            Prelude.&&
            ((isecPtDistance2 `RA.leqReals` 2) == Just True)
        | otherwise = False
    (isecPtDistance,  maybeSkewVar, skewedBox) = ppSkewAlongHyperPlane prebox hyperplane1
    (isecPtDistance2, _, _) = ppSkewAlongHyperPlane prebox hyperplane2
    (gotHyperPlane, hyperplane1, hyperplane2)
        = case tv of
            (TVMUndecided _ _ ((_, hyperplane1) : (_, hyperplane2) : _)) -> 
                (True, hyperplane1, hyperplane2)
            _ -> 
                (False, err, err)
        where
        err = error $ "PolyPaver.Logic: tryToSkew: internal error, tv = " ++ show tv

makeSplit splitGuessing varsNotToSplit box maybeSkewVar
    = (success, (boxL, boxR))
    where
    -- perform split (potentially after skewing):
    success = Prelude.not $ (box `ppEqual` boxL) Prelude.|| (box `ppEqual` boxR)
    var
        =
        case maybeSkewVar of
            Just var
                | 20 * varWidth < largestWidth -> widestVar
                | otherwise -> var
                where
                varWidth = case IMap.lookup var widths of Just w -> w
            _ -> widestVar
                
    (largestWidth, widestVar) = foldl findWidestVar (0, err) $ IMap.toList widths
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

analyseLeq a b =
    (maybeResult, distance, maybeHyperplane)
    where
    maybeHyperplane =
        case vagueness `RA.leqReals` (2^^(-10)) of
            Just True -> Just hyperplane
            _ -> Nothing
    maybeResult = a `RA.leqReals` b
    distance = snd $ RA.doubleBounds $ a - b
    hyperplane = (t c, IMap.map t $ IMap.fromList $ Map.toList coeffs)
    t [a] = a
    (c, coeffs) = UFA.getAffineUpperBound $ a - b
    vagueness = (t c - t cDn) / avgSlope -- average distance of upper and lower linear bound of a - b
    avgSlope = (sum $ map (abs . head) $ Map.elems coeffs) / (fromIntegral $ Map.size coeffs)
    (cDn, _) = UFA.getAffineUpperBound $ b - a
    
data TVDebugReport = TVDebugReport String    
    
instance TruthValue TVDebugReport where
    not tv = tv
    (TVDebugReport r1) && (TVDebugReport r2) = TVDebugReport $ r1 ++ r2 
    (TVDebugReport r1) || (TVDebugReport r2) = TVDebugReport $ r1 ++ r2 
    (TVDebugReport r1) ~> (TVDebugReport r2) = TVDebugReport $ r1 ++ r2 
    fromBool _ _ = TVDebugReport ""
    bot form = TVDebugReport ""
    leq form a b = 
        TVDebugReport $
            banner
            ++ "\nLEQ:\n" ++ show form
            ++ "\n\nLHS:\n" ++ show a
            ++ "\n\nRHS:\n" ++ show b
            ++ "\n\nRESULT = " ++ show (a `RA.leqReals` b)
            where
            banner = concat $ replicate 100 "<="
    includes form a b = 
        TVDebugReport $
            banner
            ++ "\nINCL:\n" ++ show form
            ++ "\n\nLHS:\n" ++ show b
            ++ "\n\nRHS:\n" ++ show a
            ++ "\n\nRESULT = " ++ show (a `RA.leqReals` b)
            where
            banner = concat $ replicate 100 "<="
            