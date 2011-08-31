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
    leq :: Form -> PPBox BM -> FAPUOI BM -> FAPUOI BM -> tv
    includes :: Form -> PPBox BM -> FAPUOI BM -> FAPUOI BM -> tv
    bot :: Form -> tv
    decide :: Int -> tv -> Maybe Bool
    split :: 
        [Int] -> -- vars that must not be split
        Maybe Int -> -- preferred variable to split
        PPBox BM -> -- box to split
        Bool -> -- True to allow skewing 
        Bool -> -- True to allow split direction guessing 
        tv -> -- undecided truth value that may be used to help guide splitting and/or skewing 
        (Bool, -- whether split succeeded in providing two proper sub-boxes 
         Maybe ((BoxHyperPlane BM, BoxHyperPlane BM), Form, IRA BM), -- whether box skewing has been used
         Int, -- variable whose domain was split
         (PPBox BM, PPBox BM))

data TVM
    = TVMDecided Bool 
    | TVMUndecided 
        { 
            tvmSimplifiedFormula :: Form
        ,   tvmDistanceFromTruth :: Double
        ,   tvmDecisionHyperPlanes :: [(Double, ((BoxHyperPlane BM, BoxHyperPlane BM), Form, IRA BM))] 
            -- the first one is the best one, keeping its measure, formula and vagueness 
        }
    
instance Show TVM where
    show (TVMDecided result) = "TVMDecided " ++ show result
    show (TVMUndecided form dist hps)
        =
        "TVMUndecided"
        ++ "\n form = " ++ showForm form 
        ++ "\n distance = " ++ show dist
        ++ "\n hyperplanes = "
            ++ (case hps of
                    [] -> "none"
                    _ -> (unlines $ map showHP $ zip [1..] hps))
        where
        showHP (n,(dist, ((hp, hpDn), hpForm, vagueness)))
            =
            "\n   hp" ++ show n ++ ":"
            ++ "dist = " ++ show dist
            ++ "; vagueness = " ++ show vagueness
            ++ "; hp = " ++ showAffine hp
            ++ "; hpDn = " ++ showAffine hpDn
            ++ "; form = " ++ showForm hpForm

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
    leq form box a b = 
        case maybeResult of
            Just result -> TVMDecided result
            Nothing ->
                case maybeHyperplane of
                    Just hyperplane -> TVMUndecided form distance [(distance, (hyperplane, form, vagueness))]
                    _ -> TVMUndecided form distance []
        where
        (maybeResult, distance, maybeHyperplane, vagueness) = analyseLeq box a b
    includes form box a b -- b `Ni` a
        =
        case (maybeResultL, maybeResultR, maybeResultU, maybeResultD) of
            (Just True, Just True, _, _) -> TVMDecided True
            (_, _, Just False, _) -> TVMDecided False
            (_, _, _, Just False) -> TVMDecided False
            _ -> TVMUndecided form distance hyperplanes 
        where
        distance = foldl1 max [distanceL, distanceR, distanceU, distanceD]
        hyperplanes 
            =
            case (maybeHyperplaneL, maybeHyperplaneR) of
                (Just hyperplaneL, Just hyperplaneR) 
                    -> [(distanceL, (hyperplaneL, form, vaguenessL)), 
                        (distanceR, (hyperplaneR, form, vaguenessR))]
                (Just hyperplaneL, _) -> [(distanceL, (hyperplaneL, form, vaguenessL))]
                (_, Just hyperplaneR) -> [(distanceR, (hyperplaneR, form, vaguenessR))]
                _ -> []
        (maybeResultL, distanceL, maybeHyperplaneL, vaguenessL) 
            = analyseLeq box aL bL -- testing for truth (part 1)
        (maybeResultR, distanceR, maybeHyperplaneR, vaguenessR) 
            = analyseLeq box bR aR -- testing for truth (part 2)
        (maybeResultU, distanceU, maybeHyperplaneU, _) 
            = analyseLeq box bL aR -- testing for falsity due to b > a
        (maybeResultD, distanceD, maybeHyperplaneD, _) 
            = analyseLeq box aL bR -- testing for falsity due to b < a
        (_,(aL,aR)) = RA.oiBounds a
        ((bL,bR),_) = RA.oiBounds b 
    bot form = TVMUndecided form (1/0) [] -- infinite badness...
    decide _ (TVMDecided result) = Just result
    decide _ (TVMUndecided _ _ _) = Nothing
    
    split varsNotToSplit maybeVar prebox boxSkewing splitGuessing tv 
        = 
        (success, maybeHP, splitVar, boxes)
        where
        -- investigate need for skewing and possibly skew:
        (box, maybeHP, maybeSkewVar)
            = tryToSkew boxSkewing prebox tv
        (success, boxes, splitVar)    
            = makeSplit splitGuessing varsNotToSplit maybeVar box maybeSkewVar
            
combineHPs hps1 hps2
    = List.sortBy (\(m1, _) (m2, _) -> compare m1 m2) $ hps1 ++ hps2 

analyseLeq box a b =
    (maybeResult, distanceD + vaguenessD, maybeHyperplane, vagueness)
    where
    maybeHyperplane =
        case maybeResult of
            Just _ -> Nothing
            Nothing -> 
                case vagueness `RA.leqReals` (2^^(0)) of
                    Just True -> Just (hyperplane, hyperplaneDn)
                    _ -> Nothing
    maybeResult = a `RA.leqReals` b
    distanceD = snd $ RA.doubleBounds $ a - b
    vaguenessD = snd $ RA.doubleBounds vagueness
    hyperplane
        = (t c, IMap.map t $ IMap.fromList $ Map.toList coeffs)
    hyperplaneDn 
        = (tn cDnNeg, IMap.map tn $ IMap.fromList $ Map.toList coeffsDnNeg)
        where
        negateSnd (a,b) = (a, negate $ t b)
    t [a] = a
    tn [a] = negate a
    (c, coeffs) = UFA.getAffineUpperBound $ a - b
    (cDnNeg, coeffsDnNeg) = UFA.getAffineUpperBound $ b - a
    vagueness =
        (t c + t cDnNeg) / avgSlope -- average distance of upper and lower linear bound of a - b
--        findMaxCoeffDifference 0 $ 
--            Map.toAscList $ 
--                Map.unionWith takeNonZero 
--                    (Map.map (\[cf] -> (-cf,0)) coeffsDnNeg)
--                    (Map.map (\[cf] -> (0,cf)) coeffs)
--        where
--        takeNonZero (cfL1,cfR1) (cfL2,cfR2) = (cfL, cfR)
--            where
--            cfL | cfL1 == 0 = cfL2
--                | otherwise = cfL1 
--            cfR | cfR1 == 0 = cfR2
--                | otherwise = cfR1 
--        findMaxCoeffDifference prevMax [] = prevMax  
--        findMaxCoeffDifference prevMax ((v,(l,r)):rest)
--            =
--            findMaxCoeffDifference (max prevMax currDiff) rest
--            where
--            currDiff = abs $ (r - l) / l
    avgSlope = (sum $ map (abs . head) $ Map.elems coeffs) / (fromIntegral $ Map.size coeffs)
    
tryToSkew boxSkewing prebox tv
    | (Prelude.not boxSkewing) 
       Prelude.|| 
      (Prelude.not hyperplanesClose) 
        = 
        case gotHyperPlane of
            True -> (prebox, Nothing, maybeSkewVar)
            False -> (prebox, Nothing, Nothing)
    | otherwise
        = (skewedBox, Just hp1, maybeSkewVar)
    where
    hyperplanesClose
        | gotHyperPlane
            = 
            ((isecPtDistance `RA.leqReals` 4) == Just True)
            Prelude.&&
            ((isecPtDistance2 `RA.leqReals` 4) == Just True)
        | otherwise = False
    (isecPtDistance,  maybeSkewVar, skewedBox) = ppSkewAlongHyperPlane prebox $ hyperplane1
    (isecPtDistance2, _, _) = ppSkewAlongHyperPlane prebox $ hyperplane2
    ((hyperplane1,_), _, _) = hp1
    ((hyperplane2,_), _, _) = hp2
    (gotHyperPlane, hp1, hp2)
        = case tv of
            (TVMUndecided _ _ ((_, hp1) : (_, hp2) : _)) -> 
                (True, hp1, hp2)
            _ -> 
                (False, err, err)
        where
        err = error $ "PolyPaver.Logic: tryToSkew: internal error, tv = " ++ show tv

makeSplit splitGuessing varsNotToSplit maybeVar box maybeSkewVar
    = (success, (boxL, boxR), var)
    where
    -- perform split (potentially after skewing):
    success = Prelude.not $ (box `ppEqual` boxL) Prelude.|| (box `ppEqual` boxR)
    var
        =
        case (splitGuessing, maybeSkewVar) of
            (True, Just var)
                | 20 * varWidth < largestWidth -> widestVar
                | otherwise -> var
                where
                varWidth =
--                    unsafePrint ("makeSplit: split direction from HP") $ 
                    case IMap.lookup var widths of Just w -> w
            _ -> 
                case maybeVar of
                    Just var -> var
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

data TVDebugReport = TVDebugReport String    
    
instance TruthValue TVDebugReport where
    not tv = tv
    (TVDebugReport r1) && (TVDebugReport r2) = TVDebugReport $ r1 ++ r2 
    (TVDebugReport r1) || (TVDebugReport r2) = TVDebugReport $ r1 ++ r2 
    (TVDebugReport r1) ~> (TVDebugReport r2) = TVDebugReport $ r1 ++ r2 
    fromBool _ _ = TVDebugReport ""
    bot form = TVDebugReport ""
    leq form box a b = 
        TVDebugReport $
            banner
            ++ "\nLEQ:\n" ++ show form
            ++ "\n\nLHS:\n" ++ show a
            ++ "\n\nRHS:\n" ++ show b
            ++ "\n\nRESULT = " ++ show (a `RA.leqReals` b)
            where
            banner = concat $ replicate 100 "<="
    includes form box a b = 
        TVDebugReport $
            banner
            ++ "\nINCL:\n" ++ show form
            ++ "\n\nLHS:\n" ++ show b
            ++ "\n\nRHS:\n" ++ show a
            ++ "\n\nRESULT = " ++ show (a `RA.leqReals` b)
            where
            banner = concat $ replicate 100 "<="
            