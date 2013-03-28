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
    leq :: Label -> Form -> PPBox BM -> FAPUOI BM -> FAPUOI BM -> tv
    less :: Label -> Form -> PPBox BM -> FAPUOI BM -> FAPUOI BM -> tv
    includes :: Label -> Form -> PPBox BM -> FAPUOI BM -> FAPUOI BM -> tv
    bot :: Form -> tv
    decide :: tv -> Maybe Bool
    split :: 
        [Int] -> -- vars that must not be split
        Maybe Int -> -- preferred variable to split
        PPBox BM -> -- box to split
        Bool -> -- True to allow skewing 
        Maybe Int -> -- maybe allow split direction guessing but limit box width ratio to n 
        tv -> -- undecided truth value that may be used to help guide splitting and/or skewing 
        (Bool, -- whether split succeeded in providing two proper sub-boxes 
         Maybe ((BoxHyperPlane BM, BoxHyperPlane BM), Form, IRA BM), -- whether box skewing has been used
         Int, -- variable whose domain was split
         (PPBox BM, PPBox BM))

data TVM
    = TVMDecided 
        {
            tvmAtomicResults :: [(Label, (Maybe Bool, Double, Double))] -- result, distance, vagueness
        ,   tvmResult :: Bool 
        } 
    | TVMUndecided 
        { 
            tvmSimplifiedFormula :: Form
        ,   tvmDistanceFromTruth :: Double
        ,   tvmAtomicResults :: [(Label, (Maybe Bool, Double, Double))] -- result, distance, vagueness
        ,   tvmDecisionHyperPlanes :: [(Double, ((BoxHyperPlane BM, BoxHyperPlane BM), Form, IRA BM))] 
            -- the first one is the best one, keeping its measure, formula and vagueness 
        }
    
instance Show TVM where
    show (TVMDecided ares result) 
        = 
        "TVMDecided: " ++ show result 
        ++ "\n sub-results =\n " ++ unlines (map showAtomicResult ares) 
    show (TVMUndecided form dist ares hps)
        =
        "TVMUndecided"
        ++ "\n sub-results = \n" ++ unlines (map showAtomicResult ares)
        ++ " distance+vagueness = " ++ show dist
--        ++ "\n form = " ++ showForm form 
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

showAtomicResult (lab, (maybeResult, distanceD, vaguenessD)) =
    "    " ++ lab ++ ": " 
    ++ show maybeResult ++ ", dist = " ++ show distanceD ++ ", vagu = " ++ show vaguenessD

instance TruthValue TVM where
    not tv = tvmNot tv
    -- and:
    tv1@(TVMDecided _ False) && _ = tv1
    tv1@(TVMDecided ares1 True) && tv2 = tv2 { tvmAtomicResults = ares1 ++ (tvmAtomicResults tv2) }
    tv1 && (TVMDecided ares2 False) = TVMDecided (tvmAtomicResults tv1 ++ ares2) False
    tv1 && (TVMDecided ares2 True) = tv1 { tvmAtomicResults = (tvmAtomicResults tv1) ++ ares2}
    (TVMUndecided form1 dist1 ares1 hps1) && (TVMUndecided form2 dist2 ares2 hps2) 
        = TVMUndecided (And form1 form2) (max dist1 dist2) (ares1 ++ ares2) (combineHPs hps1 hps2)
    -- or: 
    tv1@(TVMDecided ares1 True) || _ = tv1
    (TVMDecided ares1 False) || tv2 = tv2 { tvmAtomicResults = ares1 ++ (tvmAtomicResults tv2)}
    tv1 || (TVMDecided ares2 True) = TVMDecided (tvmAtomicResults tv1 ++ ares2) True
    tv1 || (TVMDecided ares2 False) = tv1 { tvmAtomicResults = tvmAtomicResults tv1 ++ ares2 }
    (TVMUndecided form1 dist1 ares1 hps1) || (TVMUndecided form2 dist2 ares2 hps2)
        = TVMUndecided (Or form1 form2) (max dist1 dist2) (ares1 ++ ares2) (combineHPs hps1 hps2)
    -- implication:
    (TVMDecided ares1 False) ~> _ = TVMDecided ares1 True
    (TVMDecided ares1 True) ~> tv2 = tv2 { tvmAtomicResults = ares1 ++ (tvmAtomicResults tv2)}
    tv1 ~> (TVMDecided ares2 True) = TVMDecided (tvmAtomicResults tv1 ++ ares2) True
    tv1 ~> (TVMDecided ares2 False) = tvmNot tv1 { tvmAtomicResults = tvmAtomicResults tv1 ++ ares2 }
    (TVMUndecided form1 dist1 ares1 hps1) ~> (TVMUndecided form2 dist2 ares2 hps2)
        = TVMUndecided (Implies form1 form2) (max dist1 dist2) (ares1 ++ ares2) (combineHPs hps1 hps2)

    fromBool _ = TVMDecided []
    leq = tvmLeqLess True
    less = tvmLeqLess False
    includes lab form box a b -- b `Ni` a
        =
--        unsafePrintReturn
--        (
--            "Logic.includes:"
--            ++ "\n a = " ++ show a
--            ++ "\n b = " ++ show b
--            ++ "\n maybeResultL = " ++ show maybeResultL
--            ++ "\n maybeResultR = " ++ show maybeResultR
--            ++ "\n result = "
--        )
--        $
        case (maybeResultL, maybeResultR, maybeResultU, maybeResultD) of
            (Just True, Just True, _, _) -> TVMDecided [(lab, ares $ Just True)] True
            (_, _, Just True, _) -> TVMDecided [(lab, ares $ Just False)] False
            (_, _, _, Just True) -> TVMDecided [(lab, ares $ Just False)] False
            _ -> TVMUndecided form (distanceD + vaguenessD) [(lab, ares Nothing)] hyperplanes 
--        case a `RA.includes` b of
--            Just res -> TVMDecided res
--            _ -> TVMUndecided form 1 []
        where
        ares maybeResult = (maybeResult, distanceD, vaguenessD)
        distanceD = foldl1 max [distanceDL, distanceDR, distanceDU, distanceDD]
        vaguenessD = foldl1 max [vaguenessDL, vaguenessDR, vaguenessDU, vaguenessDD]
        hyperplanes 
            =
            case (maybeHyperplaneL, maybeHyperplaneR) of
                (Just hyperplaneL, Just hyperplaneR) 
                    -> [(distanceDL + vaguenessDL, (hyperplaneL, form, vaguenessL)), 
                        (distanceDR + vaguenessDR, (hyperplaneR, form, vaguenessR))]
                (Just hyperplaneL, _) -> [(distanceDL + vaguenessDL, (hyperplaneL, form, vaguenessL))]
                (_, Just hyperplaneR) -> [(distanceDR + vaguenessDR, (hyperplaneR, form, vaguenessR))]
                _ -> []
        (maybeResultL, distanceDL, vaguenessDL, maybeHyperplaneL, vaguenessL) 
            = analyseLeqLess True box aiL boL -- testing for truth (part 1)
        (maybeResultR, distanceDR, vaguenessDR, maybeHyperplaneR, vaguenessR) 
            = analyseLeqLess True box boR aiR -- testing for truth (part 2)
        (maybeResultU, distanceDU, vaguenessDU, maybeHyperplaneU, _) 
            = analyseLeqLess True box aoR boL -- testing for falsity due to b > a
        (maybeResultD, distanceDD, vaguenessDD, maybeHyperplaneD, _) 
            = analyseLeqLess True box boR aoL -- testing for falsity due to b < a
        ((aoL,aoR),(aiL,aiR)) = RA.oiBounds a
        ((boL,boR),_) = RA.oiBounds b 
    bot form = TVMUndecided form (1/0) [] [] -- infinite badness...
    decide (TVMDecided _ result) = Just result
    decide (TVMUndecided _ _ _ _) = Nothing
    
    split varsNotToSplit maybeVar prebox boxSkewing splitGuessing tv 
        = 
        (success, maybeHP, splitVar, boxes)
        where
        -- investigate need for skewing and possibly skew:
        (box, maybeHP, maybeSkewVar)
            = tryToSkew boxSkewing prebox tv
        (success, boxes, splitVar)    
            = makeSplit splitGuessing varsNotToSplit maybeVar box maybeSkewVar
            
tvmNot (TVMDecided ares x) = TVMDecided ares (Prelude.not x)
tvmNot (TVMUndecided form dist ares hps) = TVMUndecided (Not form) dist ares hps
            
combineHPs hps1 hps2
    = List.sortBy (\(m1, _) (m2, _) -> compare m1 m2) $ hps1 ++ hps2 

tvmLeqLess isLeq lab form box a b = 
    case maybeResult of
        Just result -> TVMDecided [(lab, (maybeResult, distanceD, vaguenessD))] result
        Nothing ->
            case maybeHyperplane of
                Just hyperplane -> 
                    TVMUndecided form distanceVagueness
                        [(lab, (Nothing, distanceD, vaguenessD))] 
                        [(distanceVagueness, (hyperplane, form, vagueness))]
                _ -> 
                    TVMUndecided form distanceVagueness 
                        [(lab, (Nothing, distanceD, vaguenessD))] 
                        []
    where
    distanceVagueness = distanceD + vaguenessD 
    (maybeResult, distanceD, vaguenessD, maybeHyperplane, vagueness) = analyseLeqLess isLeq box a b

analyseLeqLess isLeq box a b =
--    unsafePrint
--    (
--        "analyseLeqLess:"
--        ++ "\n isLeq = " ++ show isLeq
--        ++ "\n box = " ++ show box
--        ++ "\n a = " ++ show a
--        ++ "\n b = " ++ show b
--        ++ "\n maybeResult = " ++ show maybeResult
--        ++ "\n distanceD = " ++ show distanceD
--        ++ "\n vaguenessD = " ++ show vaguenessD
--    )
--    (maybeResult, 1, Nothing, 0)
    (maybeResult, distanceD, vaguenessD, maybeHyperplane, vagueness)
    where
    maybeHyperplane =
        case maybeResult of
            Just _ -> Nothing
            Nothing -> 
                case vagueness `RA.leqReals` (2^^(0)) of
                    Just True -> Just (hyperplane, hyperplaneDn)
                    _ -> Nothing
    maybeResult
        | isLeq = a `RA.leqReals` b
        | otherwise = fmap Prelude.not $ b `RA.leqReals` a
    distanceD = snd $ RA.doubleBounds $ a - b
    vaguenessD = snd $ RA.doubleBounds vagueness
    hyperplane
        = (t c, Map.map t coeffs)
    hyperplaneDn 
        = (tn cDnNeg, Map.map tn coeffsDnNeg)
    t [a] = a
    tn [a] = negate a
    (c, coeffs) = UFA.getAffineUpperBound $ a - b
    (cDnNeg, coeffsDnNeg) = UFA.getAffineUpperBound $ b - a
    vagueness 
        | Map.null coeffs = (t c + t cDnNeg)
        | otherwise =
            (t c + t cDnNeg) / (1 + avgSlope) -- average distance of upper and lower linear bound of a - b
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
    | Prelude.not boxSkewing =
        (prebox, Nothing, if gotHyperPlane then maybeSkewVar else Nothing)  
    | Prelude.not hyperplanesClose = 
        (prebox, Nothing, Nothing)  
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
            (TVMUndecided _ _ _ ((_, hp1) : (_, hp2) : _)) -> 
                (True, hp1, hp2)
            _ -> 
                (False, err, err)
        where
        err = error $ "PolyPaver.Logic: tryToSkew: internal error, tv = " ++ show tv

makeSplit splitGuessing varsNotToSplit maybeSplitVar ppb@(skewed, box, varIsInts, varNames) maybeSkewVar
    = (success, (ppbL, ppbR), splitVar)
    where
    -- perform split (potentially after skewing):
    success = Prelude.not $ (ppb `ppEqual` ppbL) Prelude.|| (ppb `ppEqual` ppbR)
    splitVar =
        case splitGuessing of
            Just ratioLimit ->
                case maybeSkewVar of
                    Just skewVar
                        | (fromIntegral ratioLimit) * skewVarWidth < largestWidth -> 
                            widestVar
                        | otherwise -> 
--                            unsafePrint 
--                            (
--                                "makeSplit: split direction from HP"
--                                ++ "\n ratioLimit = " ++ show ratioLimit
--                                ++ "\n skewVarWidth = " ++ show skewVarWidth
--                                ++ "\n largestWidth = " ++ show largestWidth
--                            ) $ 
                            skewVar
                        where
                        skewVarWidth =
                            case Map.lookup skewVar widths of Just w -> w
                    _ -> 
                        case maybeSplitVar of
                            Just splitVar -> splitVar
                            _ -> widestVar
            _ ->
                case maybeSplitVar of
                    Just splitVar -> splitVar
                    _ -> widestVar
                
    (largestWidth, widestVar) = foldl findWidestVar (0, err) $ Map.toList widths
        where
        err = 
            error $ "PolyPaver.Logic: split: failed to find a split for " ++ ppShow ppb 
        findWidestVar (prevWidth, prevRes) (var, currWidth)
            | currWidth `RA.leqReals` prevWidth == Just True = (prevWidth, prevRes)
            | otherwise = (currWidth, var)
    widths 
        = 
        foldl (Map.unionWith (+)) Map.empty $
            map (Map.map abs) $ map snd $ IMap.elems splittablesubbox
    splittablesubbox =
        foldr IMap.delete box varsNotToSplit
    (ppbL, ppbR)
        | Prelude.not skewed Prelude.&& splitVarIsIntvar =
            ((skewed, IMap.insert splitVar intSplitVarAffineL box, varIsInts, varNames),
             (skewed, IMap.insert splitVar intSplitVarAffineR box, varIsInts, varNames))
        | otherwise = 
            ((skewed, IMap.map substL box, varIsInts, varNames),
             (skewed, IMap.map substR box, varIsInts, varNames))
        where
        splitVarIsIntvar =
            IMap.lookup splitVar varIsInts == Just True
        intSplitVarAffineL = updateConstCoeff (lCeilRA, mFloorRA)
        intSplitVarAffineR = updateConstCoeff (mCeilRA, rFloorRA)
        updateConstCoeff (l, r) =
            (const, Map.insert splitVar slope coeffs)
            where
            (const, slope) = constSlopeFromRA (l,r)
            (_, coeffs) = intSplitVarAffine
        mCeilRA 
            | mCeil == mFloor = fromInteger $ mCeil + 1
            | otherwise = fromInteger mCeil
        mFloorRA = fromInteger mFloor
        (mCeil, mFloor) = shrinkIntervalToIntegerBounds mRA
        mRA = (lCeilRA + rFloorRA) / 2
        lCeilRA = fromInteger lCeil
        rFloorRA = fromInteger rFloor
        (lCeil, rFloor) = shrinkIntervalToIntegerBounds intSplitVarInterval
        intSplitVarInterval = affineUnivariateToInterval (splitVar, intSplitVarAffine) 
        Just intSplitVarAffine = IMap.lookup splitVar box
        
        substL (c, coeffs) =
            (lower $ c - varCoeffHalf, Map.insert splitVar varCoeffHalf coeffs)
            where
            varCoeffHalf
                =
                upper $
                case Map.lookup splitVar coeffs of Just cf -> cf / 2
        substR (c, coeffs) =
            (lower $ c + varCoeffHalf, Map.insert splitVar varCoeffHalf coeffs)
            where
            varCoeffHalf
                =
                upper $
                case Map.lookup splitVar coeffs of Just cf -> cf / 2
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
    leq lab form box a b = 
        TVDebugReport $
            banner
            ++ "\nLEQ [" ++ lab ++ "]:\n" ++ showForm form
            ++ "\n\nLHS:\n" ++ show a
            ++ "\n\nRHS:\n" ++ show b
            ++ "\n\nRESULT = " ++ show (a `RA.leqReals` b)
            where
            banner = "\n" ++ (concat $ replicate 50 "<=")
    less lab form box a b = 
        TVDebugReport $
            banner
            ++ "\nLE [" ++ lab ++ "]:\n" ++ showForm form
            ++ "\n\nLHS:\n" ++ show a
            ++ "\n\nRHS:\n" ++ show b
            ++ "\n\nRESULT = " ++ show (a `RA.leqReals` b)
            where
            banner = "\n" ++ (concat $ replicate 50 "<=")
    includes lab form box a b = 
        TVDebugReport $
            banner
            ++ "\nINCL [" ++ lab ++ "]:\n" ++ showForm form
            ++ "\n\nLHS:\n" ++ show b
            ++ "\n\nRHS:\n" ++ show a
            ++ "\n\nRESULT = " ++ show (a `RA.includes` b)
            where
            banner = "\n" ++ (concat $ replicate 100 "⊆")
    decide = error "TVDebugReport: `decide' not meaningful"
    split = error "TVDebugReport: `split' not meaningful"
            