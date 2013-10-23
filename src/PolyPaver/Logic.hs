{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

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

--import Numeric.ER.Misc
import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Base as B
import qualified Numeric.ER.RnToRm.UnitDom.Approx as UFA
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.RnToRm.DefaultRepr

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.IntMap as IMap

class (HasDefaultValue l, Eq l, Show l) => TruthValue tv l | tv -> l where
    not :: tv -> tv
    (&&) :: tv -> tv -> tv
    (||) :: tv -> tv -> tv
    (~>) :: tv -> tv -> tv
    fromBool :: FormLabel -> l -> PPBox BM -> Bool -> tv
    leq :: FormLabel -> Form l -> PPBox BM -> FAPUOI BM -> FAPUOI BM -> tv
    less :: FormLabel -> Form l -> PPBox BM -> FAPUOI BM -> FAPUOI BM -> tv
    includes :: FormLabel -> Form l -> PPBox BM -> FAPUOI BM -> FAPUOI BM -> tv
    bot :: Form l -> tv
    decide :: tv -> Maybe Bool
    split :: 
        (HasDefaultValue l, Eq l, Show l) => 
        [Int] -> -- vars that must not be split
        Maybe Int -> -- preferred variable to split
        PPBox BM -> -- box to split
        Bool -> -- True to allow skewing 
        Maybe Int -> -- maybe allow split direction guessing but limit box width ratio to n 
        tv -> -- undecided truth value that may be used to help guide splitting and/or skewing 
        (Bool, -- whether split succeeded in providing two proper sub-boxes 
         Maybe ((BoxHyperPlane BM, BoxHyperPlane BM), Form l, FormLabel, IRA BM), -- whether box skewing has been used
         Int, -- variable whose domain was split
         (PPBox BM, PPBox BM))

data TVM l
    = TVMDecided 
        {
            tvmAtomicResults :: [(FormLabel, (Maybe Bool, Double, Double))] -- result, distance, vagueness
        ,   tvmResult :: Bool 
        } 
    | TVMUndecided 
        { 
            tvmSimplifiedFormula :: Form l
        ,   tvmDistanceFromTruth :: Double
        ,   tvmAtomicResults :: [(FormLabel, (Maybe Bool, Double, Double))] -- result, distance, vagueness
        ,   tvmDecisionHyperPlanes :: [(Double, ((BoxHyperPlane BM, BoxHyperPlane BM), Form l, FormLabel, IRA BM))] 
            -- the first one is the best one, keeping its measure, formula and vagueness 
        }
    
instance (HasDefaultValue l, Eq l, Show l) => Show (TVM l) where
    show (TVMDecided ares result) 
        = 
        "TVMDecided: " ++ show result 
        ++ "\n sub-results =\n" ++ unlines (map showAtomicResult ares) 
    show (TVMUndecided form dist ares hps)
        =
        "TVMUndecided"
        ++ "\n Sub-results: \n" ++ unlines (map showAtomicResult ares)
        ++ " Distance+vagueness: " ++ show dist
        ++ "\n Formula: " ++ showForm 1000 ignoreLabel form 
        ++ "\n Formula with ranges:\n" ++ showForm 10000 showLabel form 
        ++ "\n Hyperplanes: "
            ++ (case hps of
                    [] -> "none"
                    _ -> (unlines $ map showHP $ zip [1..] hps))
        where
        showHP (n,(_dist, ((hp, hpDn), hpForm, hpLabel, vagueness)))
            =
            "\n   hp" ++ show (n :: Int) ++ ":"
            ++ "label = " ++ hpLabel
            ++ "; dist = " ++ show dist
            ++ "; vagueness = " ++ show vagueness
            ++ "; hp = " ++ showAffine hp
            ++ "; hpDn = " ++ showAffine hpDn
            ++ "; form = " ++ showForm 1000 ignoreLabel hpForm

ignoreLabel :: String -> l -> String
ignoreLabel = const
showLabel :: Show l => String -> l -> String
showLabel s l = s ++ (show l) 

showAtomicResult :: 
    (Show a1, Show a2, Show a3) =>
    (FormLabel, (a1, a2, a3)) -> 
    String
showAtomicResult (lab, (maybeResult, distanceD, vaguenessD)) =
    "    " ++ lab ++ ": " 
    ++ show maybeResult ++ ", distance = " ++ show distanceD ++ ", vagueness = " ++ show vaguenessD

instance (HasDefaultValue l, Eq l, Show l) => TruthValue (TVM l) l where
    not tv = tvmNot tv
    -- and:
    tv1@(TVMDecided _ False) && _ = tv1
    _tv1@(TVMDecided ares1 True) && tv2 = tv2 { tvmAtomicResults = ares1 ++ (tvmAtomicResults tv2) }
    tv1 && (TVMDecided ares2 False) = TVMDecided (tvmAtomicResults tv1 ++ ares2) False
    tv1 && (TVMDecided ares2 True) = tv1 { tvmAtomicResults = (tvmAtomicResults tv1) ++ ares2}
    (TVMUndecided form1 dist1 ares1 hps1) && (TVMUndecided form2 dist2 ares2 hps2) 
        = TVMUndecided (And form1 form2) (max dist1 dist2) (ares1 ++ ares2) (combineHPs hps1 hps2)
    -- or: 
    tv1@(TVMDecided _ares1 True) || _ = tv1
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

    fromBool lab _ _ b = TVMDecided [(lab, (Just b, 1, 0))] b
    leq = tvmLeqLess True
    less = tvmLeqLess False
    includes lab form box a b -- b `ContainedIn` a
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
                    -> [(distanceDL + vaguenessDL, (hyperplaneL, form, lab, vaguenessL)), 
                        (distanceDR + vaguenessDR, (hyperplaneR, form, lab, vaguenessR))]
                (Just hyperplaneL, _) -> [(distanceDL + vaguenessDL, (hyperplaneL, form, lab, vaguenessL))]
                (_, Just hyperplaneR) -> [(distanceDR + vaguenessDR, (hyperplaneR, form, lab, vaguenessR))]
                _ -> []
        (maybeResultL, distanceDL, vaguenessDL, maybeHyperplaneL, vaguenessL) 
            = analyseLeqLess True box aiL boL -- testing for truth (part 1)
        (maybeResultR, distanceDR, vaguenessDR, maybeHyperplaneR, vaguenessR) 
            = analyseLeqLess True box boR aiR -- testing for truth (part 2)
        (maybeResultU, distanceDU, vaguenessDU, _maybeHyperplaneU, _) 
            = analyseLeqLess True box aoR boL -- testing for falsity due to b > a
        (maybeResultD, distanceDD, vaguenessDD, _maybeHyperplaneD, _) 
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
            
tvmNot :: TVM l -> TVM l
tvmNot (TVMDecided ares x) = TVMDecided ares (Prelude.not x)
tvmNot (TVMUndecided form dist ares hps) = TVMUndecided (Not form) dist ares hps
            
combineHPs :: Ord a => [(a, t)] -> [(a, t)] -> [(a, t)]
combineHPs hps1 hps2
    = List.sortBy (\(m1, _) (m2, _) -> compare m1 m2) $ hps1 ++ hps2 

tvmLeqLess ::
    (HasDefaultValue l, Eq l, Show l) => 
    Bool ->
    FormLabel -> Form l -> PPBox BM -> FAPUOI BM -> FAPUOI BM -> TVM l
tvmLeqLess isLeq lab form box a b = 
    case maybeResult of
        Just result -> TVMDecided [(lab, (maybeResult, distanceD, vaguenessD))] result
        Nothing ->
            case maybeHyperplane of
                Just hyperplane -> 
                    TVMUndecided form distanceVagueness
                        [(lab, (Nothing, distanceD, vaguenessD))] 
                        [(distanceVagueness, (hyperplane, form, lab, vagueness))]
                _ -> 
                    TVMUndecided form distanceVagueness 
                        [(lab, (Nothing, distanceD, vaguenessD))] 
                        []
    where
    distanceVagueness = distanceD + vaguenessD 
    (maybeResult, distanceD, vaguenessD, maybeHyperplane, vagueness) = analyseLeqLess isLeq box a b

analyseLeqLess :: 
    UFA.ERUnitFnApprox box varid domra ranra fa =>
    Bool -> 
    box2 -> 
    fa -> 
    fa -> 
    (Maybe Bool,
        Double,
        Double,
        Maybe ((ranra, Map.Map varid ranra), (ranra, Map.Map varid ranra)),
        ranra)
analyseLeqLess isLeq _box a b =
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
                case vagueness `RA.leqReals` (2^^(0 :: Int)) of
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
    t [x] = x
    t _ = error "analyseLeqLess: t: invalid parameter"
    tn [x] = negate x
    tn _ = error "analyseLeqLess: tn: invalid parameter"
    (c, coeffs) =
--        unsafePrint
--        (
--            "analyseLeqLess: affine: "
--            ++ "\n a = " ++ show a
--            ++ "\n b = " ++ show b
--            ++ "\n UFA.getAffineUpperBound $ a - b = " ++ (show $ UFA.getAffineUpperBound $ a - b)
--        ) $ 
        UFA.getAffineUpperBound $ a - b
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
    
tryToSkew :: 
    (Eq l, Show l, HasDefaultValue l) =>
    Bool -> 
    PPBox BM -> 
    TVM l -> 
    (PPBox BM,
     Maybe ((BoxHyperPlane BM, BoxHyperPlane BM), Form l, FormLabel, IRA BM),
     Maybe Int)
tryToSkew boxSkewing prebox tv
    | Prelude.not boxSkewing =
        (prebox, Nothing, if gotHyperPlane then maybeSkewVar else Nothing)  
    | Prelude.not hyperplanesClose = 
        (prebox, Nothing, Nothing)  
    | otherwise =
        (skewedBox, Just hp1, maybeSkewVar)
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
    ((hyperplane1,_), _, _, _) = hp1
    ((hyperplane2,_), _, _, _) = hp2
    (gotHyperPlane, hp1, hp2)
        = case tv of
            (TVMUndecided _ _ _ ((_, hp1_) : (_, hp2_) : _)) -> 
                (True, hp1_, hp2_)
            _ -> 
                (False, err, err)
        where
        err = error $ "PolyPaver.Logic: tryToSkew: internal error, tv = " ++ show tv

makeSplit :: 
    (B.ERRealBase b) =>
    Maybe Int -> 
    [IMap.Key] -> 
    Maybe Int ->
    PPBox b ->
    Maybe Int -> 
    (Bool, (PPBox b, PPBox b), Int)
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
                            case Map.lookup skewVar widths of 
                                Just w -> w
                                _ -> error $ "makeSplit: splitVar: variable " ++ show skewVar ++ " not found in " ++ show widths
                    _ -> 
                        case maybeSplitVar of
                            Just splitVar2 -> splitVar2
                            _ -> widestVar
            _ ->
                case maybeSplitVar of
                    Just splitVar2 -> splitVar2
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
            (c, Map.insert splitVar slope coeffs)
            where
            (c, slope) = constSlopeFromRA (l,r)
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
                case Map.lookup splitVar coeffs of 
                    Just cf -> cf / 2
                    _ -> error $ "makeSplit: substL: variable " ++ show splitVar ++ " not found in " ++ show coeffs
        substR (c, coeffs) =
            (lower $ c + varCoeffHalf, Map.insert splitVar varCoeffHalf coeffs)
            where
            varCoeffHalf
                =
                upper $
                case Map.lookup splitVar coeffs of 
                    Just cf -> cf / 2
                    _ -> error $ "makeSplit: substR: variable " ++ show splitVar ++ " not found in " ++ show coeffs
        lower i = fst $ RA.bounds i
        upper i = snd $ RA.bounds i

data TVDebugReport = TVDebugReport String
    
instance TruthValue TVDebugReport () where
    not tv = tv
    (TVDebugReport r1) && (TVDebugReport r2) = TVDebugReport $ r1 ++ r2 
    (TVDebugReport r1) || (TVDebugReport r2) = TVDebugReport $ r1 ++ r2 
    (TVDebugReport r1) ~> (TVDebugReport r2) = TVDebugReport $ r1 ++ r2 
    fromBool lab _ _ b = TVDebugReport $ "BOOL [" ++ lab ++ "]: " ++ show b 
    bot _form = TVDebugReport ""
    leq _lab form _box a b = 
        TVDebugReport $
            banner
            ++ "\nLEQ:\n" ++ showForm 1000 showLabel form
            ++ "\n\nLHS:\n" ++ show a
            ++ "\n\nRHS:\n" ++ show b
            ++ "\n\nRESULT = " ++ show (a `RA.leqReals` b)
            where
            banner = "\n" ++ (concat $ replicate 50 "<=")
    less _lab form _box a b = 
        TVDebugReport $
            banner
            ++ "\nLE:\n" ++ showForm 1000 showLabel form
            ++ "\n\nLHS:\n" ++ show a
            ++ "\n\nRHS:\n" ++ show b
            ++ "\n\nRESULT = " ++ show (a `RA.leqReals` b)
            where
            banner = "\n" ++ (concat $ replicate 50 "<=")
    includes _lab form _box a b = 
        TVDebugReport $
            banner
            ++ "\nINCL:\n" ++ showForm 1000 showLabel form
            ++ "\n\nLHS:\n" ++ show b
            ++ "\n\nRHS:\n" ++ show a
            ++ "\n\nRESULT = " ++ show (a `RA.includes` b)
            where
            banner = "\n" ++ (concat $ replicate 100 "⊆")
    decide = error "TVDebugReport: `decide' not meaningful"
    split = error "TVDebugReport: `split' not meaningful"
            