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

import PolyPaver.APBox
import PolyPaver.Form

--import Numeric.ER.Misc
import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Base as B
import qualified Numeric.ER.RnToRm.UnitDom.Approx as UFA
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.RnToRm.DefaultRepr

--import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.IntMap as IMap

class (Eq l, Show l) => TruthValue tv l | tv -> l where
    not :: tv -> tv
    (&&) :: tv -> tv -> tv
    (||) :: tv -> tv -> tv
    (~>) :: tv -> tv -> tv
    fromBool :: FormLabel -> l -> APBox BM -> Bool -> tv
    leq :: FormLabel -> Form l -> APBox BM -> FAPUOI BM -> FAPUOI BM -> tv
    less :: FormLabel -> Form l -> APBox BM -> FAPUOI BM -> FAPUOI BM -> tv
    includes :: FormLabel -> Form l -> APBox BM -> FAPUOI BM -> FAPUOI BM -> tv
    bot :: Form l -> tv
    decide :: tv -> Maybe Bool
    split :: 
        (Eq l, Show l) => 
        [Int] -> -- vars that must not be split
        Maybe Int -> -- preferred variable to split
        APBox BM -> -- box to split
        tv -> -- undecided truth value that may be used to help guide splitting 
        (Bool, -- whether split succeeded in providing two proper sub-boxes 
         Int, -- variable whose domain was split
         (APBox BM, APBox BM))

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
        }
    
instance (Eq l, Show l) => Show (TVM l) where
    show (TVMDecided ares result) 
        = 
        "TVMDecided: " ++ show result 
        ++ "\n sub-results =\n" ++ unlines (map showAtomicResult ares) 
    show (TVMUndecided form dist ares)
        =
        "TVMUndecided"
        ++ "\n Sub-results: \n" ++ unlines (map showAtomicResult ares)
        ++ " Distance+vagueness: " ++ show dist
        ++ "\n Formula: " ++ showForm 1000 ignoreLabel form 
        ++ "\n Formula with ranges:\n" ++ showForm 10000 showLabel form 

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

instance (Eq l, Show l) => TruthValue (TVM l) l where
    not tv = tvmNot tv
    -- and:
    tv1@(TVMDecided _ False) && _ = tv1
    _tv1@(TVMDecided ares1 True) && tv2 = tv2 { tvmAtomicResults = ares1 ++ (tvmAtomicResults tv2) }
    tv1 && (TVMDecided ares2 False) = TVMDecided (tvmAtomicResults tv1 ++ ares2) False
    tv1 && (TVMDecided ares2 True) = tv1 { tvmAtomicResults = (tvmAtomicResults tv1) ++ ares2}
    (TVMUndecided form1 dist1 ares1) && (TVMUndecided form2 dist2 ares2) 
        = TVMUndecided (And form1 form2) (max dist1 dist2) (ares1 ++ ares2)
    -- or: 
    tv1@(TVMDecided _ares1 True) || _ = tv1
    (TVMDecided ares1 False) || tv2 = tv2 { tvmAtomicResults = ares1 ++ (tvmAtomicResults tv2)}
    tv1 || (TVMDecided ares2 True) = TVMDecided (tvmAtomicResults tv1 ++ ares2) True
    tv1 || (TVMDecided ares2 False) = tv1 { tvmAtomicResults = tvmAtomicResults tv1 ++ ares2 }
    (TVMUndecided form1 dist1 ares1) || (TVMUndecided form2 dist2 ares2)
        = TVMUndecided (Or form1 form2) (max dist1 dist2) (ares1 ++ ares2)
    -- implication:
    (TVMDecided ares1 False) ~> _ = TVMDecided ares1 True
    (TVMDecided ares1 True) ~> tv2 = tv2 { tvmAtomicResults = ares1 ++ (tvmAtomicResults tv2)}
    tv1 ~> (TVMDecided ares2 True) = TVMDecided (tvmAtomicResults tv1 ++ ares2) True
    tv1 ~> (TVMDecided ares2 False) = tvmNot tv1 { tvmAtomicResults = tvmAtomicResults tv1 ++ ares2 }
    (TVMUndecided form1 dist1 ares1) ~> (TVMUndecided form2 dist2 ares2)
        = TVMUndecided (Implies form1 form2) (max dist1 dist2) (ares1 ++ ares2)

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
            _ -> TVMUndecided form (distanceD + vaguenessD) [(lab, ares Nothing)] 
--        case a `RA.includes` b of
--            Just res -> TVMDecided res
--            _ -> TVMUndecided form 1 []
        where
        ares maybeResult = (maybeResult, distanceD, vaguenessD)
        distanceD = foldl1 max [distanceDL, distanceDR, distanceDU, distanceDD]
        vaguenessD = foldl1 max [vaguenessDL, vaguenessDR, vaguenessDU, vaguenessDD]
        (maybeResultL, distanceDL, vaguenessDL, _vaguenessL) 
            = analyseLeqLess True box aiL boL -- testing for truth (part 1)
        (maybeResultR, distanceDR, vaguenessDR, _vaguenessR) 
            = analyseLeqLess True box boR aiR -- testing for truth (part 2)
        (maybeResultU, distanceDU, vaguenessDU, _) 
            = analyseLeqLess True box aoR boL -- testing for falsity due to b > a
        (maybeResultD, distanceDD, vaguenessDD, _) 
            = analyseLeqLess True box boR aoL -- testing for falsity due to b < a
        ((aoL,aoR),(aiL,aiR)) = RA.oiBounds a
        ((boL,boR),_) = RA.oiBounds b 
    bot form = TVMUndecided form (1/0) [] -- infinite badness...
    decide (TVMDecided _ result) = Just result
    decide (TVMUndecided _ _ _) = Nothing
    
    split varsNotToSplit maybeVar box _tv
        = 
        (success, splitVar, boxes)
        where
        (success, boxes, splitVar)    
            = makeSplit varsNotToSplit maybeVar box
            
tvmNot :: TVM l -> TVM l
tvmNot (TVMDecided ares x) = TVMDecided ares (Prelude.not x)
tvmNot (TVMUndecided form dist ares) = TVMUndecided (Not form) dist ares
            
tvmLeqLess ::
    (Eq l, Show l) => 
    Bool ->
    FormLabel -> Form l -> APBox BM -> FAPUOI BM -> FAPUOI BM -> TVM l
tvmLeqLess isLeq lab form box a b = 
    case maybeResult of
        Just result -> 
            TVMDecided [(lab, (maybeResult, distanceD, vaguenessD))] result
        Nothing ->
            TVMUndecided form distanceVagueness 
                [(lab, (Nothing, distanceD, vaguenessD))] 
    where
    distanceVagueness = distanceD + vaguenessD 
    (maybeResult, distanceD, vaguenessD, _vagueness) = analyseLeqLess isLeq box a b

analyseLeqLess :: 
    UFA.ERUnitFnApprox box varid domra ranra fa =>
    Bool -> 
    box2 -> 
    fa -> 
    fa -> 
    (Maybe Bool,
        Double,
        Double,
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
    (maybeResult, distanceD, vaguenessD, vagueness)
    where
    maybeResult
        | isLeq = a `RA.leqReals` b
        | otherwise = fmap Prelude.not $ b `RA.leqReals` a
    distanceD = snd $ RA.doubleBounds $ a - b
    vaguenessD = snd $ RA.doubleBounds vagueness
    t [x] = x
    t _ = error "analyseLeqLess: t: invalid parameter"
    (c, coeffs) =
--        unsafePrint
--        (
--            "analyseLeqLess: affine: "
--            ++ "\n a = " ++ show a
--            ++ "\n b = " ++ show b
--            ++ "\n UFA.getAffineUpperBound $ a - b = " ++ (show $ UFA.getAffineUpperBound $ a - b)
--        ) $ 
        UFA.getAffineUpperBound $ a - b
    (cDnNeg, _coeffsDnNeg) = UFA.getAffineUpperBound $ b - a
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
    

makeSplit :: 
    (B.ERRealBase b) =>
    [IMap.Key] -> 
    Maybe Int ->
    APBox b ->
    (Bool, (APBox b, APBox b), Int)
makeSplit varsNotToSplit maybeSplitVar ppb@(box, varIsInts, varNames)
    = (success, (ppbL, ppbR), splitVar)
    where
    success = Prelude.not $ (ppb `boxEqual` ppbL) Prelude.|| (ppb `boxEqual` ppbR)
    splitVar =
        case maybeSplitVar of
            Just splitVar2 -> splitVar2
            _ -> widestVar
                
    (_largestWidth, widestVar) = foldl findWidestVar (0, err) $ widths
        where
        err = 
            error $ "PolyPaver.Logic: split: failed to find a split for " ++ showBox ppb 
        findWidestVar (prevWidth, prevRes) (var, currWidth)
            | currWidth `RA.leqReals` prevWidth == Just True = (prevWidth, prevRes)
            | otherwise = (currWidth, var)
    widths 
        = 
            map (\(var,(_c,r)) -> (var, abs r)) $ IMap.toList splittablesubbox
    splittablesubbox =
        foldr IMap.delete box varsNotToSplit
    (ppbL, ppbR) =
        ((IMap.insert splitVar splitVarCenterRadiusL box, varIsInts, varNames),
         (IMap.insert splitVar splitVarCenterRadiusR box, varIsInts, varNames))
        where
        splitVarCenterRadiusL = centerRadiusFromEndpoints (ll, lr)
        splitVarCenterRadiusR = centerRadiusFromEndpoints (rl, rr)
        
        ((ll, lr), (rl, rr))
            | splitVarIsIntvar = ((lCeilRA, mFloorRA), (mCeilRA, rFloorRA))
            | otherwise = ((l,mR),(mL,r))
        splitVarIsIntvar =
            IMap.lookup splitVar varIsInts == Just True
        
        -- normal split:
        (mL, mR) = RA.bounds m
        m = (l + r) /2
        (l,r) = RA.bounds splitVarInterval
        
        -- integer split:
        mCeilRA
            | (mCeil == mFloor) Prelude.&& (mCeil + 1 <= rFloor) = fromInteger $ mCeil + 1
            | otherwise = fromInteger mCeil
        mFloorRA = fromInteger mFloor
        (mCeil, mFloor) = shrinkIntervalToIntegerBounds mRA
        mRA = (lCeilRA + rFloorRA) / 2
        lCeilRA = fromInteger lCeil
        rFloorRA = fromInteger rFloor
        (lCeil, rFloor) = shrinkIntervalToIntegerBounds splitVarInterval
        
        splitVarInterval = centerRadiusToInterval splitVarCenterRadius 
        Just splitVarCenterRadius = IMap.lookup splitVar box

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
            