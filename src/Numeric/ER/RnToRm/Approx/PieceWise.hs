{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-|
    Module      :  Numeric.ER.RnToRm.Approx.PieceWise
    Description :  arbitrary precision piece-wise-something function enclosures
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision piece-wise something 
    (eg linear, polynomial, rational) enclosures 
    of functions @R^n->R^m@.
    
    The type of approximation within segments is specified
    by an instance of 'FA.ERFnDomApprox'.
        
    The piece-wise construction defines another instance of 'FA.ERFnDomApprox'.
-}
module Numeric.ER.RnToRm.Approx.PieceWise 
(
    ERFnPiecewise(..)
)
where

import qualified Numeric.ER.RnToRm.BisectionTree as BISTR
import qualified Numeric.ER.RnToRm.BisectionTree.Integration as BTINTEG

import qualified Numeric.ER.RnToRm.Approx as FA 
import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Approx.Elementary as RAEL

import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainBoxMappable, DomainIntBox)
import Numeric.ER.BasicTypes
import Numeric.ER.Misc

import qualified Text.Html as H

import Data.Typeable
import Data.Generics.Basics
import Data.Binary

import Data.Maybe

{-|
    Arbitrary precision piece-wise something 
    (eg linear, polynomial, rational) enclosures 
    of functions @R^n->R^m@.
    
    The type of approximation within segments is specified
    by an instance of 'FA.ERFnDomApprox'.
        
    The piece-wise construction defines another instance of 'FA.ERFnDomApprox'.
-}
data ERFnPiecewise box varid domra fa = 
    ERFnPiecewise (BISTR.BisectionTree box varid domra fa)
    deriving (Typeable, Data)
    
instance (Binary a, Binary b, Binary c, Binary d) => Binary (ERFnPiecewise a b c d) where
  put (ERFnPiecewise a) = put a
  get = get >>= \a -> return (ERFnPiecewise a)
    
pwLift1 ::
    (DomainBox box varid domra) =>
    (fa -> fa) ->
    (ERFnPiecewise box varid domra fa) -> 
    (ERFnPiecewise box varid domra fa)
pwLift1 op (ERFnPiecewise bistr) =    
    ERFnPiecewise (BISTR.mapWithDom (const op) bistr)
        
pwLift2 ::
    (RA.ERIntApprox domra, FA.ERFnDomApprox box varid domra ranra fa) =>
    (fa -> fa -> fa) ->
    EffortIndex ->
    (ERFnPiecewise box varid domra fa) -> 
    (ERFnPiecewise box varid domra fa) -> 
    (ERFnPiecewise box varid domra fa)
pwLift2 op ix f1@(ERFnPiecewise bistr1) f2@(ERFnPiecewise bistr2) =
    ERFnPiecewise $ 
        fromJust $ fst $ 
            BISTR.combineWith faSplit faSplit opBistr ix bistr1 bistr2
    where
    opBistr domB val1 val2 =
        (Just $ op val1 val2, [])
        
pwbistrZipWith ::
    (RA.ERIntApprox domra, FA.ERFnDomApprox box varid domra ranra fa) =>
    (fa -> fa -> res) ->
    EffortIndex ->
    (BISTR.BisectionTree box varid domra fa) ->
    (BISTR.BisectionTree box varid domra fa) ->
    (BISTR.BisectionTree box varid domra res)
pwbistrZipWith op ix bistr1 bistr2 =
    fromJust $ fst $ 
        BISTR.combineWith faSplit faSplit opBistr ix bistr1 bistr2    
    where
    opBistr domB val1 val2 =
        (Just $ op val1 val2, [])

pwSplit ::
    (RA.ERIntApprox domra, DomainBox box varid domra) =>
    (fa -> (fa, fa)) ->
    (ERFnPiecewise box varid domra fa) -> (ERFnPiecewise box varid domra fa, ERFnPiecewise box varid domra fa)
pwSplit op f@(ERFnPiecewise bistr) = 
    (ERFnPiecewise bistr1, ERFnPiecewise bistr2)
    where
    bistr1 = BISTR.mapWithDom (const fst) bistr12 
    bistr2 = BISTR.mapWithDom (const snd) bistr12 
    bistr12 = BISTR.mapWithDom (const op) bistr

faSplit :: 
    (RA.ERIntApprox domra, FA.ERFnDomApprox box varid domra ranra fa) =>
    BISTR.ValueSplitter box varid domra fa
faSplit ix depth domB fa var pt = 
    FA.bisect var (Just pt) fa 
    
faCombine ::
    (RA.ERIntApprox domra, FA.ERFnDomApprox box varid domra ranra fa) =>
    BISTR.ValueCombiner box varid domra fa
faCombine ix dp (BISTR.Leaf _ _ v) = v
faCombine ix dp (BISTR.Node _ dom x pt lo hi) =
    FA.unBisect x (vLO, vHI)
    where
    vLO = faCombine ix (dp + 1) lo   
    vHI = faCombine ix (dp + 1) hi   
    
instance 
    (FA.ERFnDomApprox box varid domra ranra fa, VariableID varid) =>
    Show (ERFnPiecewise box varid domra fa)
    where
    show f@(ERFnPiecewise bistr) =
        "\nERFnPiecewise:" ++ show bistr

instance 
    (FA.ERFnDomApprox box varid domra ranra fa, RA.ERIntApprox fa, H.HTML fa) =>
    H.HTML (ERFnPiecewise box varid domra fa)
    where
    toHtml (ERFnPiecewise bistr) =
        H.toHtml bistr

instance
    (FA.ERFnDomApprox box varid domra ranra fa) =>
    Eq (ERFnPiecewise box varid domra fa)
    where
    (ERFnPiecewise bistr1) == (ERFnPiecewise bistr2) =
        error $
            "ERFnPiecewise: Eq: not implemented yet"

instance
    (FA.ERFnDomApprox box varid domra ranra fa) =>
    Ord (ERFnPiecewise box varid domra fa)
    where
    compare (ERFnPiecewise bistr1) (ERFnPiecewise bistr2) =
        error $
            "ERFnPiecewise: Ord: not implemented yet"

instance
    (FA.ERFnDomApprox box varid domra ranra fa, VariableID varid) =>
    Num (ERFnPiecewise box varid domra fa)
    where
    fromInteger n = ERFnPiecewise $ BISTR.const DBox.noinfo (fromInteger n)
    negate = pwLift1 negate
    (+) = pwLift2 (+) 10
    (*) = pwLift2 (*) 10

instance
    (FA.ERFnDomApprox box varid domra ranra fa, VariableID varid) =>
    Fractional (ERFnPiecewise box varid domra fa)
    where
    fromRational r = ERFnPiecewise $ BISTR.const DBox.noinfo (fromRational r)
    recip = pwLift1 recip

instance 
    (FA.ERFnDomApprox box varid domra ranra fa, VariableID varid) =>
    RA.ERApprox (ERFnPiecewise box varid domra fa)
    where
    initialiseBaseArithmetic _ =
    	RA.initialiseBaseArithmetic (0 :: fa)
    getGranularity (ERFnPiecewise bistr) =
        foldl max 10 $ map RA.getGranularity $ BISTR.collectValues bistr
    setGranularityOuter gran = pwLift1 (RA.setGranularityOuter gran) 
    setMinGranularityOuter gran = pwLift1 (RA.setMinGranularityOuter gran)
    isBounded (ERFnPiecewise bistr) =
        and $ map RA.isBounded $ BISTR.collectValues bistr
    f1 /\ f2 = pwLift2 (RA./\) 10 f1 f2
    intersectMeasureImprovement ix f1@(ERFnPiecewise bistr1) f2@(ERFnPiecewise bistr2) =
--        unsafePrint
--        (
--            "ERFnPiecewise: intersectMeasureImprovement:"
--            ++ "\n f1 = " ++ show f1
--            ++ "\n f2 = " ++ show f2
--            ++ "\n isect = " ++ show (ERFnPiecewise bistrIsect)
--            ++ "\n impr = " ++ show (ERFnPiecewise bistrImpr)
--        )
--        | length fas1 == length fas2 =
        (ERFnPiecewise bistrIsect, ERFnPiecewise bistrImpr)
--        | otherwise =
--            error $ show $ f1 RA./\ f2 
        where
        bistrIsect = BISTR.mapWithDom (const fst) bistrIsectImpr 
        bistrImpr = BISTR.mapWithDom (const snd) bistrIsectImpr
        bistrIsectImpr = pwbistrZipWith (RA.intersectMeasureImprovement ix) ix bistr1 bistr2 
    leqReals f1@(ERFnPiecewise bistr1) f2@(ERFnPiecewise bistr2) =
--        | length fas1 == length fas2 =
            leqTuple $ BISTR.collectValues $ pwbistrZipWith (RA.leqReals) 10 bistr1 bistr2
--        | otherwise =
--            error $ show $ f1 RA./\ f2
        where
        leqTuple [] = Just True
        leqTuple (tv : tvs) =
            case and $ map (== tv) tvs of
                True -> tv
                _ -> Nothing  
    refines f1@(ERFnPiecewise bistr1) f2@(ERFnPiecewise bistr2) =
        and $ BISTR.collectValues $ pwbistrZipWith (RA.refines) 10 bistr1 bistr2
    compareApprox f1@(ERFnPiecewise bistr1) f2@(ERFnPiecewise bistr2) =
        BISTR.compare RA.compareApprox RA.compareApprox bistr1 bistr2
instance 
    (FA.ERFnDomApprox box varid domra ranra fa, RA.ERIntApprox fa, VariableID varid) =>
    RA.ERIntApprox (ERFnPiecewise box varid domra fa)
    where
--    doubleBounds = :: ira -> (Double, Double) 
--    floatBounds :: ira -> (Float, Float)
--    integerBounds :: ira -> (ExtendedInteger, ExtendedInteger)
    bisectDomain maybePt f@(ERFnPiecewise bistr) =
        case maybePt of
            Nothing ->
                pwSplit (RA.bisectDomain Nothing) f
            Just (ERFnPiecewise bistrPt) -> 
                (ERFnPiecewise bistr1, ERFnPiecewise bistr2)
                where
                bistr1 = BISTR.mapWithDom (const fst) bistr12 
                bistr2 = BISTR.mapWithDom (const snd) bistr12 
                bistr12 =
                        pwbistrZipWith (\fa pt -> RA.bisectDomain (Just pt) fa) 10 
                            bistr bistrPt
    bounds = pwSplit RA.bounds
    f1 \/ f2 = pwLift2 (RA.\/) 10 f1 f2
    
instance 
    (FA.ERFnDomApprox box varid domra ranra fa, RAEL.ERApproxElementary fa, VariableID varid) =>
    RAEL.ERApproxElementary (ERFnPiecewise box varid domra fa)
    where
    abs ix = pwLift1 $ RAEL.abs ix
    sqrt ix = pwLift1 $ RAEL.sqrt ix
    exp ix = pwLift1 $ RAEL.exp ix
    log ix = pwLift1 $ RAEL.log ix
    sin ix = pwLift1 $ RAEL.sin ix
    cos ix = pwLift1 $ RAEL.cos ix
    atan ix = pwLift1 $ RAEL.atan ix
    
instance 
    (FA.ERFnDomApprox box varid domra ranra fa, 
     RA.ERIntApprox fa, 
     DomainBoxMappable box box varid domra domra, 
     Show box) =>
    FA.ERFnApprox box varid domra ranra (ERFnPiecewise box varid domra fa)
    where
    check prgLocation (ERFnPiecewise bistr) =
        ERFnPiecewise $ BISTR.mapWithDom checkSegm bistr
        where
        checkSegm dom f =
            FA.check (prgLocation ++ "segm " ++ show dom ++ ": ") f
    domra2ranra (ERFnPiecewise bistr) d =
        FA.domra2ranra fa d
        where
        (fa : _) = BISTR.collectValues bistr
    ranra2domra (ERFnPiecewise bistr) r =
        FA.ranra2domra fa r
        where
        (fa : _) = BISTR.collectValues bistr
    setMaxDegree maxDegree = pwLift1 (FA.setMaxDegree maxDegree)
    setMaxSize maxSize = pwLift1 (FA.setMaxSize maxSize)
    getRangeApprox (ERFnPiecewise bistr) =
        foldl1 (zipWith (RA.\/)) $ map FA.getRangeApprox $ BISTR.collectValues bistr
    getTupleSize (ERFnPiecewise bistr) =
        FA.getTupleSize $ head $ BISTR.collectValues bistr
    tuple fs =
        foldl1 (pwLift2 (\a b -> FA.tuple [a,b]) 10) fs
    applyTupleFn tupleFn = pwLift1 $ FA.applyTupleFn tupleFnNoPW 
        where
        tupleFnNoPW fas =
            map (\ (ERFnPiecewise (BISTR.Leaf _ _ fa)) -> fa ) $
                tupleFn $
                    map (\fa -> ERFnPiecewise $ BISTR.Leaf 0 (FA.dom fa) fa) 
                        fas
        err = error "ERFnPiecewise: applyTupleFn"
    volume (ERFnPiecewise bistr) = 
        sum $ map FA.volume $ BISTR.collectValues bistr
    scale ratio = pwLift1 (FA.scale ratio)
    partialIntersect ix substitutions 
            f1@(ERFnPiecewise bistr1) 
            f2@(ERFnPiecewise bistr2) =
        ERFnPiecewise $ 
            head $
                BTINTEG.zipOnSubdomain 
                    faSplit ix maxDepth substitutions
                    updateInside updateTouch updateAway 
                    [bistr1, bistr2]
        where
        maxDepth = effIx2int ix
        updateInside dom [val1, val2] =
            [FA.partialIntersect ix substitutions val1 val2]
        updateTouch = updateInside
        updateAway dom [val1, val2] =
            [val2]
    eval ptBox (ERFnPiecewise bistr) =
        foldl1 (zipWith (RA./\)) $ 
--            unsafePrintReturn ("ERFnPiecewise: eval: vals = ")$
            map (\fa -> FA.eval ptBox fa) $
                concat $ map BISTR.collectValues $ 
                    BISTR.lookupLeavesIntersectingDom bistr ptBox 
    evalInner ptBox (ERFnPiecewise bistr) =
        foldl1 (zipWith (RA./\)) $ 
--            unsafePrintReturn ("ERFnPiecewise: evalInner: vals = ")$
            map (\fa -> FA.evalInner ptBox fa) $
                concat $ map BISTR.collectValues $ 
                    BISTR.lookupLeavesIntersectingDom bistr ptBox 
    partialEval substitutions f@(ERFnPiecewise bistr) = 
        pwLift1 (FA.partialEval substitutions) (ERFnPiecewise bistrNoVars)
        where
        bistrNoVars =
            BISTR.removeVars substitutions bistr
    composeNonDecreasing
        fOuter@(ERFnPiecewise bistrOuter)
        varid
        fInner@(ERFnPiecewise bistrInner)
        =
--        unsafePrintReturn
--        (
--          "PieceWise: composeNonDecreasing: "
--          ++ "\n fOuter = " ++ show fOuter
--          ++ "\n fInner = " ++ show fInner
--          ++ "\n result = "
--        ) $
        ERFnPiecewise $ BISTR.mapLeaves composeLeaf bistrInner
        where
        composeLeaf leaf@(BISTR.Leaf _ _ vInner) =
            leaf { BISTR.bistrVal = vComposed }
            where
            vComposed = 
                FA.composeNonDecreasing vOuter varid vInner 
            vOuter = faCombine 10 (BISTR.bistrDepth bistrOuter) bistrOuterRelevant
            bistrOuterRelevant = BISTR.restrictToDom bistrOuter composeDomB
            composeDomB = DBox.insert varid composeDomVar $ BISTR.bistrDom bistrOuter
            composeDomVar = FA.ranra2domra fInner $ foldl1 (RA.\/) $ FA.getRangeApprox fInner 
    intersectMeasureImprovement ix f1@(ERFnPiecewise bistr1) f2@(ERFnPiecewise bistr2) =
        (intersection, improvementRA)
        where
        (intersection, _) = RA.intersectMeasureImprovement ix f1 f2
        improvementRA 
            | 0 `RA.refines` intersectionVolume && 0 `RA.refines` f1Volume = 1
--                error $ 
--                    "ERFnInterval: intersectMeasureImprovement: inconsistent result: " 
--                    ++ show intersection
            | otherwise =
                 f1Volume / intersectionVolume
        intersectionVolume = FA.volume intersection
        f1Volume = FA.volume f1
instance 
    (FA.ERFnDomApprox box varid domra ranra fa, RA.ERIntApprox fa, Show box, 
     DomainBoxMappable box box varid domra domra) =>
    FA.ERFnDomApprox box varid domra ranra (ERFnPiecewise box varid domra fa)
    where
    dom (ERFnPiecewise bistr) = BISTR.bistrDom bistr
    bottomApprox domB tupleSize =
        ERFnPiecewise (BISTR.const domB $ FA.bottomApprox domB tupleSize)
    const domB vals =
        ERFnPiecewise $
            BISTR.const domB $ FA.const domB vals
    proj domB i =
        ERFnPiecewise $ BISTR.Leaf 0 domB $ FA.proj domB i 
    bisect var maybePt (ERFnPiecewise bistr) =
        (ERFnPiecewise bistrLo, ERFnPiecewise bistrHi)
        where
        (BISTR.Node _ _ _ _ bistrLo bistrHi) =
            BISTR.split faSplit 10 var pt DBox.noinfo bistr 
        pt =
            case maybePt of
                Nothing -> 
                    RA.defaultBisectPt $ DBox.lookup "PieceWise: bisect: " var (BISTR.bistrDom bistr)
                Just pt -> pt
    unBisect var (ERFnPiecewise bistr1, ERFnPiecewise bistr2) =
        ERFnPiecewise $ 
            BISTR.Node (depth - 1) dom var domVarMid bistr1Dp bistr2Dp
        where
        depth = max depth1 depth2
        depth1 = BISTR.bistrDepth bistr1
        depth2 = BISTR.bistrDepth bistr2
        bistr1Dp 
            | depth1 == depth = bistr1
            | otherwise =
                BISTR.setDepth depth bistr1
        bistr2Dp 
            | depth2 == depth = bistr2
            | otherwise =
                BISTR.setDepth depth bistr2
        dom1 = BISTR.bistrDom bistr1
        dom2 = BISTR.bistrDom bistr2
        dom = DBox.unionWith (RA.\/) dom1 dom2
        domVarMid =
            snd $ RA.bounds $
                DBox.lookup "ERFnPiecewise: FA.unbisect: " var dom1

    integrate ix fD@(ERFnPiecewise bistrD) x integdomBox origin (ERFnPiecewise bistrInit) =
        ERFnPiecewise bistrIntegr
        where
        maxDepth = intLogUp 2 (max 1 ix)
        [bistrIntegr] =
            BTINTEG.zipFromOrigin -- invoke a generic BISTR "integrator"
                faSplit faCombine faSplit faCombine 
                ix x origin (Just $ DBox.findWithDefault RA.bottomApprox x integdomBox)
                zipOutsideRange -- outside the integration range, set result to bottom
                shouldSplit
                integrateOriginHere -- how to integrate a piece that crosses the origin hyperplane
                integrateOriginLower -- how to integrate a piece to the left of the origin hyperplane
                integrateOriginHigher -- how to integrate a piece to the right of the origin hyperplane
                [bistrD,  bistrInit]
        zipOutsideRange maybeFromL maybeFromR [bistrD, bistrInit] =
--            unsafePrint
--            (
--                "ERFnPiecewise: integrate: zipOutsideRange: "
--                ++ "\n domB = " ++ show domB
--                ++ "\n bottomFn = " ++ show bottomFn
--            )
            [bistrPadj]
            where
            (ERFnPiecewise bistrPadj) =
                case (maybeFromL, maybeFromR) of
                    (Nothing, Nothing) -> bottomFn
                    (Just bistrLO, Nothing) ->
                        FA.partialIntersect ix 
                            (DBox.singleton x domLO) 
                            (ERFnPiecewise bistrLO) 
                            bottomFn 
                    (Nothing, Just bistrHI) ->
                        FA.partialIntersect ix 
                            (DBox.singleton x domHI) 
                            (ERFnPiecewise bistrHI)
                            bottomFn 
            bottomFn =
                ERFnPiecewise $ BISTR.Leaf depth domB $ FA.bottomApprox domB (FA.getTupleSize fD)
            (domLO, domHI) = 
                RA.bounds $ 
                    DBox.lookup "ERFnPieceWise: integrate: zipOutsideRange: " x domB
            domB = BISTR.bistrDom bistrD
            depth = BISTR.bistrDepth bistrD
        shouldSplit _ depth _ _ _ =
            depth < maxDepth
        integrateOriginHere ix depth dom [faD, faInit] =
--            unsafePrint
--            (
--                "ERFnPiecewise: integrateMeasureImprovement: integrateOriginHere: "
--                ++ "\n dom = " ++ show dom
--                ++ "\n faLO = " ++ show faLO
--                ++ "\n faHI = " ++ show faHI
--            )
            (faLO, [faIntegr], faHI)
            where
            faIntegr = 
                FA.integrate ix faD x integdomBox origin faInit
            faLO =
                FA.partialEval (DBox.singleton x domLO) faIntegr
            faHI =
                FA.partialEval (DBox.singleton x domHI) faIntegr
            (domLO, domHI) = 
                RA.bounds $ 
                    DBox.lookup "ERFnPieceWise: integrate: integrateOriginHere: " x dom
        integrateOriginLower ix depth dom bistrLO [faD, faInit] =
--            unsafePrint
--            (
--                "ERFnPiecewise: integrateMeasureImprovement: integrateOriginLower: "
--                ++ "\n dom = " ++ show dom
--                ++ "\n faLO = " ++ show faLO
--                ++ "\n faHI = " ++ show faHI
--            )
            ([faIntegr], bistrLO { BISTR.bistrVal = faHI })
            where
            faIntegr = 
                FA.integrate ix faD x integdomBox domLO (BISTR.bistrVal bistrLO)
            faHI =
                FA.partialEval (DBox.singleton x domHI) faIntegr
            (domLO, domHI) = 
                RA.bounds $ 
                    DBox.lookup  "ERFnPieceWise: integrate: integrateOriginLower: "  x dom
        integrateOriginHigher ix depth dom [faD, faInit] bistrHI =
            (bistrHI { BISTR.bistrVal = faLO }, [faIntegr])
            where
            faIntegr = 
                FA.integrate ix faD x integdomBox domHI (BISTR.bistrVal bistrHI)
            faLO =
                FA.partialEval (DBox.singleton x domLO) faIntegr
            (domLO, domHI) = 
                RA.bounds $ 
                    DBox.lookup "ERFnPieceWise: integrate: integrateOriginHigher: " x dom

    integrateMeasureImprovement ix (ERFnPiecewise bistrD) x integdomBox origin (ERFnPiecewise bistrP) =
        (ERFnPiecewise bistrIsect, ERFnPiecewise bistrImpr)
        where
        [bistrIsect, bistrImpr] =
            BTINTEG.zipFromOrigin
                faSplit faCombine faSplit faCombine 
                ix x origin (Just $ DBox.findWithDefault RA.bottomApprox x integdomBox)
                zipOutsideRange
                shouldSplit
                integrateOriginHere
                integrateOriginLower
                integrateOriginHigher
                [bistrD,  bistrP]
        zipOutsideRange maybeFromL maybeFromR [bistrD, bistrP] =
--            unsafePrint
--            (
--                "ERFnPiecewise: zipOutsideRange"
--            )
            [bistrPadj, BISTR.mapWithDom (\d v -> FA.const d [1]) bistrP]
            where
            (ERFnPiecewise bistrPadj) =
                case (maybeFromL, maybeFromR) of
                    (Nothing, Nothing) -> (ERFnPiecewise bistrP)
                    (Just bistrLO, Nothing) ->
                        FA.partialIntersect ix 
                            (DBox.singleton x domLO) 
                            (ERFnPiecewise bistrLO) 
                            (ERFnPiecewise bistrP) 
                    (Nothing, Just bistrHI) -> 
                        FA.partialIntersect ix 
                            (DBox.singleton x domHI) 
                            (ERFnPiecewise bistrHI) 
                            (ERFnPiecewise bistrP) 
            (domLO, domHI) = 
                RA.bounds $ 
                    DBox.lookup "ERFnPieceWise: integrateMeasureImprovement: zipOutsideRange: " x domB
            domB = BISTR.bistrDom bistrP
            depth = BISTR.bistrDepth bistrP
        shouldSplit ix depth _ _ _ =
            depth < (effIx2int ix)
        integrateOriginHere ix depth dom [faD, faP] =
--            unsafePrint
--            (
--                "ERFnPiecewise: integrateMeasureImprovement: integrateOriginHere: "
--                ++ "\n dom = " ++ show dom
--                ++ "\n faLO = " ++ show faLO
--                ++ "\n faHI = " ++ show faHI
--            )
            (faLO, [faIsect, faImpr], faHI)
--            (FA.check "ERFnPieceWise: integrateOriginHere: faLO: " faLO,  
--             [FA.check "ERFnPieceWise: integrateOriginHere: faIsect: " faIsect, 
--              FA.check "ERFnPieceWise: integrateOriginHere: faImpr: " faImpr], 
--             FA.check "ERFnPieceWise: integrateOriginHere: faHI: " faHI)
            where
            (faIsect, faImpr) = 
                FA.integrateMeasureImprovement ix faD x integdomBox origin faP
--                FA.integrateMeasureImprovement ix 
--                    (FA.check "ERFnPieceWise: integrateOriginHere: faD: " faD)
--                    x integdomBox origin 
--                    (FA.check "ERFnPieceWise: integrateOriginHere: faP: " faP)
            faLO =
                FA.partialEval (DBox.singleton x domLO) faIsect
            faHI =
                FA.partialEval (DBox.singleton x domHI) faIsect
            (domLO, domHI) = 
                RA.bounds $ 
                    DBox.lookup "ERFnPieceWise: integrateMeasureImprovement: integrateOriginHere: " x dom
        integrateOriginLower ix depth dom bistrLO [faD, faP] =
--            unsafePrint
--            (
--                "ERFnPiecewise: integrateMeasureImprovement: integrateOriginLower: "
--                ++ "\n dom = " ++ show dom
--                ++ "\n faLO = " ++ show faLO
--                ++ "\n faPadj = " ++ show faPadj
--                ++ "\n faHI = " ++ show faHI
--            )
            ([faIsect, faImpr], bistrLO {BISTR.bistrVal = faHI})
            where
            (faIsect, faImpr) = 
                FA.integrateMeasureImprovement ix faD x integdomBox domLO faPadj
            faPadj =
                FA.partialIntersect ix (DBox.singleton x domLO) (BISTR.bistrVal bistrLO) faP
            faHI =
                FA.partialEval (DBox.singleton x domHI) faIsect
            (domLO, domHI) = 
                RA.bounds $ 
                    DBox.lookup  "ERFnPieceWise: integrateMeasureImprovement: integrateOriginLower: "  x dom
        integrateOriginHigher ix depth dom [faD, faP] bistrHI =
--            unsafePrint
--            (
--                "ERFnPiecewise: integrateMeasureImprovement: integrateOriginHigher: "
--                ++ "\n dom = " ++ show dom
--                ++ "\n faLO = " ++ show faLO
--                ++ "\n faHI = " ++ show faHI
    --            )
            (bistrHI {BISTR.bistrVal = faLO}, [faIsect, faImpr])
            where
            (faIsect, faImpr) = 
                FA.integrateMeasureImprovement ix faD x integdomBox domHI faPadj
            faPadj =
                FA.partialIntersect ix (DBox.singleton x domHI) (BISTR.bistrVal bistrHI) faP
            faLO =
                FA.partialEval (DBox.singleton x domLO) faIsect
            (domLO, domHI) = 
                RA.bounds $ 
                    DBox.lookup "ERFnPieceWise: integrateMeasureImprovement: integrateOriginHigher: " x dom
                 
