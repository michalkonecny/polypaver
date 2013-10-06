{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-|
    Module      :  Numeric.ER.RnToRm.Approx.Derivatives
    Description :  a tree of all partial derivatives
    Copyright   :  (c) 2009 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Approximate not only the function but also all of its
    partial derivatives.  
    The partial derivatives are worked out using automatic differentiation. 
-}
module Numeric.ER.RnToRm.Approx.Derivatives
(
    ERFnDerivatives(..)
)
where

import qualified Numeric.ER.RnToRm.Approx as FA 
import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Approx.Elementary as RAEL
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes
import Numeric.ER.Misc

import Numeric.ER.ShowHTML
import qualified Text.Html as H

import qualified Data.Map as Map
import Data.Typeable
import Data.Generics.Basics
import Data.Binary

import Data.Convertible.Base

{-|
    A tuple of function approximations allowing one to get from 
    functions @R^n->R@ to a function @R^n -> R^m@.
-}
data ERFnDerivatives varid fa = 
    ERFnDerivatives 
    { 
        erfnNoDerivative :: fa, 
        erfnPartialDerivatives :: Map.Map varid (ERFnDerivatives varid fa) 
    }
    deriving (Typeable, Data)
    
instance (Binary a, Binary v, Ord v) => Binary (ERFnDerivatives v a) where
  put (ERFnDerivatives a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (ERFnDerivatives a b)

instance 
    (FA.ERFnDomApprox box varid domra ranra fa) =>
    Show (ERFnDerivatives varid fa)
    where
    show f@(ERFnDerivatives fa derivsMap) =
        "ERFnDerivatives" ++ show fa

derivsLift1 op (ERFnDerivatives fa derivs) = 
    ERFnDerivatives (op fa) (Map.map (derivsLift1 op) derivs)


--instance 
--    (FA.ERFnDomApprox box varid domra ranra fa, H.HTML fa) =>
--    H.HTML (ERFnTuple fa)
--    where
--    toHtml (ERFnTuple []) = H.toHtml "[]"
----    toHtml (ERFnTuple [f]) = H.toHtml f
--    toHtml (ERFnTuple fs) =
--        H.toHtml $
--            abovesTable [H.border 2] $
----                [H.toHtml "Function tuple"] ++ 
--                map H.toHtml fs
--
instance
    (FA.ERFnApprox box varid domra ranra fa) =>
    Eq (ERFnDerivatives varid fa)
    where
    (ERFnDerivatives fa1 _) == (ERFnDerivatives fa2 _) =
        fa1 == fa2

instance
    (FA.ERFnApprox box varid domra ranra fa, Ord fa) =>
    Ord (ERFnDerivatives varid fa)
    where
    compare (ERFnDerivatives fa1 _) (ERFnDerivatives fa2 _) =
        compare fa1 fa2

instance
    (FA.ERFnDomApprox box varid domra ranra fa) =>
    Num (ERFnDerivatives varid fa)
    where
    fromInteger n = ERFnDerivatives (fromInteger n) Map.empty
    negate = derivsLift1 negate
    (ERFnDerivatives fa1 derivs1) + (ERFnDerivatives fa2 derivs2) =
        ERFnDerivatives 
            (fa1 + fa2)
            (Map.unionWith (+) derivs1 derivs2)
--        where
--        (derivs1Unif, derivs2Unif) = unifyDerivs derivs1 derivs2
    f1@(ERFnDerivatives fa1 derivs1) * f2@(ERFnDerivatives fa2 derivs2) =
        ERFnDerivatives 
            (fa1 * fa2)
            (Map.mapWithKey getDerivByVar $ Map.union derivs1 derivs2)
            where
            getDerivByVar x _ = 
                fa1DxTimesFa2 + fa1Timesfa2Dx
                where
                fa1DxTimesFa2 = 
                    case Map.lookup x derivs1 of
                        Nothing -> 0
                        Just fa1Dx -> fa1Dx * f2
                fa1Timesfa2Dx =
                    case Map.lookup x derivs2 of
                        Nothing -> 0
                        Just fa2Dx -> f1 * fa2Dx

instance 
    (FA.ERFnDomApprox box varid domra ranra fa) =>
    Fractional (ERFnDerivatives varid fa)
--    where
--    fromRational r = ERFnTuple [fromRational r]
--    recip = tuplesLift1 recip 
--
--
instance 
    (FA.ERFnDomApprox box varid domra ranra fa) =>
    RA.ERApprox (ERFnDerivatives varid fa)
--    where
--    initialiseBaseArithmetic _ =
--    	RA.initialiseBaseArithmetic (0 :: fa)
--    getGranularity (ERFnTuple fas) =
--        foldl max 10 $ map RA.getGranularity fas
--    setGranularityOuter gran = tuplesLift1 (RA.setGranularityOuter gran) 
--    setMinGranularityOuter gran = tuplesLift1 (RA.setMinGranularityOuter gran)
--    f1 /\ f2 = tuplesLift2 "ERFnTuple: /\\: " (RA./\) f1 f2
--    refines f1@(ERFnTuple fas1) f2@(ERFnTuple fas2) =
--        and $ zipWith RA.refines fas1 fas2
--    intersectMeasureImprovement ix f1@(ERFnTuple fas1) f2@(ERFnTuple fas2)
--        | length fas1 == length fas2 =
--            (ERFnTuple fasIsect, ERFnTuple fasImpr)
--        | otherwise =
--            error $ show $ f1 RA./\ f2 
--        where
--        (fasIsect, fasImpr) = unzip $ zipWith (RA.intersectMeasureImprovement ix) fas1 fas2 
--    leqReals f1@(ERFnTuple fas1) f2@(ERFnTuple fas2)
--        | length fas1 == length fas2 =
--            leqTuple $ zipWith RA.leqReals fas1 fas2
--        | otherwise =
--            error $ show $ f1 RA./\ f2
--        where
--        leqTuple [] = Just True
--        leqTuple _ = 
--            error $ "ERFnTuple: leqReals not implemented"
--    compareApprox f1@(ERFnTuple fas1) f2@(ERFnTuple fas2) =
--        compareListsWith RA.compareApprox fas1 fas2
--
instance 
    (FA.ERFnDomApprox box varid domra ranra fa, RA.ERIntApprox fa) =>
    RA.ERIntApprox (ERFnDerivatives varid fa)
--    where
----    doubleBounds = :: ira -> (Double, Double) 
----    floatBounds :: ira -> (Float, Float)
----    integerBounds :: ira -> (ExtendedInteger, ExtendedInteger)
--    bisectDomain maybePt f@(ERFnTuple fas) =
--        case maybePt of
--            Nothing ->
--                tuplesSplit (RA.bisectDomain Nothing) f
--            Just (ERFnTuple fasPt) -> 
--                (ERFnTuple fas1, ERFnTuple fas2)
--                where
--                (fas1, fas2) = 
--                    unzip $ 
--                        map (\(fa, pt) -> RA.bisectDomain (Just pt) fa) $ 
--                            zip fas fasPt
--    bounds = tuplesSplit RA.bounds
--    f1 \/ f2 = tuplesLift2 "ERFnTuple: \\/: " (RA.\/) f1 f2
--
instance 
    (FA.ERFnDomApprox box varid domra ranra fa, RAEL.ERApproxElementary fa) =>
    RAEL.ERApproxElementary (ERFnDerivatives varid fa)
--    where
--    abs ix = tuplesLift1 $ RAEL.abs ix
--    exp ix = tuplesLift1 $ RAEL.exp ix
--    log ix = tuplesLift1 $ RAEL.log ix
--    sin ix = tuplesLift1 $ RAEL.sin ix
--    cos ix = tuplesLift1 $ RAEL.cos ix
--    atan ix = tuplesLift1 $ RAEL.atan ix
--        
instance 
    (FA.ERFnDomApprox box varid domra ranra fa) =>
    FA.ERFnApprox box varid domra ranra (ERFnDerivatives varid fa)
    where
    getPartialDerivative (ERFnDerivatives fa derivs) x =
        case Map.lookup x derivs of
            Nothing -> 0
            Just deriv -> deriv
--    check prgLocation (ERFnTuple fs) =
--        ERFnTuple $ map checkComp $ zip [0..] fs
--        where
--        checkComp (n, f) =
--            FA.check (prgLocation ++ "fn" ++ show n ++ ": ") f
--    domra2ranra (ERFnTuple (fa:_)) d =
--        FA.domra2ranra fa d
--    ranra2domra (ERFnTuple (fa:_)) r =
--        FA.ranra2domra fa r
    setMaxDegree maxDegree = derivsLift1 (FA.setMaxDegree maxDegree)
--    setMaxSize maxSize = tuplesLift1 (FA.setMaxSize maxSize)
--    getTupleSize (ERFnTuple fas) = length fas
--    tuple fs 
--        | sameDomains doms = 
--            ERFnTuple $ concat $ map erfnTuple fs
--        | otherwise = 
--            error $ 
--                "ERFnTuple: FA.tuple: incompatible domains:\n " 
--                ++ (unlines $ map show fs)
--        where
--        sameDomains [_] = True
--        sameDomains (a : rest@(b : _)) =
--            sameab && (sameDomains rest)
--            where
--            sameab =
--                and $ map snd $ DBox.zipWithDefault RA.bottomApprox RA.equalApprox a b
--        doms = map FA.dom fs
--    applyTupleFn tupleFn (ERFnTuple fs) =
--        FA.tuple $ tupleFn $ map (\fa -> ERFnTuple [fa]) fs
--    volume (ERFnTuple fas) = sum $ map (FA.volume) fas
--    scale ratio = tuplesLift1 (FA.scale ratio)
--    partialIntersect ix substitutions =
--        tuplesLift2 "ERFnTuple: partialIntersect: " $ FA.partialIntersect ix substitutions
--    eval ptBox (ERFnTuple fas) =
--        concat $ map (FA.eval ptBox) fas
--    evalInner ptBox (ERFnTuple fas) =
--        concat $ map (FA.evalInner ptBox) fas
--    partialEval substitutions = tuplesLift1 $ FA.partialEval substitutions
--            
instance 
    (FA.ERFnDomApprox box varid domra ranra fa) =>
    FA.ERFnDomApprox box varid domra ranra (ERFnDerivatives varid fa)
    where
--    dom (ERFnTuple (fa:_)) = FA.dom fa
--    bottomApprox domB tupleSize =
--        ERFnTuple $ replicate tupleSize $ FA.bottomApprox domB 1
--    const domB vals =
--        ERFnTuple $ map (\v -> FA.const domB [v]) vals
    proj domB i =
        ERFnDerivatives (FA.proj domB i) (Map.singleton i one)
        where
        one = 
            ERFnDerivatives 1 (Map.singleton i zero)
        zero = 
            ERFnDerivatives 0 (Map.singleton i zero)
--
--    bisect var maybePt =
--        tuplesSplit $ FA.bisect var maybePt
--    integrate ix (ERFnTuple fasD) x integdomBox origin (ERFnTuple fasInit) =
--        ERFnTuple $ map integ $ zip fasD fasInit
--        where
--        integ (faD, faInit) =
--            FA.integrate ix faD x integdomBox origin faInit
--    integrateMeasureImprovement ix (ERFnTuple fasD) x integdomBox origin (ERFnTuple fasP) =
--        (ERFnTuple fasIsect, ERFnTuple fasImpr) 
--        where
--        (fasIsect, fasImpr) =
--            unzip $ map integ $ zip fasD fasP
--        integ (faD, faP) =
--            FA.integrateMeasureImprovement ix faD x integdomBox origin faP


instance 
    (FA.ERFnDomApprox box varid domra ranra fa) =>
    Convertible (ERFnDerivatives varid fa) fa
    where
    safeConvert (ERFnDerivatives fa derivs) = Right fa
     