{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-|
    Module      :  Numeric.ER.RnToRm.Approx.Tuples
    Description :  a list of approximations over the same domain
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Lists of function approximations over the same domain. 
-}
module Numeric.ER.RnToRm.Approx.Tuple 
(
    ERFnTuple(..)
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

import Data.Typeable
import Data.Generics.Basics
import Data.Binary


{-|
    A tuple of function approximations allowing one to get from 
    functions @R^n->R@ to a function @R^n -> R^m@.
-}
data ERFnTuple fa = 
    ERFnTuple { erfnTuple :: [fa] }
    deriving (Typeable, Data)
    
instance (Binary a) => Binary (ERFnTuple a) where
  put (ERFnTuple a) = put a
  get = get >>= \a -> return (ERFnTuple a)

tuplesLift1 ::
    (fa -> fa) ->
    (ERFnTuple fa) -> (ERFnTuple fa)
tuplesLift1 op (ERFnTuple fas) =    
    ERFnTuple (map op fas)
        
tuplesLift2 ::
    (Show fa) =>
    String ->
    (fa -> fa -> fa) ->
    (ERFnTuple fa) -> (ERFnTuple fa) -> (ERFnTuple fa)
tuplesLift2 callerLocation op f1@(ERFnTuple fas1) f2@(ERFnTuple fas2) 
        | length fas1 == length fas2 =
            ERFnTuple $ zipWith op fas1 fas2
        | otherwise =
            error $ 
                callerLocation ++ "incompatible lengths: " 
                ++ show (length fas1) ++ " != " ++ show (length fas2)
                ++ "\n first argument = \n" ++ show fas1
                ++ "\n second argument = \n" ++ show fas2

tuplesSplit ::
    (fa -> (fa, fa)) ->
    (ERFnTuple fa) -> (ERFnTuple fa, ERFnTuple fa)
tuplesSplit op f@(ERFnTuple fas) = 
    (ERFnTuple fas1, ERFnTuple fas2)
    where
    (fas1, fas2) = unzip $ map op fas

-- version with Map.Map:
--data ERFnTuple fa = 
--    ERFnTuple (Map.Map varid fa)
--    deriving (Typeable, Data)
--    
--tuplesLift1 ::
--    (fa -> fa) ->
--    (ERFnTuple fa) -> (ERFnTuple fa)
--tuplesLift1 op (ERFnTuple fas) =    
--    ERFnTuple (Map.map op fas)
--        
--tuplesLift2 ::
--    (fa -> fa -> fa) ->
--    (ERFnTuple fa) -> (ERFnTuple fa) -> (ERFnTuple fa)
--tuplesLift2 op f1@(ERFnTuple fas1) f2@(ERFnTuple fas2) 
--        | Map.keys fas1 == Map.keys fas2 =
--            ERFnTuple $ Map.intersectionWith op fas1 fas2
--        | otherwise =
--            error $ 
--                "ERFnTuple: incompatible keys: " 
--                ++ show (Map.keys fas1) ++ "\n*****\n" ++ show (Map.keys fas2)
--
--tuplesSplit ::
--    (fa -> (fa, fa)) ->
--    (ERFnTuple fa) -> (ERFnTuple fa, ERFnTuple fa)
--tuplesSplit op f@(ERFnTuple fas) = 
--    (ERFnTuple fas1, ERFnTuple fas2)
--    where
--    fas1 = Map.map fst fas12 
--    fas2 = Map.map snd fas12
--    fas12 = Map.map op fas
    
    
instance 
    (FA.ERFnDomApprox box varid domra ranra fa) =>
    Show (ERFnTuple fa)
    where
    show f@(ERFnTuple fas) =
        concat $ map showFA $ zip [0,1..] fas
        where
        showFA (fnname, fa) =
            "\n>>> Function " ++ show fnname ++ ":" ++ show fa

instance 
    (FA.ERFnDomApprox box varid domra ranra fa, H.HTML fa) =>
    H.HTML (ERFnTuple fa)
    where
    toHtml (ERFnTuple []) = H.toHtml "[]"
--    toHtml (ERFnTuple [f]) = H.toHtml f
    toHtml (ERFnTuple fs) =
        H.toHtml $
            abovesTable [H.border 2] $
--                [H.toHtml "Function tuple"] ++ 
                map H.toHtml fs

instance
    (FA.ERFnApprox box varid domra ranra fa) =>
    Eq (ERFnTuple fa)
    where
    (ERFnTuple fas1) == (ERFnTuple fas2) =
        fas1 == fas2

instance
    (FA.ERFnApprox box varid domra ranra fa, Ord fa) =>
    Ord (ERFnTuple fa)
    where
    compare (ERFnTuple fas1) (ERFnTuple fas2) =
        compare fas1 fas2

instance
    (FA.ERFnDomApprox box varid domra ranra fa) =>
    Num (ERFnTuple fa)
    where
    fromInteger n = ERFnTuple [fromInteger n]
    negate = tuplesLift1 negate
    (+) = tuplesLift2 "ERFnTuple: +: " (+)
    (*) = tuplesLift2 "ERFnTuple: *: " (*)

instance 
    (FA.ERFnDomApprox box varid domra ranra fa) =>
    Fractional (ERFnTuple fa)
    where
    fromRational r = ERFnTuple [fromRational r]
    recip = tuplesLift1 recip 


instance 
    (FA.ERFnDomApprox box varid domra ranra fa) =>
    RA.ERApprox (ERFnTuple fa)
    where
    initialiseBaseArithmetic _ =
        RA.initialiseBaseArithmetic (0 :: fa)
    getGranularity (ERFnTuple fas) =
        foldl max 10 $ map RA.getGranularity fas
    setGranularityOuter gran = tuplesLift1 (RA.setGranularityOuter gran) 
    setMinGranularityOuter gran = tuplesLift1 (RA.setMinGranularityOuter gran)
    isBounded (ERFnTuple fas) = 
        and $ map RA.isBounded fas
    f1 /\ f2 = tuplesLift2 "ERFnTuple: /\\: " (RA./\) f1 f2
    refines f1@(ERFnTuple fas1) f2@(ERFnTuple fas2) =
        and $ zipWith RA.refines fas1 fas2
    intersectMeasureImprovement ix f1@(ERFnTuple fas1) f2@(ERFnTuple fas2)
        | length fas1 == length fas2 =
            (ERFnTuple fasIsect, ERFnTuple fasImpr)
        | otherwise =
            error $ show $ f1 RA./\ f2 
        where
        (fasIsect, fasImpr) = unzip $ zipWith (RA.intersectMeasureImprovement ix) fas1 fas2 
    leqReals f1@(ERFnTuple fas1) f2@(ERFnTuple fas2)
        | length fas1 == length fas2 =
            leqTuple $ zipWith RA.leqReals fas1 fas2
        | otherwise =
            error $ show $ f1 RA./\ f2
        where
        leqTuple [] = Just True
        leqTuple (tv : tvs) =
            case and $ map (== tv) tvs of
                True -> tv
                _ -> Nothing  
    compareApprox f1@(ERFnTuple fas1) f2@(ERFnTuple fas2) =
        compareListsWith RA.compareApprox fas1 fas2

instance 
    (FA.ERFnDomApprox box varid domra ranra fa, RA.ERIntApprox fa) =>
    RA.ERIntApprox (ERFnTuple fa)
    where
--    doubleBounds = :: ira -> (Double, Double) 
--    floatBounds :: ira -> (Float, Float)
--    integerBounds :: ira -> (ExtendedInteger, ExtendedInteger)
    bisectDomain maybePt f@(ERFnTuple fas) =
        case maybePt of
            Nothing ->
                tuplesSplit (RA.bisectDomain Nothing) f
            Just (ERFnTuple fasPt) -> 
                (ERFnTuple fas1, ERFnTuple fas2)
                where
                (fas1, fas2) = 
                    unzip $ 
                        map (\(fa, pt) -> RA.bisectDomain (Just pt) fa) $ 
                            zip fas fasPt
    bounds = tuplesSplit RA.bounds
    f1 \/ f2 = tuplesLift2 "ERFnTuple: \\/: " (RA.\/) f1 f2

instance 
    (FA.ERFnDomApprox box varid domra ranra fa, RAEL.ERApproxElementary fa) =>
    RAEL.ERApproxElementary (ERFnTuple fa)
    where
    abs ix = tuplesLift1 $ RAEL.abs ix
    sqrt ix = tuplesLift1 $ RAEL.sqrt ix
    exp ix = tuplesLift1 $ RAEL.exp ix
    log ix = tuplesLift1 $ RAEL.log ix
    sin ix = tuplesLift1 $ RAEL.sin ix
    cos ix = tuplesLift1 $ RAEL.cos ix
    atan ix = tuplesLift1 $ RAEL.atan ix
        
instance 
    (FA.ERFnDomApprox box varid domra ranra fa) =>
    FA.ERFnApprox box varid domra ranra (ERFnTuple fa)
    where
    check prgLocation (ERFnTuple fs) =
        ERFnTuple $ map checkComp $ zip [0..] fs
        where
        checkComp (n, f) =
            FA.check (prgLocation ++ "fn" ++ show n ++ ": ") f
    domra2ranra (ERFnTuple (fa:_)) d =
        FA.domra2ranra fa d
    ranra2domra (ERFnTuple (fa:_)) r =
        FA.ranra2domra fa r
    setMaxDegree maxDegree = tuplesLift1 (FA.setMaxDegree maxDegree)
    setMaxSize maxSize = tuplesLift1 (FA.setMaxSize maxSize)
    getRangeApprox (ERFnTuple fas) = 
        concat $ map FA.getRangeApprox fas
    getTupleSize (ERFnTuple fas) = length fas
    tuple fs 
        | sameDomains doms = 
            ERFnTuple $ concat $ map erfnTuple fs
        | otherwise = 
            error $ 
                "ERFnTuple: FA.tuple: incompatible domains:\n " 
                ++ (unlines $ map show fs)
        where
        sameDomains [_] = True
        sameDomains (a : rest@(b : _)) =
            sameab && (sameDomains rest)
            where
            sameab =
                and $ map snd $ DBox.zipWithDefault RA.bottomApprox RA.equalApprox a b
        doms = map FA.dom fs
    applyTupleFn tupleFn (ERFnTuple fs) =
        FA.tuple $ tupleFn $ map (\fa -> ERFnTuple [fa]) fs
    volume (ERFnTuple fas) = sum $ map (FA.volume) fas
    scale ratio = tuplesLift1 (FA.scale ratio)
    partialIntersect ix substitutions =
        tuplesLift2 "ERFnTuple: partialIntersect: " $ FA.partialIntersect ix substitutions
    eval ptBox (ERFnTuple fas) =
        concat $ map (FA.eval ptBox) fas
    evalInner ptBox (ERFnTuple fas) =
        concat $ map (FA.evalInner ptBox) fas
    partialEval substitutions = tuplesLift1 $ FA.partialEval substitutions
            
instance 
    (FA.ERFnDomApprox box varid domra ranra fa) =>
    FA.ERFnDomApprox box varid domra ranra (ERFnTuple fa)
    where
    dom (ERFnTuple (fa:_)) = FA.dom fa
    bottomApprox domB tupleSize =
        ERFnTuple $ replicate tupleSize $ FA.bottomApprox domB 1
    const domB vals =
        ERFnTuple $ map (\v -> FA.const domB [v]) vals
    proj domB i =
        ERFnTuple [FA.proj domB i] 

    bisect var maybePt =
        tuplesSplit $ FA.bisect var maybePt
    integrate ix (ERFnTuple fasD) x integdomBox origin (ERFnTuple fasInit) =
        ERFnTuple $ map integ $ zip fasD fasInit
        where
        integ (faD, faInit) =
            FA.integrate ix faD x integdomBox origin faInit
    integrateMeasureImprovement ix (ERFnTuple fasD) x integdomBox origin (ERFnTuple fasP) =
        (ERFnTuple fasIsect, ERFnTuple fasImpr) 
        where
        (fasIsect, fasImpr) =
            unzip $ map integ $ zip fasD fasP
        integ (faD, faP) =
            FA.integrateMeasureImprovement ix faD x integdomBox origin faP
