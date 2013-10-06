{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Basic
    Description :  (internal) polynomial datatype and simple functions
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Internal module for "Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom".
    
    Definition of the polynomial datatype and simple related functions.
-}
module Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Basic 
where

import qualified Numeric.ER.Real.Base as B
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainIntBox)
import Numeric.ER.Misc

import qualified Data.Map as Map

import Data.Typeable
import Data.Generics.Basics
import Data.Binary

errorModule msg = error $ "Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom: " ++ msg

{-|
    A polynomial represented by its coefficients it the Chebyshev basis.
    
    The polynomials are never to be used outside the domain @[-1,1]^n@.
    
    All operations are rounded in such a way that the resulting polynomial
    is a /point-wise upper or lower bound/ of the exact result. 
-}
data ERChebPoly box b =
    ERChebPoly
--         Map (MultiSet Int) b
    {
        chplCoeffs :: (Map.Map (TermKey box) b)
    }
    deriving (Eq, Ord, Typeable, Data)

type TermKey box = box
    
instance (Ord a, Binary a, Binary b) => Binary (ERChebPoly a b) where
  put (ERChebPoly a) = put a
  get = get >>= \a -> return (ERChebPoly a)
    
chplHasNoNaN p@(ERChebPoly coeffs) =
    Map.fold (&&) True $ Map.map coeffOK coeffs
    where
    coeffOK c =
        not $ B.isERNaN c

chplHasNoNaNOrInfty p@(ERChebPoly coeffs) =
    Map.fold (&&) True $ Map.map coeffOK coeffs
    where
    coeffOK = isBounded
    isBounded c 
        | B.isERNaN c = False
        | B.isPlusInfinity c = False
        | B.isPlusInfinity (- c) = False
        | otherwise = True

chplCheck prgLocation p
    | chplHasNoNaNOrInfty p = p
    | otherwise =
        unsafePrint (prgLocation ++ " problem with p = \n" ++ show p) p

chplCompareApprox (ERChebPoly coeffs1) (ERChebPoly coeffs2) =
    compare coeffs1 coeffs2 
    
chplConstTermKey :: (DomainBox box varid d) => box
chplConstTermKey = DBox.noinfo

chplIsConstTermKey :: (DomainBox box varid d) => box -> Bool
chplIsConstTermKey = DBox.isNoinfo

chplTermOrder :: (DomainBox box varid d, Num d) => box -> d
chplTermOrder termKey = DBox.fold (+) 0 termKey

chplTermArity :: (DomainBox box varid d) => box -> Int
chplTermArity termKey = length $ DBox.keys termKey

{-|
    Inspect all terms of the polynomial and return the 
    degree of the highest degree term.
-}
chplGetDegree ::
    (B.ERRealBase b, DomainBox box varid d, Num d, Ord d) =>
    (ERChebPoly box b) ->
    d
chplGetDegree (ERChebPoly coeffs) =
    foldl max 0 $ map chplTermOrder $ Map.keys coeffs

{-|
    If the polynomial is constant, return the constant,
    otherwise return Nothing.
-}
chplGetConst ::
    (B.ERRealBase b, DomainBox box varid d, Num d, Ord d) =>
    (ERChebPoly box b) ->
    Maybe b
chplGetConst (ERChebPoly coeffs) =
    case Map.toList coeffs of
        [] -> Just 0
        [(key,val)] | chplIsConstTermKey key -> Just val
        _ -> Nothing
    
-- chplGetArity = length . chplGetVars  
    
chplGetVars (ERChebPoly coeffs) =
    DBox.keys $ foldl DBox.union DBox.noinfo $ Map.keys coeffs

chplGetGranularity (ERChebPoly coeffs) =
    foldl max 0 $ map B.getGranularity $ Map.elems coeffs
    
chplSetMinGranularity gran (ERChebPoly coeffs) =
    ERChebPoly $ Map.map (B.setMinGranularity gran) coeffs
    
chplSetGranularity gran (ERChebPoly coeffs) =
    ERChebPoly $ Map.map (B.setGranularity gran) coeffs
    
chplConst ::    
    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
    b -> 
    ERChebPoly box b
chplConst val =    
    (ERChebPoly $ Map.singleton chplConstTermKey val)
    
{-|
    Make a basic "x" polynomial for a given variable number. 
-}
chplVar :: 
    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
    varid -> 
    ERChebPoly box b
chplVar varName =
    ERChebPoly $ Map.singleton (DBox.singleton varName 1) 1

{-|
    Construct an affine polynomial.
-}
chplAffine ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    b -> 
    Map.Map varid b ->
    ERChebPoly box b
chplAffine at0 varCoeffs =
    ERChebPoly $ 
        Map.insert chplConstTermKey at0 $
            Map.mapKeys (\ i -> DBox.singleton i 1) varCoeffs


--chplRemoveZeroTermsDown, chplRemoveZeroTermsUp ::
--    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
--    ERChebPoly box b -> ERChebPoly box b
--chplRemoveZeroTermsDown = chplNeg . fst . chplRemoveZeroTerms
--chplRemoveZeroTermsUp = snd . chplRemoveZeroTerms

--chplRemoveZeroTerms ::
--    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
--    ERChebPoly box b -> (ERChebPoly box b, ERChebPoly box b)
--chplRemoveZeroTerms (ERChebPoly coeffs) =
--    (chplNeg $ ERChebPoly $ coeffsNo0T0Down,
--     ERChebPoly $ coeffsNo0T0Up)
--    where
--    coeffsNo0T0Down =
--        Map.insertWith plusDown chplConstTermKey (- err) coeffsNo0T0
--    coeffsNo0T0Up =
--        Map.insertWith plusUp chplConstTermKey err coeffsNo0T0
--    (coeffsNo0T0, err) = 
--        foldl addTermNo0T0 (Map.empty, 0) $ Map.toList coeffs
--    addTermNo0T0 (prevCoeffs, prevErr) (term, coeff) 
--        | coeff == 0 =
--            (prevCoeffs, prevErr)
--        | otherwise =
--            (newCoeffs, newErr)
--        where
--        newTerm =
--            DBox.filter (> 0) term
--        newCoeffs = 
--            Map.insert newTerm newCoeffUp prevCoeffs
--        newCoeffUp = prevCoeff + coeff
--        newCoeffDown = prevCoeff `plusDown` coeff
--        prevCoeff =
--            Map.findWithDefault 0 newTerm prevCoeffs
--        newErr = prevErr +  newCoeffUp - newCoeffDown

chplRemoveZeroTermsUp ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    ERChebPoly box b -> ERChebPoly box b
chplRemoveZeroTermsUp (ERChebPoly coeffs) =
    ERChebPoly coeffsNo0T0Up
    where
    coeffsNo0T0Up =
        Map.insertWith plusUp chplConstTermKey err coeffsNo0T0
    (coeffsNo0T0, err) = 
        foldl addTermNo0T0 (Map.empty, 0) $ Map.toList coeffs
    addTermNo0T0 (prevCoeffs, prevErr) (term, coeff)
        | coeff == 0 =
            (prevCoeffs, prevErr)
        | otherwise =
            (newCoeffs, newErr)
        where
        newTerm =
            DBox.filter (> 0) term
        newCoeffs = 
            Map.insert newTerm newCoeffUp prevCoeffs
        newCoeffUp = prevCoeff + coeff
        newCoeffDown = prevCoeff `plusDown` coeff
        prevCoeff =
            Map.findWithDefault 0 newTerm prevCoeffs
        newErr = prevErr +  newCoeffUp - newCoeffDown

--chplRemoveLowCoeffsDown, chplRemoveLowCoeffsUp ::
--    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
--    b -> ERChebPoly box b -> ERChebPoly box b
--chplRemoveLowCoeffsDown maxCoeff = chplNeg . fst . chplRemoveLowCoeffs maxCoeff
--chplRemoveLowCoeffsUp maxCoeff = snd . chplRemoveLowCoeffs maxCoeff

--chplRemoveLowCoeffs ::
--    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
--    b -> ERChebPoly box b -> (ERChebPoly box b, ERChebPoly box b)
--chplRemoveLowCoeffs maxCoeff (ERChebPoly coeffs) =
--    (chplNeg $ ERChebPoly $ coeffsNoLowDown,
--     ERChebPoly $ coeffsNoLowUp)
--    where
--    coeffsNoLowDown =
--        Map.insertWith plusDown chplConstTermKey (- err) coeffsNoLow
--    coeffsNoLowUp =
--        Map.insertWith plusUp chplConstTermKey err coeffsNoLow
--    err = sum $ map abs $ Map.elems coeffsLow
--    (coeffsLow, coeffsNoLow) = 
--        Map.partition (\ c -> abs c < maxCoeff) coeffs

chplCountTerms ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    ERChebPoly box b -> Int
chplCountTerms (ERChebPoly coeffs) =
    Map.size coeffs


{------------------ Formatting ------------------------}

instance (B.ERRealBase b, DomainBox box varid Int, Ord box) => Show (ERChebPoly box b)
    where
--    show = chplShow 8 False True
    show = chplShow 8 False False

{-|
    Convert a polynomial to a string representation,
    using the ordinary x^n basis.
-}
chplShow :: 
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    Int {- ^ number of decimal digits to show -} ->
    Bool {-^ whether to show granularity -} ->
    Bool {-^ show the polynomial also in its native Chebyshev basis -} ->
    ERChebPoly box b ->
    String
chplShow digitsToShow showGranularity showChebyshevBasis (ERChebPoly coeffs) 
    | showChebyshevBasis = "\n" ++ inChebBasis ++ " = \n" ++ inXBasis
    | otherwise = inXBasis
    where
    inChebBasis = 
        showCoeffs showTermT $ Map.filter (\c -> c /= 0) $ coeffs
    inXBasis = 
        showCoeffs showTermX $ chebToXBasis coeffs
    showCoeffs showTerm coeffs =
        concatWith " + " $ map showTerm $ Map.toAscList coeffs
    showTermT (term, coeff)
        | chplIsConstTermKey term = show coeff
        | otherwise =  
            show coeff ++ "*" ++ (concat $ map showT $ DBox.toList term) 
    showTermX (term, coeff)
        | chplIsConstTermKey term = showC coeff
        | otherwise = 
            showC coeff ++ (concatMap ((:) '*' . showX) $ DBox.toList term) 
--            showC coeff ++ "*" ++ (concat $ map showX $ DBox.toList term)
    showT (var, deg) = "T" ++ show deg ++ "(" ++ showVar var ++ ")"
--    showX (var, deg) = showVar var ++ "**" ++ show deg
    showX (var, deg) = showVar var ++ "^" ++ show deg
    showC = B.showDiGrCmp digitsToShow showGranularity False

{-|
    conversion of polynomials from Chebyshev basis to the X^n basis
    
    (not exact - suffering from rounding in the coefficient conversions)
-}
chebToXBasis ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    (Map.Map (TermKey box) b) {-^ polynomial in Chebyshev basis -} ->
    (Map.Map (TermKey box) b) {-^ approxition of the equivalent polynomial in X^n basis -}
chebToXBasis coeffs =
    Map.filter (\c -> c /= 0) $
        Map.foldWithKey addTerm Map.empty coeffs
    where
    addTerm term coeff prevXCoeffs =
        Map.unionWith (+) prevXCoeffs $
            Map.map (\ c -> coeff * (fromInteger c)) $ 
                termXterms term

{-|
    conversion of one Chebyshev term to the X^n basis
-}
termXterms ::
    (DomainBox box varid Int, Ord box) =>
    TermKey box 
        {-^ a Chebyshev term represented by the Chebyshev degrees 
            for each variable in the term -} ->
    Map.Map (TermKey box) Integer
        {-^ the polynomial equivalent to the given Chebyshev term 
            (using integer coefficients) -}
termXterms term =
    foldl addCombination Map.empty $ 
        allCombinations $ 
            map (mapSnd $ \ deg -> chebyXCoeffsLists !! deg) $ 
                DBox.toList term
    where
    addCombination prevMap (varPowerCoeffTriples) =
        Map.insertWith (+) term coeffProduct prevMap
        where
        term = 
            DBox.fromList $
                filter (\(v,p) -> p > 0) $
                    map (\(v,(p,_)) -> (v,p)) varPowerCoeffTriples 
        coeffProduct =
            fromInteger $
                product $ 
                    map (\(_,(_,c)) -> c) varPowerCoeffTriples
    
{-| Chebyshev polynomials expressed as associative lists power -> coeff -}
chebyXCoeffsLists ::
    (Num d1, Enum d1, Num d2, Enum d2, Eq d2) =>
    [[(d1, d2)]]
chebyXCoeffsLists =
    map convertCoeffs chebyXCoeffs
    where
    convertCoeffs coeffs =
        filter ((/= 0) . snd) $ zip [0,1..] coeffs

{-| Chebyshev polynomials expressed as lists of integer coefficients for powers 0,1,2... -}
chebyXCoeffs ::
    (Num d, Enum d) =>
    [[d]]
chebyXCoeffs =
    aux 
        [1] -- T_0(x) = 1
        [0,1] -- T_1(x) = x
    where
    aux tnM2 tnM1 =
        tnM2 : (aux tnM1 (newTerm tnM2 tnM1))
    newTerm tnM2 tnM1 =
        zipWith (-) (0 : (map (*2) tnM1)) (tnM2 ++ [0,0..])
        -- T_n(x) = 2 * x * T_{n-1}(x) - T_{n-2}(x)
        
