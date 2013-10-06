{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Integration
    Description :  (internal) integration of polynomials  
    Copyright   :  (c) 2007-2009 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Internal module for "Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom".
    
    Implementation of safely rounded integration of polynomials
    and other related functions.
-}
module Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Integration 
where

import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Basic
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Eval
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Ring
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Bounds

import qualified Numeric.ER.Real.Base as B
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainBoxMappable, DomainIntBox)
import Numeric.ER.Real.Approx.Interval
import Numeric.ER.Misc

import qualified Data.Map as Map

{-|
     Approximate from below and from above the integral of a polynomial.
     
     Based on the following formulas for Chebyshev polynomials:
     
>     \int T_n(x)dx = 
>        T_{n+1}(x)/2(n+1) - T_{n-1}(x)/2(n-1)

>     \int T_1(x)dx = 
>        T_2(x)/4 + 1/4

>     \int T_0(x)dx = 
>        T_1(x)
 
-}
chplIntegrate ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
    varid {-^ variable to integrate by -} -> 
    ERChebPoly box b ->
    (ERChebPoly box b, ERChebPoly box b)
chplIntegrate x p@(ERChebPoly coeffs) =
--    unsafePrintReturn
--    (
--        "ERChebPoly: integrate:"
--        ++ "\n p = " ++ show p
--        ++ "\n result = " 
--    )
    (pNp1Down -. pNm1Up, 
     pNp1Up -^ pNm1Down)
--    (chplRemoveZeroTermsDown $ pNp1Down - pNm1Up, 
--     chplRemoveZeroTermsUp $ pNp1Up - pNm1Down)
    where
    pNp1Up =
        ERChebPoly $ 
            Map.insertWith plusUp chplConstTermKey errBoundNp1 $ 
                Map.fromList coeffsNp1
    pNp1Down =
        ERChebPoly $ 
            Map.insertWith plusDown chplConstTermKey (- errBoundNp1) $ 
                Map.fromList coeffsNp1
    pNm1Up =
        ERChebPoly $ 
            Map.insertWith plusUp chplConstTermKey errBoundNm1 $ 
                Map.fromList coeffsNm1
    pNm1Down =
        ERChebPoly $ 
            Map.insertWith plusDown chplConstTermKey (- errBoundNm1) $ 
                Map.fromList coeffsNm1
    (coeffsNp1, errBoundNp1) =
        foldl cfNp1 ([],0) coeffsList
    (coeffsNm1, errBoundNm1) =
        foldl cfNm1 ([],0) coeffsList
    coeffsList = Map.toList coeffs
    cfNp1 (prevTerms, prevErr) (termKey, coeff)
        | n == 0 =
            ((termKeyNp1, coeff):prevTerms, prevErr)
        | n == 1 =
            ((termKeyN0, coeff0Up):(termKeyNp1, coeffNp1Up):prevTerms, prevErr + coeff0Err + coeffNp1Err)
        | otherwise =
            ((termKeyNp1, coeffNp1Up):prevTerms, prevErr + coeffNp1Err)
        where
        termKeyNp1 = DBox.insert x (n + 1) termKey
        termKeyNm1 = DBox.insert x (n - 1) termKey 
        termKeyN0 = DBox.delete x termKey 
        n = DBox.findWithDefault 0 x termKey
        coeffNp1Err = coeffNp1Up - coeffNp1Down 
        coeffNp1Up = coeff / (2*nB + 2)
        coeffNp1Down = -((-coeff) / (2*nB + 2))
        nB = fromInteger $ toInteger n
        coeff0Up = coeff / 4
        coeff0Down = - ((- coeff) / 4)
        coeff0Err = coeff0Up - coeff0Down 
    cfNm1 (prevTerms, prevErr) (termKey, coeff)
        | n == 0 || n == 1 =
            (prevTerms, prevErr)
        | otherwise =
            ((termKeyNm1, coeffNm1Up):prevTerms, prevErr + coeffNm1Err)
        where
        termKeyNm1 = DBox.insert x (n - 1) termKey 
        n = DBox.findWithDefault 0 x termKey
        coeffNm1Up = coeff / (2*nB - 2)
        coeffNm1Down = -((-coeff) / (2*nB - 2))
        nB = fromInteger $ toInteger n
        coeffNm1Err = coeffNm1Up - coeffNm1Down 

--{-|
--    measure the volume between a polynomial and the zero axis on [-1,1]^n
---}
--chplVolumeAboveZero ::
--    (B.ERRealBase b, DomainBox box varid Int, Ord box, 
--     DomainBoxMappable boxb boxbb varid b [ERInterval b]) =>
--    [varid] ->
--    ERChebPoly box b ->
--    (b,b)
--chplVolumeAboveZero vars p@(ERChebPoly coeffs) =
----    unsafePrint ("chplVolumeAboveZero: returning:" ++ show result) $
----    unsafePrint ("chplVolumeAboveZero: vars = " ++ show vars) $
--    result
--    where
--    result = 
--        (- (integUpAtOddCorners - integDownAtEvenCorners), integUpAtEvenCorners - integDownAtOddCorners)
--    integUpAtEvenCorners = sumUp $ map (chplEvalUp integUp) evenCorners
--    integUpAtOddCorners = sumUp $ map (chplEvalUp integUp) oddCorners 
--    integDownAtEvenCorners = sumDown $ map (chplEvalDown integDown) evenCorners  
--    integDownAtOddCorners = sumDown $ map (chplEvalDown integDown) oddCorners
--    evenCorners = map (DBox.fromList) evenCornersL
--    oddCorners = map (DBox.fromList) oddCornersL
--    (evenCornersL, oddCornersL) =
--        allPairsCombinationsEvenOdd $ zip vars $ repeat (1,-1)
--    integUp = integrateByAllVars snd p vars
--    integDown = integrateByAllVars fst p vars
--    integrateByAllVars pick p [] = p
--    integrateByAllVars pick p (x : xs) =
--        integrateByAllVars pick ip xs
--        where
--        ip = pick $ chplIntegrate x p
----    vars = chplGetVars p
      

