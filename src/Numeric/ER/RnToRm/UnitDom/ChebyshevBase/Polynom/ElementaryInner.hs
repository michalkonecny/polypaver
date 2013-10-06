{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Elementary
    Description :  (internal) elementary functions applied to polynomials  
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Internal module for "Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom".
    
    Implementation of inner-rounded 
    elementary functions applied to polynomials.
-}
module Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.ElementaryInner 
where

import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Basic
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Reduce
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Eval
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Ring
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Bounds
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Enclosure
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Division
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Elementary 

import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Approx.Elementary as RAEL
import qualified Numeric.ER.Real.Base as B
import Numeric.ER.Real.Approx.Interval
import Numeric.ER.Real.Arithmetic.Elementary
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainIntBox, DomainBoxMappable)
import Numeric.ER.BasicTypes
import Numeric.ER.Misc

import qualified Data.Map as Map

{-|
    
-}
ienclSqrt ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    EffortIndex {-^ for calls to other ER functions -} -> 
    Int {-^ how many times to iterate -} -> 
    ((ERChebPoly box b, ERChebPoly box b), Bool) ->
    ((ERChebPoly box b, ERChebPoly box b), Bool)
ienclSqrt maxDegree maxSize ix maxIters (e@(ln, h), isDAC) = 
    ((lnRDown,hRDown), isDAC)
    where
    lnRDown = chplNeg lRUp
    hRDown = chplNeg hnRUp
    (_, lRUp) = enclSqrt maxDegree maxSize ix maxIters (ln,chplNeg ln)
    (hnRUp, _) = enclSqrt maxDegree maxSize ix maxIters (chplNeg h,h)

{-|
    
-}
ienclExp ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    EffortIndex {-^ for calls to other ER functions -} -> 
    ((ERChebPoly box b, ERChebPoly box b), Bool) ->
    ((ERChebPoly box b, ERChebPoly box b), Bool)
ienclExp maxDegree maxSize ix (e@(ln, h), isDAC) = 
    ((lnEDown,hEDown), isDAC)
    where
    lnEDown = chplNeg lEUp
    hEDown = chplNeg hnEUp
    (_, lEUp) = enclExp maxDegree maxSize ix (ln,chplNeg ln)
    (hnEUp, _) = enclExp maxDegree maxSize ix (chplNeg h,h)
    
--{-|
--    Approximate the pointwise exponential of a polynomial enclosure.
---}
--enclExp ::
--    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
--    Int {-^ maximum polynomial degree -} -> 
--    Int {-^ maximum term count -} -> 
--    EffortIndex {-^ used to derive minimum approx Taylor degree -} -> 
--    (ERChebPoly box b, ERChebPoly box b) ->
--    (ERChebPoly box b, ERChebPoly box b)
--enclExp maxDegree maxSize ix pEncl =
----    unsafePrintReturn
----    ( 
----        "chplExp:" ++
----        "\n pEncl = " ++ show pEncl ++
----        "\n upperB = " ++ show upperB ++
----        "\n lowerB = " ++ show lowerB ++
----        "\n m = " ++ show m ++
----        "\n expM = " ++ show expM ++
----        "\n r = " ++ show r ++
----        "\n a_int = " ++ show a_int ++
----        "\n a_base = " ++ show a_base ++
----        "\n pNear0Encl = " ++ show (pNear0Encl) ++
----        "\n expNear0 = " ++ show (expNear0) ++
------        "\n chplPow maxDegree (expNear0Up pNear0Up) a_int = " ++ show (chplPow maxDegree (expNear0Up pNear0Up) a_int)
----        "\n result = "
----    )
----    $ 
--    result
--    where
--    result =
--        enclRAScale maxDegree maxSize expM $ enclPow maxDegree maxSize expNear0 a_int
--
--    (lowerB, upperB) = enclBounds ix pEncl
--    mB = (upperB + lowerB) / 2
--    rB = (upperB - lowerB) / 2
--    r = ERInterval rB rB
--    m = ERInterval mB mB
--    expM = max 0 $ erExp_IR ix m
--    
--    -- scale the problem down for polynomials whose value is always near zero:
--    pNear0Encl = 
--        enclRAScale maxDegree maxSize (recip a_base) (pEncl -: (enclConst mB))
--    rNear0 = r / a_base
--    a_base = fromInteger a_int
--    a_int = max 1 $ floor rB -- could this be too high?
--    
--    expNear0 =
--        expTayNear0 +: (chplConst 0, chplConst (erintv_right truncCorrNear0))
--        -- the difference between exact exp and finite Taylor expanstion is an increasing function
--        -- therefore it is enough to compensate the error at the right-most point
--    expTayNear0 =
--        expAux pNear0Encl 1 (RA.setGranularity coeffGr 1)
--    expAux p0Encl nextDegree thisCoeff
--            | nextDegree > taylorDegree =
--                enclRAConst thisCoeff
--            | otherwise =
--                (enclRAConst thisCoeff) +: (p0Encl *: (expAux p0Encl (nextDegree + 1) nextCoeff))
--            where
--            (*:) = enclMultiply maxDegree maxSize
--            nextCoeff = 
--                thisCoeff / (fromInteger nextDegree)
--    taylorDegree = 1 + 2 * (ix `div` 6)
--    coeffGr = effIx2gran $ 10 + 3 * taylorDegree
--    -- correction of truncation error (highest at the right-most point):
--    truncCorrNear0 = expRNear0 - tayRNear0
--    expRNear0 = erExp_R ix rNear0
--    tayRNear0 = 
--        ERInterval
--            (negate $ getConst valueAtRNear0LowNeg) 
--            (getConst valueAtRNear0High)
--    getConst p = 
--        case chplGetConst p of Just c -> c; _ -> 0
--    (valueAtRNear0LowNeg, valueAtRNear0High) =
--        expAux rNear0Encl 1 (RA.setGranularity coeffGr 1)
--    rNear0Encl = enclRAConst rNear0
--    _ = [rNear0Encl, pEncl] -- help the typechecker...
--
--{-|
--    Approximate the pointwise integer power of an enclosure.
---}
--enclPow ::
--    (B.ERRealBase b, Integral i, DomainBox box varid Int, Ord box) => 
--    Int {-^ maximum polynomial degree -} -> 
--    Int {-^ maximum term count -} -> 
--    (ERChebPoly box b, ERChebPoly box b) ->
--    i ->
--    (ERChebPoly box b, ERChebPoly box b)
--        {-^ lower (negated) and upper bound -}
--enclPow maxDegree maxSize pEncl n
--    | n == 0 =
--        enclConst 1
--    | n == 1 =
--        pEncl
--    | even n =
--        powEvenEncl 
--    | odd n =
--        enclMultiply maxDegree maxSize powEvenEncl pEncl
--    where
--    powEvenEncl =
--        enclMultiply maxDegree maxSize powHalfEncl powHalfEncl 
--    powHalfEncl = 
--        enclPow maxDegree maxSize pEncl halfN
--    halfN = n `div` 2
--    
--{-|
--    Approximate the pointwise natural logarithm of an enclosure. 
---}
--enclLog ::
--    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
--    Int {-^ maximum polynomial degree -} -> 
--    Int {-^ maximum term count -} -> 
--    EffortIndex {-^  ?? -} -> 
--    (ERChebPoly box b, ERChebPoly box b) ->
--    (ERChebPoly box b, ERChebPoly box b)
--enclLog maxDegree maxSize ix p =
--    error "ERChebPoly: chplLog: not implemented yet"
--
--{-|
--    Approximate the pointwise sine of an enclosure.
--    
--    Assuming the polynomial range is [-pi/2, pi/2]. 
---}
--enclSine ::
--    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
--    Int {-^ maximum polynomial degree -} -> 
--    Int {-^ maximum term count -} -> 
--    EffortIndex {-^ how hard to try (determines Taylor degree and granularity) -} -> 
--    (ERChebPoly box b, ERChebPoly box b) ->
--    (ERChebPoly box b, ERChebPoly box b)
--enclSine maxDegree maxSize ix pEncl =
----        unsafePrint
----        (
----            "ERChebPoly: enclSine: "
----            ++ "\n pEncl = " ++ show pEncl
----            ++ "\n ranLargerEndpoint = " ++ show ranLargerEndpoint
----            ++ "\n sineEncl = " ++ show sineEncl
----        ) $
--        sineEncl
--        where
--        sineEncl =
--            enclAddErr sineErrorBound $
--            enclMultiply maxDegree maxSize pEncl sineTayEncl
--        (sineTayEncl, sineErrorTermDegree, sineErrorTermCoeffRA) =
--            sincosTaylorAux maxDegree maxSize True pSqrEncl taylorDegree 1 one
--        one = RA.setGranularity coeffGr 1
--        pSqrEncl = enclMultiply maxDegree maxSize pEncl pEncl
--        sineErrorBound =
--            case sineErrorBoundRA of 
--                ERInterval lo hi -> hi
--                ERIntervalAny -> B.plusInfinity
--            where
--            sineErrorBoundRA =        
--                (ranLargerEndpointRA ^ sineErrorTermDegree) * sineErrorTermCoeffHighRA
--            sineErrorTermCoeffHighRA =
--                snd $ RA.bounds $ abs sineErrorTermCoeffRA
--        ranLargerEndpointRA =
--            ERInterval ranLargerEndpoint ranLargerEndpoint
--        ranLargerEndpoint =
--            max (abs ranLowB) (abs ranHighB)
--        (ranLowB, ranHighB) = enclBounds ix pEncl
--        taylorDegree = effIx2int $ ix `div` 3
--        coeffGr = effIx2gran $ ix
--        
--{-|
--    Approximate the pointwise cosine of an enclosure.
--    
--    Assuming the polynomial range is [-pi/2, pi/2]. 
---}
--enclCosine ::
--    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
--    Int {-^ maximum polynomial degree -} -> 
--    Int {-^ maximum term count -} -> 
--    EffortIndex {-^ how hard to try (determines Taylor degree and granularity) -} -> 
--    (ERChebPoly box b, ERChebPoly box b) ->
--    (ERChebPoly box b, ERChebPoly box b)
--enclCosine maxDegree maxSize ix pEncl =
----        unsafePrint
----        (
----            "ERChebPoly: chplCosine: "
----            ++ "\n pEncl = " ++ show pEncl
----            ++ "\n ranLargerEndpoint = " ++ show ranLargerEndpoint
----            ++ "\n cosineEncl = " ++ show cosineEncl
----        ) $
--        cosineEncl
--        where
--        cosineEncl =
--            enclAddErr cosineErrorBound $
--            cosineTayEncl
--        (cosineTayEncl, cosineErrorTermDegree, cosineErrorTermCoeffRA) =
--            sincosTaylorAux maxDegree maxSize True pSqrEncl taylorDegree 0 one
--        one = RA.setGranularity coeffGr 1
--        pSqrEncl = enclMultiply maxDegree maxSize pEncl pEncl
--        cosineErrorBound =
--            case cosineErrorBoundRA of 
--                ERInterval lo hi -> hi
--                ERIntervalAny -> B.plusInfinity
--            where
--            cosineErrorBoundRA =        
--                (ranLargerEndpointRA ^ cosineErrorTermDegree) * cosineErrorTermCoeffHighRA
--            cosineErrorTermCoeffHighRA =
--                snd $ RA.bounds $ abs cosineErrorTermCoeffRA
--        ranLargerEndpointRA =
--            ERInterval ranLargerEndpoint ranLargerEndpoint
--        ranLargerEndpoint =
--            max (abs ranLowB) (abs ranHighB)
--        (ranLowB, ranHighB) = enclBounds ix pEncl
--        taylorDegree = effIx2int $ ix `div` 3
--        coeffGr = effIx2gran $ ix
--    
--sincosTaylorAux ::
--    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
--    Int {-^ maximum polynomial degree -} ->
--    Int {-^ maximum term count -} ->
--    Bool {-^ is sine ? -} -> 
--    (ERChebPoly box b, ERChebPoly box b) ->
--    Int {-^ how far to go in the Taylor series -} ->
--    Int {-^ degree of the term now being constructed -} ->
--    ERInterval b {-^ the coefficient of the term now being constructed -} -> 
--    ((ERChebPoly box b, ERChebPoly box b),
--     Int,
--     ERInterval b)
--    {-^ 
--        Bounds for the series result and information about the first discarded term,
--        from which some bound on the uniform error can be deduced.
--    -} 
--sincosTaylorAux 
--        maxDegree maxSize resultPositive pSqrEncl tayDegree 
--        thisDegree thisCoeffRA =
--    sct thisDegree thisCoeffRA
--    where
--    sct thisDegree thisCoeffRA
--        | nextDegree > tayDegree =
----            unsafePrint
----            (
----                "ERChebPoly: sincosTaylorAux: "
----                ++ "\n thisCoeffRA = " ++ show thisCoeffRA
----                ++ "\n nextDegree = " ++ show nextDegree
----            )
--            (thisCoeffEncl, nextDegree, nextCoeffRA)
--        | otherwise =
----            unsafePrint
----            (
----                "ERChebPoly: chplSine: taylorAux: "
----                ++ "\n thisCoeffRA = " ++ show thisCoeffRA
----                ++ "\n nextDegree = " ++ show nextDegree
----                ++ "\n errorTermCoeffRA = " ++ show errorTermCoeffRA
----                ++ "\n errorTermDegree = " ++ show errorTermDegree
----            )
--            (resultEncl, errorTermDegree, errorTermCoeffRA) 
--        where
--        thisCoeffEncl = enclRAConst thisCoeffRA
--        resultEncl =
--            thisCoeffEncl +: (enclMultiply maxDegree maxSize pSqrEncl restEncl)
--        (restEncl, errorTermDegree, errorTermCoeffRA) =
--            sct nextDegree nextCoeffRA
--        nextDegree = thisDegree + 2
--        nextCoeffRA = thisCoeffRA / nextCoeffDenominatorRA
--        nextCoeffDenominatorRA =
--            fromInteger $ toInteger $ 
--                negate $ nextDegree * (nextDegree - 1)
--
--{-|
--    Approximate the pointwise arcus tangens of an enclosure. 
---}
--enclAtan ::
--    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
--    Int {-^ maximum polynomial degree -} -> 
--    Int {-^ maximum term count -} -> 
--    EffortIndex {-^ how far to go in the Euler's series -} ->
--    (ERChebPoly box b, ERChebPoly box b) ->
--    (ERChebPoly box b, ERChebPoly box b)
--{- arctan using Euler's series:
--    (http://en.wikipedia.org/wiki/Inverse_trigonometric_function#Infinite_series)
--    
--    (x / (1 + x^2)) * (1 + t*2*1/(2*1 + 1)*(1 + t*2*2/(2*2 + 1)*(1 + ... (1 + t*2*n/(2*n+1)*(1 + ...)))))
--    where
--    t = x^2/(1 + x^2)
--    
--    where the tail  (1 + t*2*n/(2*n+1)*(1 + ...)) is inside the interval:
--    [1, 1 + x^2]
---}
--enclAtan maxDegree maxSize ix pEncl@(pLowNeg, pHigh)
--    | True = -- pLowerBound >= (-3) && pUpperBound <= 3 =
--        enclAtanAux maxDegree maxSize ix (Just pSquareEncl) pEncl
--    | otherwise = -- too far from 0, needs atan(x) = 2*atan(x/(1+sqrt(1+x^2)))
--        case avoidingDivBy0 of
--            True ->
--                enclScale maxDegree maxSize 2 $
--                    enclAtanAux maxDegree maxSize ix Nothing $
--                        enclMultiply maxDegree maxSize pEncl $
--                            enclRecip maxDegree maxSize ix (maxDegree + 1) $
--                                onePlusSqrtOnePlusPSquare
--    where
--    (pLowerBound, pUpperBound) = enclBounds ix pEncl
--    onePlusSqrtOnePlusPSquare =
--        enclAddConst 1 $
--            enclSqrt maxDegree maxSize ix maxDegree pSquarePlus1Encl
--    avoidingDivBy0 =
--        lower1 > 0 && lower2 > 0
--        where
--        (lower1, _) = enclBounds ix pSquarePlus1Encl
--        (lower2, _) = enclBounds ix onePlusSqrtOnePlusPSquare
--    pSquareEncl = 
--        enclSquare maxDegree maxSize pEncl
--    pSquarePlus1Encl = 
--        pSquareEncl +: (enclConst 1)
--    
--    
--enclAtanAux maxDegree maxSize ix maybePSquareEncl pEncl@(pLowNeg, pHigh) 
--    | avoidingDivBy0 = resultEncl
--    | otherwise = 
--        (piHalfUp, piHalfUp) -- [-22/14,22/14] is always safe...    
--    where            
--    piHalfUp = chplConst $ 22/7
--    avoidingDivBy0 =
--        lower > 0
--        where
--        (lower, _) = enclBounds ix pSquarePlus1Encl
--    resultEncl =
--        enclMultiply maxDegree maxSize 
--            pOverPSquarePlus1Encl preresEncl
--    preresEncl = 
--        series termsCount 2
--    termsCount = 
--        max 0 $ ix `div` 3
--    gran = effIx2gran ix
--    series termsCount coeffBase 
--        | termsCount > 0 =
--            enclAddConst 1 $
--                enclRAScale maxDegree maxSize coeff $
--                    enclMultiply maxDegree maxSize 
--                        pSquareOverPSquarePlus1Encl $
--                            series (termsCount - 1) (coeffBase + 2)
--        | otherwise =
--            enclAddConst 1 $
--            (chplConst 0, pSquareHigh)
--        where
--        coeff = coeffBase / (coeffBase + 1)
--        
--    pSquareEncl@(pSquareLowNeg, pSquareHigh) = 
--        case maybePSquareEncl of
--            Just pSquareEncl -> pSquareEncl
--            Nothing ->
--                enclSquare maxDegree maxSize pEncl
--    pSquarePlus1Encl = 
--        pSquareEncl +: (enclConst 1)
--    recipPSquarePlus1Encl = 
--        enclRecip maxDegree maxSize ix (maxDegree + 1) pSquarePlus1Encl
--    pSquareOverPSquarePlus1Encl = 
--         enclMultiply maxDegree maxSize pSquareEncl recipPSquarePlus1Encl
--    pOverPSquarePlus1Encl =
--         enclMultiply maxDegree maxSize pEncl recipPSquarePlus1Encl
