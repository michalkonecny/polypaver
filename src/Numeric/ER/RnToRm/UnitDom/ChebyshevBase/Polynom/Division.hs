{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Division
    Description :  (internal) division of polynomials
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Internal module for "Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom".
    
    Implementation of division applied to basic polynomial enclosures.
-}
module Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Division 
where

import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Basic
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Reduce
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Eval
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Ring
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Bounds
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Enclosure

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
    Approximate the pointwise reciprocal of a polynomial 
    by another polynomial from below and from above
    using the tau method    
    as described in [Mason & Handscomb 2003, p 62]. 
-}
enclRecip ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    EffortIndex {-^ minimum approx degree -} -> 
    Int {-^ degree of tau expansion -} -> 
    (ERChebPoly box b, ERChebPoly box b) ->
    (ERChebPoly box b, ERChebPoly box b)
enclRecip maxDegree maxSize ix tauDegr pEncl@(pLowNeg, pHigh)
    | pIsConst =
        enclRAConst (recip pConst)
    | upperB < 0 = -- range negative
        enclNeg $ enclRecip maxDegree maxSize ix tauDegr (enclNeg pEncl)
    | lowerB > 0 = -- range positive
--        unsafePrintReturn
--        (
--            "ERChebPoly: enclRecip: "
--            ++ "\n pEncl = " ++ show pEncl
--            ++ "\n lowerB = " ++ show lowerB
--            ++ "\n upperB = " ++ show upperB
--            ++ "\n k = " ++ show k
--            ++ "\n pAbove1Encl = " ++ show pAbove1Encl
--            ++ "\n trT1Encl = " ++ show trT1Encl
--            ++ "\n nu = " ++ show nu
--            ++ "\n c0n = " ++ show c0n
--
--            ++ "\n tauDegree = " ++ (show $ tauDegree)
--            ++ "\n tauInv = " ++ (show $ tauInv)
--            ++ "\n tau = " ++ (show $ recip tauInv)
--            ++ "\n errScaleUp = " ++ (show $ errScaleUp)
--            ++ "\n errScaleDown = " ++ (show $ errScaleDown)
--            ++ "\n resEncl = "
--        ) $
        case allInterimsBounded of
            True -> resEncl
            False -> (chplConst 0, chplConst (B.plusInfinity))
    | otherwise = -- cannot establish 0 freedom
        error $
             "ERChebPoly: enclRecip: "
             ++ "cannot deal with estimated range " ++ show ranp
             ++ "of polynomial enclosure: \n" ++ show pEncl
    where
    ranp = ERInterval lowerB upperB
    (lowerB, upperB) = enclBounds ix pEncl
    
    (pIsConst, pConst) = 
        case (chplGetConst pLowNeg, chplGetConst pHigh) of
            (Just pConstLowNeg, Just pConstHigh) ->
                (True, ERInterval (- pConstLowNeg) pConstHigh)
            _ ->
                (False, error "ERChebPoly: chplRecip: internal error")
                     
    tauDegree = max 2 tauDegr
    coeffGr = effIx2gran $ ix
    
    -- translate p to have range above 1:
    k = intLogUp 2 $ ceiling (recip lowerB) -- 2^k * lowerB >= 1
    upperBtr = upperB * 2^k -- upper bound of translated poly
    pAbove1Encl = -- p multiplied by 2^k; range in [1,upperBtr]    
        enclScale maxDegree maxSize (2^k) pEncl
        
    -- translate T_1 to domain [0, upperBtr-1] and apply it to x = (pAbove1 - 1):
    -- T'_1(x) = nu * x - 1 where nu = 2/(upperBtr - 1)
    trT1Encl = 
        enclAddConst (-1) (enclRAScale maxDegree maxSize nu (enclAddConst (-1) pAbove1Encl))
    nu = recip nuInv -- auxiliary constant
    nuInv = (RA.setMinGranularityOuter coeffGr (ERInterval upperBtr upperBtr) - 1) / 2
    
    nuPlus1 = nu + 1
    nuInvPlus1 = nuInv + 1
    nuInvDiv2 = nuInv / 2
        
    -- define such translated T_i's for all i >= 0:
    trTis =
        enclEvalTs maxDegree maxSize trT1Encl
        
    -- construct the result from interval coefficients:
    resEncl = (resLowNeg, resHigh)
    resLowNeg =
        chplScaleUp (2^k) $
            chplScaleUp errScaleDownB $
                scaledTrTisSumLowNeg
    resHigh
        | errScaleUpB > 0 =
            chplScaleUp (2^k) $
                chplScaleUp errScaleUpB $
                    scaledTrTisSumHigh
        | otherwise =
            chplScaleUp (2^k) $
                chplAddConstUp errAddUpB scaledTrTisSumHigh

    ERInterval errScaleDownB _ = nuOverNuPlusTauAns 
    nuOverNuPlusTauAns = (nu / (nu + tauAbs))
    ERInterval _ errScaleUpB = nuOverNuMinusTauAns 
    nuOverNuMinusTauAns = (nu / (nu - tauAbs)) 
    ERInterval _ errAddUpB = tauAbsTimesNuInv 
    tauAbsTimesNuInv = tauAbs * nuInv
    
    allInterimsBounded =
        and $ map RA.isBounded [nuOverNuPlusTauAns, nuOverNuMinusTauAns, nuOverNuMinusTauAns]
    
    tauAbs = abs tau
    tau = recip tauInv
                        
    (scaledTrTisSumLowNeg, scaledTrTisSumHigh) =
        foldl1 (+:) $ zipWith scaleTerm c0n trTis
    scaleTerm c trTEncl =
        enclRAScale maxDegree maxSize (c * tau) trTEncl  
                    
    -- work out the coefficients in interval arithmetic using the tau method:
    c0n = c0 : c1n
    tauInv = c0 * nuInvPlus1 + c1 * nuInvDiv2
    c0 = - c1 * nuPlus1 - c2/2
    (c1 : c2 : _) = c1n
    c1n = reverse $ take n $ csRev
    n = tauDegree
    csRev =
        cn : cnM1 : (csAux cn cnM1)
        where
        cn = 1
        cnM1 = - 2 * nuPlus1
    csAux cn cnM1 =
        cnM2 : (csAux cnM1 cnM2)
        where
        cnM2 = - cn - 2 * nuPlus1 * cnM1
