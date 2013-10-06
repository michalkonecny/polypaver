{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Bounds
    Description :  (internal) bounds of single and multiple polynomials
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Internal module for "Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom".
    
    Implementation of various functions related to the bounds of polynomials.    
-}
module Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Bounds 
where

import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Basic
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Ring
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Reduce
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Eval
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Derivative

import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Base as B
import Numeric.ER.Real.Approx.Interval
import Numeric.ER.Real.Arithmetic.LinearSolver
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainBoxMappable, DomainIntBox)
import Numeric.ER.BasicTypes
import Numeric.ER.Misc

import qualified Data.Map as Map

import Data.List

{-|
    Find an upper bound on a polynomial over the 
    unit domain [-1,1]^n.  

    Quick method that does not converge to exact result with increasing 
    effort index.
-}
chplUpperBound ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    EffortIndex {-^ how hard to try -} ->
    ERChebPoly box b ->
    b
chplUpperBound ix p = snd $ chplBounds ix p

{-|
    Find a lower bound on a polynomial over the 
    unit domain [-1,1]^n.  

    Quick method that does not converge to exact result with increasing 
    effort index.
-}
chplLowerBound ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    EffortIndex {-^ how hard to try -} ->
    ERChebPoly box b ->
    b
chplLowerBound ix p = fst $ chplBounds ix p

{-|
    Find both lower and upper bounds on a polynomial over the 
    unit domain [-1,1]^n.  

    Quick method that does not converge to exact result with increasing 
    effort index.
-}
chplBounds ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    EffortIndex {-^ how hard to try -} ->
    ERChebPoly box b ->
    (b,b)
chplBounds = 
    chplBoundsAffine

{-|
    Find an upper bound on a polynomial over the 
    unit domain [-1,1]^n.  
-}
chplUpperBoundExpensive ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    EffortIndex {-^ how hard to try -} ->
    ERChebPoly box b ->
    b
chplUpperBoundExpensive ix p = snd $ chplBoundsExpensive ix p

{-|
    Find a lower bound on a polynomial over the 
    unit domain [-1,1]^n.  
    
    Quick method that does not converge to exact result with increasing 
    effort index.
-}
chplLowerBoundExpensive ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    EffortIndex {-^ how hard to try -} ->
    ERChebPoly box b ->
    b
chplLowerBoundExpensive ix p = fst $ chplBoundsExpensive ix p

{-|
    Find both lower and upper bounds on a polynomial over the 
    unit domain [-1,1]^n.
    
    Quick method that does not converge to exact result with increasing 
    effort index.
-}
chplBoundsExpensive ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    EffortIndex {-^ how hard to try -} ->
    ERChebPoly box b ->
    (b,b)
chplBoundsExpensive = chplBoundsByDerivative

{-|
    Find bounds on a polynomial over the unit domain [-1,1]^n.
    
    Fast but inaccurate method, in essence
    taking the maximum of the upper affine reduction.
-}
chplBoundsAffine ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
    EffortIndex {-^ how hard to try -} ->
    ERChebPoly box b ->
    (b,b)
chplBoundsAffine ix p@(ERChebPoly coeffs) =
--    unsafePrintReturn
--    (
--        "chplBoundsAffine:"
--        ++ "\n p = " ++ show p
--        ++ "\n noConstCoeffAbsSum = " ++ show noConstCoeffAbsSum
--        ++ "\n result = "
--    )    
    result
    where
    result =
        (constTerm `plusDown` (- noConstCoeffAbsSum),
         constTerm `plusUp` noConstCoeffAbsSum)
    noConstCoeffAbsSum = Map.fold plusUp 0 absCoeffs
    absCoeffs = Map.map abs $ Map.delete chplConstTermKey coeffs
    constTerm = Map.findWithDefault 0 chplConstTermKey coeffs


{-|
    Find a close upper bound of a polynomial over the 
    unit domain [-1,1]^n.
    
    Approximates all local extrema and computes their maximum.  
-}
chplBoundsByDerivative ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    EffortIndex {-^ how hard to try looking for peaks -} ->
    ERChebPoly box b ->
    (b,b)
chplBoundsByDerivative ix p =
--    unsafePrint
--    (
--        "chplBoundsByDerivative: "
--        ++ "\n extremaValues = " ++ show extremaValues
--    ) $
    (lowerBound, upperBound)
    where
    lowerBound = foldl1 min $ map fst extremaValues 
    upperBound = foldl1 max $ map snd extremaValues
    ra2bb (ERInterval l r) = (l,r)
    b2ra b = ERInterval b b
    extremaValues = 
        collectValuesOnFaces vars varDerivatives (p,p)
        where
        vars = chplGetVars p
        varDerivatives = -- var |-> (lower, upper) bounds on partial derivative
            Map.fromList $
                map getDerivatives vars
        getDerivatives var =
            (var,
                chplBall2DownUp $
                    ballDifferentiate p var)
    collectValuesOnFaces varsSpecialise varDerivatives (pDown, pUp) =
--        unsafePrint
--        (
--            "chplBoundsByDerivative: collectValuesOnFaces: "
--            ++ "\n vars = " ++ (show $ Map.keys varDerivatives)
--            ++ "\n valuesThisFace = " ++ show valuesThisFace
--        ) $
        valuesThisFace ++ (valuesSubFaces varsSpecialise)
        where
        valuesThisFace =
            collectExtremeValues varDerivatives (pDown, pUp)
        valuesSubFaces [] = []
        valuesSubFaces (var : vars) =
            (collectValuesOnFaces vars varDerivativesNoVarL (pDownNoVarL, pUpNoVarL))
            ++
            (collectValuesOnFaces vars varDerivativesNoVarR (pDownNoVarR, pUpNoVarR))
            ++
            (valuesSubFaces vars)
            where
            (pDownNoVarR, pUpNoVarR) = substVarR (pDown, pUp)
            (pDownNoVarL, pUpNoVarL) = substVarL (pDown, pUp)
            substVarL = substVar (-1)
            substVarR = substVar 1
            substVar val (pDown, pUp) =
                (fst $ chplPartialRAEval ra2bb pDown $ DBox.singleton var val,
                 snd $ chplPartialRAEval ra2bb pUp $ DBox.singleton var val)
            varDerivativesNoVarL =
                Map.map substVarL varDerivativesNoVar 
            varDerivativesNoVarR =
                Map.map substVarR varDerivativesNoVar 
            varDerivativesNoVar = 
                Map.delete var varDerivatives
    collectExtremeValues varDerivatives (pDown, pUp)
        | null varsNoConst =
--            unsafePrint
--            (
--                "chplBoundsByDerivative: collectExtremeValues:" 
--                ++ "\n null varsNoConst"
--                ++ "\n varDerivatives = " ++ show varDerivatives
--            )
            -- corner or near constant function 
            [pEvalAt unitDomBox] 
        | otherwise =
--            unsafePrint
--            (
--                "chplBoundsByDerivative: collectExtremeValues:" 
--                ++ "\n varDerivatives = " ++ show varDerivatives
--                ++ "\n boxesWithPotentialExtrema = " ++ show boxesWithPotentialExtrema
--            ) $
            map pEvalAt boxesWithPotentialExtrema
        where
        boxesWithPotentialExtrema = 
            paveFindBoxes [(unitDomBox,0)] 
        varDerivativesNoZeros =
            Map.filter (not . isConstWithZero) varDerivatives
            where
            isConstWithZero (pDown, pUp) =
                (snd $ chplBoundsAffine ix pDown) <= 0
                &&
                (fst $ chplBoundsAffine ix pUp) >= 0
--                case (chplGetConst pDown, chplGetConst pUp) of
--                    (Just cDown, Just cUp) ->
--                        cDown <= 0 && cUp >= 0 
--                    _ -> False 
        vars = Map.keys varDerivatives
        varsNoConst = Map.keys varDerivativesNoZeros
        varsNoConstLength = length varsNoConst
        pEvalAt = evalAt (pDown, pUp)
        evalAt (pDown,pUp) box =
            (fst $ ra2bb $ chplRAEval b2ra pDown box,
             snd $ ra2bb $ chplRAEval b2ra pUp box)
        unitDomBox =
            DBox.fromList $ zip vars (repeat unitInterval)
        unitInterval = ((-1) RA.\/ 1)
        maxDepth = fromInteger $ toInteger $ max 3 ix
        keepBox box =
            and $ map evalDeriv $ Map.elems varDerivativesNoZeros
            where
            evalDeriv derivBounds = hasZero $ evalAt derivBounds box  
            hasZero (l,h) = l <= 0 && h >= 0
        paveFindBoxes [] = [] 
        paveFindBoxes boxes@((box, depth) : boxesRest) 
            | keepBox box =
                case depth < maxDepth of
                    True ->
                        paveFindBoxes ((boxL, newDepth) : (boxR, newDepth) : boxesRest)
                    False ->
                        box : (paveFindBoxes boxesRest)
            | otherwise =
                paveFindBoxes boxesRest
            where
            var = varsNoConst !! (depth `mod` varsNoConstLength)
            (boxL, boxR) = DBox.split box var Nothing
            newDepth = depth + 1


--{-|
--    Find a close upper bound on a quadratic polynomial over the 
--    unit domain [-1,1]^n.  
--
--    Much slower and somewhat more accurate method, in essence
--    taking the maximum of the upper quadratic reduction.
--    
--    !!! Not properly tested !!!
---}
--chplUpperBoundQuadr ::
--    (B.ERRealBase b, DomainBox box varid Int, Ord box,
--     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b],
--     DomainBoxMappable boxra boxra varid (ERInterval b) (ERInterval b), 
--     DomainIntBox boxra varid (ERInterval b), Num varid, Enum varid) => 
--    EffortIndex {-^ how hard to try looking for peaks -} ->
--    ERChebPoly box b ->
--    b
--chplUpperBoundQuadr ix p@(ERChebPoly coeffs) =
--    quadBound (coeffsQ, vars)
--    where
--    pQ@(ERChebPoly coeffsQ) = chplReduceDegreeUp 2 p
--    vars = chplGetVars pQ
--    quadBound (coeffs, vars)
--        | null vars =
--            Map.findWithDefault 0 chplConstTermKey coeffs
--        | hasInteriorPeak =
--            foldl max peakValue edgeBounds
--        | otherwise =
--            foldl1 max edgeBounds
--        where
--        edgeBounds =
--            map quadBound $ concat $ map removeVar vars
--        (hasInteriorPeak, peakValue) =
--            case maybePeak of
--                Just peak ->
--                    (noPositiveSquare -- if any term x^2 has a positive coeff, there is no peak  
--                     &&
--                     (and $ map maybeInUnit $ DBox.elems peak)
--                    ,
--                     erintv_right $
--                     chplRAEval makeInterval p peak
--                    )
--                Nothing -> (False, undefined)
--            where
--            noPositiveSquare =
--                and $ map (<= 0) $ map getQuadCoeff vars
--            getQuadCoeff var = 
--                Map.findWithDefault 0 (DBox.singleton var 2) coeffs
--            maybeInUnit r =
--                case (RA.compareReals r (-1), RA.compareReals (1) r) of
--                    (Just LT, _) -> False -- ie r < -1
--                    (_, Just LT) -> False -- ie r > 1
--                    _ -> True
--        maybePeak =
--            linearSolver
--                (map derivZeroLinearEq vars)
--                (DBox.fromList $ map (\v -> (v,(-1) RA.\/ 1)) vars)
--                (2^^(-ix))
--            where
--            derivZeroLinearEq var =
--                (linCoeffs, - constCoeff)
--                where
--                constCoeff =
--                    makeInterval $
--                    Map.findWithDefault 0 (DBox.singleton var 1) coeffs
--                      -- recall T_1(x) = x, T_1'(x) = 1
--                linCoeffs =
--                    DBox.fromList $
--                        (var, 4 * quadCoeff) -- T_2(x) = 2*x^2 - 1; T_2'(x) = 4*x
--                        : (map getVarVarCoeff $ var `delete` vars)
--                quadCoeff =
--                    makeInterval $
--                    Map.findWithDefault 0 (DBox.singleton var 2) coeffs
--                getVarVarCoeff var2 =
--                    (var2,
--                      makeInterval $
--                      Map.findWithDefault 0 (DBox.fromList [(var,1), (var2,1)]) coeffs)
--        makeInterval b = ERInterval b b
--        removeVar var =
--            [(substVar True, newVars), 
--             (substVar False, newVars)]
--            where
--            newVars = var `delete` vars
--            substVar isOne =
--                chplCoeffs $
--                    foldl (+^) (chplConst 0) $ 
--                        map (makeMonomial isOne) $ 
--                            Map.toList coeffs
--            makeMonomial isOne (term, coeff) =
--                ERChebPoly $ Map.fromList $
--                case (DBox.toList term) of
--                    [(v,2)] | v == var ->
--                        [(chplConstTermKey, coeff)]
--                    [(v,1)] | v == var ->
--                        [(chplConstTermKey, 
--                          case isOne of True -> coeff; False -> - coeff)]
--                    [(v1,1), (v2,1)] | v1 == var ->
--                        [(DBox.fromList [(v2,1)], 
--                          case isOne of True -> coeff; False -> - coeff)]
--                    [(v1,1), (v2,1)] | v2 == var ->
--                        [(DBox.fromList [(v1,1)], 
--                          case isOne of True -> coeff; False -> - coeff)]
--                    _ ->
--                        [(term, coeff)]

{-|
     Approximate from below and  from above the pointwise maximum of two polynomials
-}
chplMax ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    ERChebPoly box b ->
    ERChebPoly box b ->
    (ERChebPoly box b, ERChebPoly box b)
chplMax maxDegree maxSize p1 p2 =
    (p1 +. differenceDown, p1 +^ differenceUp)
    where
    (differenceDown, _) = chplNonneg maxDegree maxSize p2MinusP1Down
    (_, differenceUp) = chplNonneg maxDegree maxSize $ p2MinusP1Up
    (p2MinusP1Down, p2MinusP1Up) = chplBall2DownUp $ ballAdd p2 (chplNeg p1)

chplMaxDn m s a b = fst $ chplMax m s a b
chplMaxUp m s a b = snd $ chplMax m s a b
chplMinDn m s a b = fst $ chplMin m s a b
chplMinUp m s a b = snd $ chplMin m s a b

{-|
     Approximate from below and  from above the pointwise minimum of two polynomials
-}
chplMin ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    ERChebPoly box b ->
    ERChebPoly box b ->
    (ERChebPoly box b, ERChebPoly box b)
chplMin m s a b =
    (chplNeg u,chplNeg l)
    where
    (l,u) = chplMax m s (chplNeg a) (chplNeg b)

chplNonnegDown m s p = fst $ chplNonneg m s p
chplNonnegUp m s p = snd $ chplNonneg m s p 
chplNonposDown m s p = fst $ chplNonpos m s p
chplNonposUp m s p = snd $ chplNonpos m s p 

chplNonpos m s p =
    (chplNeg h, chplNeg l)
    where
    (l,h) = chplNonneg m s (chplNeg p)

{-|
     Approximate the function max(0,p(x)) by a polynomial from below
     and from above. 
-}
chplNonneg ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    ERChebPoly box b ->
    (ERChebPoly box b, ERChebPoly box b)
chplNonneg = chplNonnegCubic

{-|
    A version of 'chplNonneg' using a cubic approximation. 
-}
chplNonnegCubic ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    ERChebPoly box b ->
    (ERChebPoly box b, ERChebPoly box b)
chplNonnegCubic maxDegree maxSize p
    | upperB <= 0 = (chplConst 0, chplConst 0)
    | lowerB >= 0 = (p, p)
    | not allInterimsBounded = (chplConst (B.plusInfinity), chplConst (B.plusInfinity))
    | otherwise = -- ie lowerB < 0 < upperB: polynomial may be crossing 0...
--        unsafePrintReturn
--        (
--            "chplNonnegCubic:"
--            ++ "\n p = " ++ show p
--            ++ "\n maxDegree = " ++ show maxDegree
--            ++ "\n maxSize = " ++ show maxSize
--            ++ "\n upperB = " ++ show upperB
--            ++ "\n lowerB = " ++ show lowerB
--            ++ "\n a0 = " ++ show a0
--            ++ "\n a1 = " ++ show a1
--            ++ "\n a2 = " ++ show a2
--            ++ "\n a3 = " ++ show a3
--            ++ "\n b = " ++ show b
--            ++ "\n rb = " ++ show rb
--            ++ "\n correctionB = " ++ show correctionB
--            ++ "\n valueAt0B = " ++ show valueAt0B
--            ++ "\n result = "
--        )
        -- work out the cubic polynomial (a3*x^3 + a2*x^2 + a1*x + a0) / b 
        -- that hits 0 at lowerB with derivative 0 
        -- and hits upperB at upperB with derivative 1 
        (chplAddConstDown (- valueAt0B) cubicAppliedOnPDown, 
         chplAddConstUp correctionB cubicAppliedOnPUp)
    where    
    (lowerB, upperB) = chplBounds 10 p
    (cubicAppliedOnPDown, cubicAppliedOnPUp, width) =
        p0 `scaleByPositiveConsts` (rbLo, rbHi)
        where
        p0 = (multiplyByP p1) `addConsts` (a0Lo, a0Hi) -- ie p*(p*(p * a3 + a2) + a1) + a0 enclosure
        p1 = (multiplyByP p2) `addConsts` (a1Lo, a1Hi) -- ie p*(p * a3 + a2) + a1 enclosure
        p2 = (multiplyByP p3) `addConsts` (a2Lo, a2Hi) -- ie p * a3 + a2 enclosure
        p3 = (chplConst a3Lo, chplConst a3Hi, a3Hi - a3Lo) -- ie a3 enclosure
    multiplyByP (lo,hi,wd) =
        (ploRed, phiRed, pwd)
        where
        ploRed = reduceDgSzDown plo
        phiRed = reduceDgSzUp phi 
        pwd = chplUpperBound 10 $ phiRed -^ ploRed 
        (plo, phi, _) = chplTimesLoHi p (lo,hi,wd)
    reduceDgSzUp =
        chplReduceTermCountUp maxSize . chplReduceDegreeUp maxDegree
    reduceDgSzDown =
        chplReduceTermCountDown maxSize . chplReduceDegreeDown maxDegree
    addConsts (lo, hi, wd) (cLo, cHi) =
        (alo, ahi, wd + wdlo + wdhi)
        where
        (alo, _, wdlo) = chplBall2DownUpWd $ ballAddConst cLo lo 
        (_, ahi, wdhi) = chplBall2DownUpWd $ ballAddConst cHi hi 
    scaleByPositiveConsts (lo, hi, wd) (cLo, cHi) =
        (slo, shi, wd + wdlo + wdhi)
        where
        (slo, _, wdlo) = chplBall2DownUpWd $ ballScale cLo lo 
        (_, shi, wdhi) = chplBall2DownUpWd $ ballScale cHi hi 
    
    -- convert interval coefficients to pairs of bounds:
    ERInterval rbLo rbHi = rb
    ERInterval a3Lo a3Hi = a3
    ERInterval a2Lo a2Hi = a2
    ERInterval a1Lo a1Hi = a1
    ERInterval a0Lo a0Hi = a0
    allInterimsBounded = 
        and $ map RA.isBounded [w, s, rb, a0, a1, a2, a3, correction]
    {-
      The cubic polynomial's coefficients are calculated by solving a system of 4 linear eqs.
      The generic solution is as follows:
         b = (r - l)^3   always positive
         a3 = -(r + l)
         a2 = 2*(r^2 + r*l + l^2)
         a1 = -l*(4*r^2 + r*l + l^2)
         a0 = 2*r^2*l^2
    -}
    rb = recip b
    b = w3 -- = w^3 -- see below
    w = r - l
    r = ERInterval upperB upperB
    l = ERInterval lowerB lowerB
    --
    a3 = - s
    s = r + l
    --
    a2 = 2 * (r2PrlPl2)
    r2PrlPl2 = s2 - rl
    rl = r * l
    --
    a1 = (- l) * (r2PrlPl2 + 3*r2)
    a0 = 2*r2*l2
    -- interval arithmetic tricks to speed it up and reduce dependency errors:
    w3 = ERInterval (wLo * wLo * wLo) (wHi * wHi * wHi) -- x^3 is monotone 
    ERInterval wLo wHi = w
    s2 = ERInterval (max 0 s2Lo) s2Hi
    s2Lo = min sLo2 sHi2 
    s2Hi = max sLo2 sHi2
    sLo2 = sLo * sLo
    sHi2 = sHi * sHi 
    ERInterval sLo sHi = s    
    r2 = ERInterval (upperB `timesDown` upperB) (upperB `timesUp` upperB)    
    l2 = ERInterval (lowerB `timesDown` lowerB) (lowerB `timesUp` lowerB)
    {- 
        The cubic polynomial may sometimes fail to dominate
        x or sometimes it dips below 0.
        Work out the amount by which it has to be lifted up
        to fix these problems. 
    -}
    ERInterval _ correctionB = correction
    correction =
        case (RA.compareReals (2 * r2) (l*s), RA.compareReals (2 * l2) (r*s)) of
            (Just LT, _) ->
                (peak0 * (peak0 * (peak0 * (-a3) - a2) - a1) - a0) / b
            (_, Just LT) ->
                ((peakP * (peakP * (peakP * (-a3) - a2) - a1) - a0) / b) + peakP
            _ -> 0
        where
        peak0 = (l + 4*r2/s) / 3 
        peakP = (r + 4*l2/s) / 3
    {-
        The same cubic polynomial can be used as a lower bound when
        we subtract its value at 0 rounded upwards.
    -}
    valueAt0B = 
        case a0 / b of
            ERInterval lo hi -> hi

{-|
    Multiply a polynomial by an enclosure (with non-negated lower bound).
-}
chplTimesLoHi ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    ERChebPoly box b ->
    (ERChebPoly box b, ERChebPoly box b, b) ->
    (ERChebPoly box b, ERChebPoly box b, b)
chplTimesLoHi p1 (p2Low, p2High, p2Width) =
    (prodMid -. (chplConst width), 
     prodMid +^ (chplConst width),
     2 * width)
    where
    prodMid = prodLowUp
    (prodLowDown, prodLowUp, prodLowWidth) = 
        chplBall2DownUpWd $ ballMultiply p1 p2Low
    (prodHighDown, prodHighUp, prodHighWidth) = 
        chplBall2DownUpWd $ ballMultiply p1 p2High
    width = 
        p1Norm `timesUp` p2Width `plusUp` prodLowWidth `plusUp` prodHighWidth
    p1Norm = 
        max (abs $ p1LowerBound) (abs $ p1UpperBound)
    (p1LowerBound, p1UpperBound) = 
        chplBounds ix p1
    ix = 10
