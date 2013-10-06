{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.EnclosureInner
    Description :  (internal) basic operations for primitive polynomial inner-rounded enclosures
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Internal module for "Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom".
    
    Implementation of selected operations working on pairs
    of polynomials understood as *inner approximations* of function enclosures.
    These are needed to define full Kaucher arithmetic.
-}
module Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.EnclosureInner

where

import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Basic
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Reduce
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Ring
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Bounds
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Eval
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Enclosure

import qualified Numeric.ER.Real.Base as B
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainIntBox, DomainBoxMappable)
import Numeric.ER.Real.Approx.Interval
import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Misc

import qualified Data.Map as Map

ienclThin ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    ERChebPoly box b ->
    ((ERChebPoly box b, ERChebPoly box b), Bool)
ienclThin p =
    ((chplNeg p, p), True)

ienclConst ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    b ->
    ((ERChebPoly box b, ERChebPoly box b), Bool)
ienclConst c =
    ((chplConst (-c), chplConst c), True)

--ienclBounds ix ((ln, h), isAC) =
--    (negate $ chplUpperBound ix ln, chplUpperBound ix h) 

ienclEval ((ln, h), isAC) pt =
    result
    where
    result = ERInterval lB hB
    lB = negate $ chplEvalDown ln pt
    hB = chplEvalDown h pt

enclEvalInner e pt = ienclEval (e, False) pt 

ienclRAEval (e@(ln, h), _) pt =
--    unsafePrintReturn
--    (
--        "ERChebPoly: ienclRAEval: "
--        ++ "\n lB = " ++ show lB
--        ++ "\n hB = " ++ show hB
--        ++ "\n result = "
--    )
    result 
    where
    result = ERInterval lAtPt hAtPt
    ERInterval _ lAtPt = negate $ chplRAEval (\b -> ERInterval b b) ln pt
    ERInterval hAtPt _ = chplRAEval (\b -> ERInterval b b) h pt
            
enclRAEvalInner e pt = ienclRAEval (e, False) pt

ienclAddErr errB ((pLowNeg, pHigh), isAC) =
    ((chplAddConstDown (- errB) pLowNeg, 
      chplAddConstDown (- errB) pHigh),
     isAC)


ienclRAConst ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    (ERInterval b) ->
    ((ERChebPoly box b, ERChebPoly box b), Bool)
ienclRAConst (ERInterval lo hi) = ((chplConst (-lo), chplConst hi), lo >= hi)

ienclReduceDegree maxDegree ((pLowNeg, pHigh), isAC) =
    ((chplReduceDegreeDown maxDegree pLowNeg, 
      chplReduceDegreeDown maxDegree pHigh),
     isAC)  
    
ienclReduceSize maxSize ((pLowNeg, pHigh), isAC) =
    ((chplReduceTermCountUp maxSize pLowNeg, 
      chplReduceTermCountUp maxSize pHigh),
     isAC)  
    
ienclAddConst c ((pLowNeg, pHigh),isAC) =
    ((chplAddConstDown (-c) pLowNeg, 
      chplAddConstDown c pHigh),
     isAC)

ienclNeg ((pLowNeg, pHigh), isAC) = ((pHigh, pLowNeg), isAC)

((p1LowNeg, p1High), isAC1) +:: ((p2LowNeg, p2High), isAC2) = 
    ((p1LowNeg +. p2LowNeg, p1High +. p2High), isAC1 && isAC2)
    
((p1LowNeg, p1High), isAC1) -:: ((p2LowNeg, p2High), isAC2) = 
    ((p1LowNeg +. p2High, p1High +. p2LowNeg), isAC1 && isAC2)

ienclAdd ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    ((ERChebPoly box b, ERChebPoly box b), Bool) -> 
    ((ERChebPoly box b, ERChebPoly box b), Bool) ->
    ((ERChebPoly box b, ERChebPoly box b), Bool)
ienclAdd maxDegr maxSize ie1 ie2 =
    ienclReduceSize maxSize $ ie1 +:: ie2
    
ienclMultiply ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    ((ERChebPoly box b, ERChebPoly box b), Bool) -> 
    ((ERChebPoly box b, ERChebPoly box b), Bool) ->
    ((ERChebPoly box b, ERChebPoly box b), Bool)
ienclMultiply maxDegr maxSize ie1@(e1@(ln1, h1), isAC1_prev) ie2@(e2@(ln2, h2), isAC2_prev) =
--    unsafePrintReturn
--    (
--        "ERChebPoly: ienclMultiply: "
--        ++ "\n ie1 = " ++ show ie1
--        ++ "\n ie2 = " ++ show ie2
--        ++ "\n isPos1 = " ++ show isPos1
--        ++ "\n isNeg1 = " ++ show isNeg1
--        ++ "\n isPos2 = " ++ show isPos2
--        ++ "\n isNeg2 = " ++ show isNeg2
--        ++ "\n result = "
--    )
    result
    where
    result = 
        ienclReduceSize maxSize $
        ienclReduceDegree maxDegr $
        (plainProduct, isAC1 && isAC2)
    plainProduct
        | isPos1 && isPos2 = multPosPos e1 e2  
        | isPos1 && isNeg2 = multPosNeg e1 e2  
        | isNeg1 && isNeg2 = multPosPos (enclNeg e1) (enclNeg e2)  
        | isNeg1 && isPos2 = multPosNeg e2 e1
        | isPos1 = multPosZer (e1, isC1, isAC1) e2
        | isNeg1 = multPosZer (enclNeg e1, isC1, isAC1) (enclNeg e2)
        | isPos2 = multPosZer (e2, isC2, isAC2) e1
        | isNeg2 = multPosZer (enclNeg e2, isC2, isAC2) (enclNeg e1)
        | otherwise = multZerZer (e1, isC1, isAC1) (e2, isC2, isAC2)
    isPos1 = chplUpperBound ix ln1 <= 0 && chplLowerBound ix h1 >= 0 
    isNeg1 = chplLowerBound ix ln1 >= 0 && chplUpperBound ix h1 <= 0
    isPos2 = chplUpperBound ix ln2 <= 0 && chplLowerBound ix h2 >= 0
    isNeg2 = chplLowerBound ix ln2 >= 0 && chplUpperBound ix h2 <= 0
    isAC1 = isAC1_prev || chplUpperBound ix (h1 +^ ln1) <= 0
    isAC2 = isAC2_prev || chplUpperBound ix (h2 +^ ln2) <= 0
    isC1 = chplLowerBound ix (h1 +. ln1) >= 0
    isC2 = chplLowerBound ix (h2 +. ln2) >= 0
    ix = 10

    multPosPos (ln1, h1) (ln2, h2) = 
        (chplNeg $ ln1 *^ ln2, h1 *. h2)
    multPosNeg (ln1, h1) (ln2, h2) = 
        (h1 *. ln2, (chplNeg ln1) *. h2)
    multPosZer ((ln1,h1), isC1, isAC1) (ln2, h2) = 
        multAux ((l1,h1), isC1, isAC1) ln2 h2 
        where
        l1 = chplNeg ln1
        
    multZerZer ((ln1, h1), isC1, isAC1) ((ln2, h2), isC2, isAC2) 
        | isC1 || isAC2 = multZZ12
        | isC2 || isAC1 = multZZ21
        | otherwise = isect multZZ12 multZZ21
        where
        multZZ12 
            | isC2 = union multZZ12L multZZ12R
            | otherwise = isect multZZ12L multZZ12R
        multZZ21 
            | isC1 = union multZZ21L multZZ21R
            | otherwise = isect multZZ21L multZZ21R
        multZZ12L = multAux ((l1,h1), isC1, isAC1) ln2 l2
        multZZ12R = multAux ((l1,h1), isC1, isAC1) hn2 h2
        multZZ21L = multAux ((l2,h2), isC2, isAC2) ln1 l1
        multZZ21R = multAux ((l2,h2), isC2, isAC2) hn1 h1
        l1 = chplNeg ln1
        l2 = chplNeg ln2
        hn1 = chplNeg h1
        hn2 = chplNeg h2
        
    isect (ln1, h1) (ln2, h2) = (minP ln1 ln2, minP h1 h2) 
    union (ln1, h1) (ln2, h2) = (maxP ln1 ln2, maxP h1 h2)
    minP = chplMinDn maxDegr maxSize 
    maxP = chplMaxDn maxDegr maxSize 
    
    multAux ((l,h), isC, isAC) an b 
        | isC =
            (
             maxP (an *. h) (an *. l)
            ,
             maxP (b *. h) (b *. l)
            )
        | isAC =
            (
             minP (an *. h) (an *. l)
            ,
             minP (b *. h) (b *. l)
            )
        | otherwise = -- enclosure could be a mix of consistent and inconsistent 
            (
             ((nonnegP an) *. h) 
             +. 
             ((nonposP an) *. l)
             -- ie: if (l <= h) then max(an*h, an*l) else min(an*h, an*l)
            ,
             ((nonnegP b) *. h) 
             +. 
             ((nonposP b) *. l)
             -- ie: if (l <= h) then max(b*h, b*l) else min(b*h, b*l)
            )
    
    nonposP = chplNonposDown maxDegr maxSize
    nonnegP = chplNonnegDown maxDegr maxSize
    

ienclSquare ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    ((ERChebPoly box b, ERChebPoly box b), Bool) ->
    ((ERChebPoly box b, ERChebPoly box b), Bool)
ienclSquare maxDegr maxSize ie@((ln, h), isAC) =
    ienclMultiply maxDegr maxSize ie ie 

--    {-
--        formula:
--            (ln, h)^2 =
--                ( minUp( 0, maxUp( - ln *. ln, - h *. h)), maxUp(ln *^ ln, h *^ h) )
--    -}
----    | minZeroHelps = 
--    = (minZeroMaxNegSq, maxSq)
----    | otherwise =
----        (maxNegSq, maxSq)
--    where
--    maxSq = maxP ln2Up h2Up
--    maxNegSq = maxP (chplNeg ln2Down) (chplNeg h2Down)
--    minZeroMaxNegSq = chplNonposUp maxDegr maxSize maxNegSq 
----    minZeroHelps =
----        (maxNegSqUpperB > 0) && (minZeroMaxNegSqUpperB / maxNegSqUpperB < 1/2)
----    maxNegSqUpperB = chplUpperBound 10 maxNegSq
----    minZeroMaxNegSqUpperB = chplUpperBound 10 minZeroMaxNegSq
--     
--    (ln2Down, ln2Up, _) = chplMultiply ln ln
--    (h2Down, h2Up, _) = chplMultiply h h
--    
----    reduceDegrSize = reduceSize maxSize . reduceDegree maxDegr
--    maxP = chplMaxUp maxDegr maxSize

{-| 
    Multiply an enclosure by a scalar 
    assuming the enclosure is non-negative on the whole unit domain.
-} 
ienclScaleNonneg ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    b {-^ ratio to scale by -} -> 
    ((ERChebPoly box b, ERChebPoly box b), Bool) -> 
    ((ERChebPoly box b, ERChebPoly box b), Bool)
ienclScaleNonneg ratio pEncl@((ln, h), isAC) =
    ((ln *. pRatio, h *. pRatio), isAC)
    where
    pRatio = chplConst ratio

{-| 
    Multiply an enclosure by a scalar.
-} 
ienclScale ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) =>
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} ->
    b {-^ ratio to scale by -} -> 
    ((ERChebPoly box b, ERChebPoly box b), Bool) -> 
    ((ERChebPoly box b, ERChebPoly box b), Bool)
ienclScale maxDegree maxSize ratio pEncl =
    ienclMultiply maxDegree maxSize pEncl (ienclConst ratio) 

--enclRAScale ::
--    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
--    Int {-^ maximum polynomial degree -} -> 
--    Int {-^ maximum term count -} ->
--    (ERInterval b) -> 
--    (ERChebPoly box b, ERChebPoly box b) ->
--    (ERChebPoly box b, ERChebPoly box b)
--enclRAScale maxDegree maxSize ra pEncl =
--    enclMultiply maxDegree maxSize pEncl (enclRAConst ra) 

{-|
    Evaluate the Chebyshev polynomials of the first kind
    applied to a given polynomial, yielding a list of polynomial enclosures. 
-}
ienclEvalTs ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) =>
    Int {-^ max degree for result -} -> 
    Int {-^ max approx size for result -} ->
    ((ERChebPoly box b, ERChebPoly box b), Bool) {-^ bounds of a polynomial enclosure to evaluate -} ->
    [((ERChebPoly box b, ERChebPoly box b), Bool)]
ienclEvalTs maxDegree maxSize p1@(pLowNeg, pHigh) =
    chebyIterate (ienclConst 1) p1
    where
    chebyIterate pNm2 pNm1 =
        pNm2 : (chebyIterate pNm1 pN)
        where
        pN = 
            (ienclScale maxDegree maxSize 2 $ 
                ienclMultiply maxDegree maxSize p1 pNm1) 
            -:: pNm2

--{-|
--    Multiply a polynomial by an enclosure using min/max
---}
--enclThinTimes ::
--    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
--    Int {-^ maximum polynomial degree -} -> 
--    Int {-^ maximum term count -} -> 
--    ERChebPoly box b ->
--    (ERChebPoly box b, ERChebPoly box b) ->
--    (ERChebPoly box b, ERChebPoly box b)
--enclThinTimes maxDegree maxSize p1 (p2LowNeg, p2High) =
--    (prodLowNeg, prodHigh)
--    where
--    prodHigh =
--        chplMaxUp maxDegree maxSize
--            (p1 *^ p2High)
--            (p1n *^ p2LowNeg) -- beware: p1 can cross zero
--    prodLowNeg =
--        chplMaxUp maxDegree maxSize
--            (p1n *^ p2High)
--            (p1 *^ p2LowNeg)
--    p1n = chplNeg p1
--
--
