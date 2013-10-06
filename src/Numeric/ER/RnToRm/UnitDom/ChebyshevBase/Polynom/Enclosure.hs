{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Enclosure
    Description :  (internal) basic operations for primitive polynomial enclosures  
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Internal module for "Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom".
    
    Implementation of selected operations working on pairs
    of polynomials understood as function enclosures.
    These are needed to define composition and some elementary operations.
-}
module Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Enclosure

where

import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Basic
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Reduce
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Ring
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Bounds
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Eval

import qualified Numeric.ER.Real.Base as B
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainIntBox, DomainBoxMappable)
import Numeric.ER.Real.Approx.Interval
import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Misc

import qualified Data.Map as Map

enclThin ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    ERChebPoly box b ->
    (ERChebPoly box b, ERChebPoly box b)
enclThin p =
    (chplNeg p, p)

enclConst ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    b ->
    (ERChebPoly box b, ERChebPoly box b)
enclConst c =
    (chplConst (-c), chplConst c)

enclBounds ix (ln, h) =
    (min lLower hLower, max lUpper hUpper)
    where
    (lLower, lUpper) = chplBounds ix $ chplNeg ln
    (hLower, hUpper) = chplBounds ix h

enclBoundsExpensive ix (ln, h) =
    (negate $ chplUpperBoundExpensive ix ln, chplUpperBoundExpensive ix h)

enclEval e@(ln, h) pt 
--    | lB > hB =
--        unsafePrintReturn
--        (
--            "ERChebPoly: enclEval: inverted result:"
--            ++ "\n h = " ++ show h 
--            ++ "\n ln = " ++ show ln 
--            ++ "\n result = "
--        )
--        result
--    | otherwise = result
    = result
    where
    result = ERInterval lB hB
    lB = negate $ chplEvalUp ln pt
    hB = chplEvalUp h pt

enclRAEval e@(ln, h) pt =
    result 
    where
    result = ERInterval lAtPt hAtPt
    ERInterval lAtPt _ = negate $ chplRAEval (\b -> ERInterval b b) ln pt
    ERInterval _ hAtPt = chplRAEval (\b -> ERInterval b b) h pt

enclAddErr errB (pLowNeg, pHigh) =
    (chplAddConstUp errB pLowNeg, chplAddConstUp errB pHigh)


enclRAConst ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    (ERInterval b) ->
    (ERChebPoly box b, ERChebPoly box b)
enclRAConst (ERInterval lo hi) = (chplConst (-lo), chplConst hi)

enclReduceDegree maxDegree (pLowNeg, pHigh) =
    (chplReduceDegreeUp maxDegree pLowNeg, chplReduceDegreeUp maxDegree pHigh)  
    
enclReduceSize maxSize (pLowNeg, pHigh) =
    (chplReduceTermCountUp maxSize pLowNeg, chplReduceTermCountUp maxSize pHigh)  
    
enclAddConst c (pLowNeg, pHigh) =
    (chplAddConstUp (-c) pLowNeg, chplAddConstUp c pHigh)

enclNeg (pLowNeg, pHigh) = (pHigh, pLowNeg)

(p1LowNeg, p1High) +: (p2LowNeg, p2High) = 
    (p1LowNeg +^ p2LowNeg, p1High +^ p2High)
    
(p1LowNeg, p1High) -: (p2LowNeg, p2High) =
    (p1LowNeg +^ p2High, p1High +^ p2LowNeg)

enclAdd ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    (ERChebPoly box b, ERChebPoly box b) -> 
    (ERChebPoly box b, ERChebPoly box b) ->
    (ERChebPoly box b, ERChebPoly box b)
enclAdd maxDegr maxSize (p1LowNeg, p1High) (p2LowNeg, p2High) =
    enclReduceSize maxSize $
    (p1LowNeg +^ p2LowNeg, p1High +^ p2High)
    
enclMultiply ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    (ERChebPoly box b, ERChebPoly box b) -> 
    (ERChebPoly box b, ERChebPoly box b) ->
    (ERChebPoly box b, ERChebPoly box b)
enclMultiply maxDegr maxSize (ln1, h1) (ln2, h2) =
    enclReduceSize maxSize $
    enclReduceDegree maxDegr $
    case (ln1UpperBound <= 0, h1UpperBound <= 0, ln2UpperBound <= 0, h2UpperBound <= 0) of
        (True, _, True, _) -> -- both non-negative
--            unsafePrint "both non-negative" $
            (l1l2Neg, h1h2)
        (_, True, _, True) -> -- both non-positive
--            unsafePrint "both non-positive" $
            (h1h2Neg, l1l2)
        (True, _, _, True) -> -- first non-negative, second non-positive
--            unsafePrint "first non-negative, second non-positive" $
            (h1l2Neg, l1h2)
        (_, True, True, _) -> -- first non-positive, second non-negative
--            unsafePrint
--                ("ERChebPoly: enclMultiply: first non-positive, second non-negative:"
--                 ++ "\n l1 = " ++ show (chplNeg ln1)
--                 ++ "\n h1 = " ++ show h1
--                 ++ "\n l2 = " ++ show (chplNeg ln2)
--                 ++ "\n h2 = " ++ show h2
--                ) $
            (l1h2Neg, h1l2)
        _ -> -- one of both may be crossing zero
            (
             (h1h2Neg `maxP` l1l2Neg) `maxP` (h1l2Neg `maxP` l1h2Neg)
            ,
             (h1h2 `maxP` l1l2) `maxP` (h1l2 `maxP` l1h2)
            )
        where
        ln1UpperBound = chplUpperBound ix ln1
        ln2UpperBound = chplUpperBound ix ln2
        h1UpperBound = chplUpperBound ix h1
        h2UpperBound = chplUpperBound ix h2
        ix = 10
        maxP = chplMaxUp maxDegr maxSize
        
        h1h2 = h1 *^ h2
        h1h2Neg = (chplNeg h1) *^ h2
        l1l2 = ln1 *^ ln2
        l1l2Neg = (chplNeg ln1) *^ ln2
        h1l2 = h1 *^ (chplNeg ln2)
        h1l2Neg = h1 *^ ln2
        l1h2 = (chplNeg ln1) *^ h2
        l1h2Neg = ln1 *^ h2


enclSquare ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    (ERChebPoly box b, ERChebPoly box b) ->
    (ERChebPoly box b, ERChebPoly box b)
enclSquare maxDegr maxSize (ln, h)
    {-
        formula:
            (ln, h)^2 =
                ( minUp( 0, maxUp( - ln *. ln, - h *. h)), maxUp(ln *^ ln, h *^ h) )
    -}
--    | minZeroHelps = 
    = (minZeroMaxNegSq, maxSq)
--    | otherwise =
--        (maxNegSq, maxSq)
    where
    maxSq = maxP ln2Up h2Up
    maxNegSq = maxP (chplNeg ln2Down) (chplNeg h2Down)
    minZeroMaxNegSq = chplNonposUp maxDegr maxSize maxNegSq 
--    minZeroHelps =
--        (maxNegSqUpperB > 0) && (minZeroMaxNegSqUpperB / maxNegSqUpperB < 1/2)
--    maxNegSqUpperB = chplUpperBound 10 maxNegSq
--    minZeroMaxNegSqUpperB = chplUpperBound 10 minZeroMaxNegSq
     
    (ln2Down, ln2Up) = chplBall2DownUp $ ballMultiply ln ln
    (h2Down, h2Up) = chplBall2DownUp $ ballMultiply h h
    
--    reduceDegrSize = reduceSize maxSize . reduceDegree maxDegr
    maxP = chplMaxUp maxDegr maxSize
    
    

    
{-| 
    Multiply an enclosure by a scalar 
    assuming the enclosure is non-negative on the whole unit domain.
-} 
enclScaleNonneg ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    b {-^ ratio to scale by -} -> 
    (ERChebPoly box b, ERChebPoly box b) -> 
    (ERChebPoly box b, ERChebPoly box b)
enclScaleNonneg ratio pEncl@(ln, h) =
    (ln *^ pRatio, h *^ pRatio)
    where
    pRatio = chplConst ratio

{-| 
    Multiply an enclosure by a scalar.
-} 
enclScale ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} ->
    b {-^ ratio to scale by -} -> 
    (ERChebPoly box b, ERChebPoly box b) -> 
    (ERChebPoly box b, ERChebPoly box b)
enclScale maxDegree maxSize ratio pEncl =
    enclMultiply maxDegree maxSize pEncl (enclConst ratio) 

enclRAScale ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} ->
    (ERInterval b) -> 
    (ERChebPoly box b, ERChebPoly box b) ->
    (ERChebPoly box b, ERChebPoly box b)
enclRAScale maxDegree maxSize ra pEncl =
    enclMultiply maxDegree maxSize pEncl (enclRAConst ra) 

{-|
    Multiply a polynomial by a scalar interval, returning an enclosure.
-} 
chplScaleRA :: 
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    ERInterval b {-^ lower and upper bounds on the ratio to scale by -} -> 
    ERChebPoly box b ->
    (ERChebPoly box b, ERChebPoly box b)
chplScaleRA maxDegr maxSize ratio@(ERInterval ratioDown ratioUp) p =
    (scaledPDownNeg, scaledPUp)
    where
    (scaledPDownNeg, scaledPUp) =
        enclMultiply maxDegr maxSize 
            (chplNeg p, p) (chplConst (- ratioDown), chplConst ratioUp)

chplScaleRADown m n r = chplNeg . fst . chplScaleRA m n r
chplScaleRAUp m n r = snd . chplScaleRA m n r

{-|
    Evaluate the Chebyshev polynomials of the first kind
    applied to a given polynomial, yielding a list of polynomial enclosures. 
-}
enclEvalTs ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ max degree for result -} -> 
    Int {-^ max approx size for result -} ->
    (ERChebPoly box b, ERChebPoly box b) {-^ bounds of a polynomial enclosure to evaluate -} ->
    [(ERChebPoly box b, ERChebPoly box b)]
enclEvalTs maxDegree maxSize p1@(pLowNeg, pHigh) =
    chebyIterate (enclConst 1) p1
    where
    chebyIterate pNm2 pNm1 =
        pNm2 : (chebyIterate pNm1 pN)
        where
        pN = 
            (enclScale maxDegree maxSize 2 $ 
                enclMultiply maxDegree maxSize p1 pNm1) 
            -: pNm2

{-|
    Multiply a polynomial by an enclosure using min/max
-}
enclThinTimes ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    ERChebPoly box b ->
    (ERChebPoly box b, ERChebPoly box b) ->
    (ERChebPoly box b, ERChebPoly box b)
enclThinTimes maxDegree maxSize p1 (p2LowNeg, p2High) =
    (prodLowNeg, prodHigh)
    where
    prodHigh =
        chplMaxUp maxDegree maxSize
            (p1 *^ p2High)
            (p1n *^ p2LowNeg) -- beware: p1 can cross zero
    prodLowNeg =
        chplMaxUp maxDegree maxSize
            (p1n *^ p2High)
            (p1 *^ p2LowNeg)
    p1n = chplNeg p1


