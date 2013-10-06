{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Ring
    Description :  (internal) uniformly roudned pointwise ring operations  
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Internal module for "Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom".
    
    Implementation of addition and multiplication over polynomials 
    with pointwise rounding uniform over the whole unit domain.
-}
module Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Ring

where

import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Basic

import qualified Numeric.ER.Real.Base as B
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainIntBox)
import Numeric.ER.Misc

import qualified Data.Map as Map

{-|
    Negate a polynomial exactly.
-}
chplNeg (ERChebPoly coeffs) =
    ERChebPoly $ Map.map negate coeffs

chplBall2DownUp ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
    (ERChebPoly box b, b) -> 
    (ERChebPoly box b, ERChebPoly box b)
chplBall2DownUp ball =
    (down, up)
    where
    (down, up, _) = chplBall2DownUpWd ball

chplBall2DownUpWd ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
    (ERChebPoly box b, b) -> 
    (ERChebPoly box b, ERChebPoly box b, b)
chplBall2DownUpWd (ERChebPoly coeffsCentre, radius) =
    (ERChebPoly coeffsDown, ERChebPoly coeffsUp, 2 * radius)
    where
    coeffsDown = 
        Map.insertWith plusDown chplConstTermKey (- radius) coeffsCentre
    coeffsUp = 
        Map.insertWith plusUp chplConstTermKey radius coeffsCentre

chplBall2Down ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
    (ERChebPoly box b, b) -> 
    (ERChebPoly box b)
chplBall2Down = fst . chplBall2DownUp

chplBall2Up ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
    (ERChebPoly box b, b) -> 
    (ERChebPoly box b)
chplBall2Up = snd . chplBall2DownUp

{-|
    Add a constant to a polynomial, rounding downwards and upwards. 
-}
ballAddConst ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
    b -> 
    (ERChebPoly box b) -> 
    (ERChebPoly box b, b)
ballAddConst c (ERChebPoly coeffs) =
    (ERChebPoly sumCoeffs, err)
    where
    sumCoeffs =
        Map.insert chplConstTermKey newConstUp coeffs
    oldConst =
        case Map.lookup chplConstTermKey coeffs of
            Just c -> c
            Nothing -> 0
    newConstUp = oldConst `plusUp` c
    newConstDown = oldConst `plusDown` c
    err = newConstUp - newConstDown    

chplAddConstUp c p = chplBall2Up $ ballAddConst c p
chplAddConstDown c p = chplBall2Down $ ballAddConst c p

{-|
    Add two polynomials, rounding downwards and upwards. 
-}
ballAdd ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
    (ERChebPoly box b) -> 
    (ERChebPoly box b) -> 
    (ERChebPoly box b, b)
ballAdd (ERChebPoly coeffs1) (ERChebPoly coeffs2) =
    (ERChebPoly coeffsUp, maxError)
    where
    coeffsUp =
        (Map.unionWith plusUp coeffs1 coeffs2)
        -- point-wise sum of polynomials with coeffs rounded upwards
    coeffsDown =
        (Map.unionWith plusDown coeffs1 coeffs2)
        -- point-wise sum of polynomials with coeffs rounded upwards
    maxError =
        Map.fold plusUp 0 $ 
            Map.intersectionWith (-) coeffsUp coeffsDown
        -- addition must round upwards on interval [-1,1]
                -- non-constant terms are multiplied by quantities in [-1,1] 
                -- and thus can make the result drop below the exact result
                -- -> to compensate add the rounding difference to the constant term 

p1 +^ p2 = chplBall2Up $ ballAdd p1 p2
p1 +. p2 = chplBall2Down $ ballAdd p1 p2
p1 -^ p2 = chplBall2Up $ ballAdd p1 (chplNeg p2)
p1 -. p2 = chplBall2Down $ ballAdd p1 (chplNeg p2)

{-|
    Multiply two polynomials, rounding downwards and upwards. 
-}
ballMultiply ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
    ERChebPoly box b -> 
    ERChebPoly box b -> 
    (ERChebPoly box b, b) 
        {-^ lower and upper bounds on the product and an upper bound on their difference -}
ballMultiply p1@(ERChebPoly coeffs1) p2@(ERChebPoly coeffs2) =
    case (chplGetConst p1, chplGetConst p2) of
        (Just c1, _) -> ballScale c1 p2
        (_, Just c2) -> ballScale c2 p1
        _ ->    
            (ERChebPoly directProdCoeffsUp, roundOffCompensation)
    where
    roundOffCompensation =
        Map.fold plusUp 0 $
            Map.unionWith plusUp directProdCoeffsUp directProdCoeffsDownNeg
    (directProdCoeffsUp, directProdCoeffsDownNeg) =
        foldl addCombiCoeff (Map.empty, Map.empty) combinedCoeffs
        where
        addCombiCoeff
                (prevCoeffsUp, prevCoeffsDownNeg) 
                (coeffUp, coeffDownNeg, (powersList, coeffCount)) =
            foldl addOnce (prevCoeffsUp, prevCoeffsDownNeg) powersList
            where
            addOnce (prevCoeffsUp, prevCoeffsDownNeg) powers =
                (Map.insertWith plusUp powers coeffUpFrac prevCoeffsUp, 
                 Map.insertWith plusUp powers coeffDownNegFrac prevCoeffsDownNeg)
            coeffUpFrac = coeffUp / coeffCountB
            coeffDownNegFrac = coeffDownNeg / coeffCountB
            coeffCountB = fromInteger coeffCount
    combinedCoeffs =
        [   -- (list of triples)
            (
                (c1 * c2) -- upwards rounded product
            ,
                ((- c1) * c2) -- downwards rounded negated product
            ,
                combinePowers powers1 powers2
            )
        |
            (powers1, c1) <- coeffs1List,
            (powers2, c2) <- coeffs2List
        ]
    combinePowers powers1 powers2 =
        (combinedPowers, 2 ^ (length sumsDiffs)) 
        where
        combinedPowers =
            map (DBox.fromAscList . (filter $ \ (k,v) -> v > 0)) $
                allPairsCombinations $ 
                    sumsDiffs
        sumsDiffs = 
            -- associative list with the sum and difference of powers for each variable
            zipWith (\(k,s) (_,d) -> (k,(s,d)))
                (DBox.toAscList $ DBox.unionWith (\a b -> (a + b)) powers1 powers2)
                (DBox.toAscList $ DBox.unionWith (\a b -> abs (a - b)) powers1 powers2)
    coeffs1List =
        Map.toList coeffs1
    coeffs2List =
        Map.toList coeffs2

p1 *^ p2 = chplBall2Up $ ballMultiply p1 p2
p1 *. p2 = chplBall2Down $ ballMultiply p1 p2

{-| Multiply a polynomial by a scalar rounding downwards and upwards. -} 
ballScale ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    b -> 
    (ERChebPoly box b) -> 
    (ERChebPoly box b, b)
        {-^ lower and upper bounds on the product and an upper bound on their difference -}
ballScale ratio p@(ERChebPoly coeffs) =
    case chplGetConst p of
        Just c -> 
            (chplConst cScaledDown, cScaledUp - cScaledDown)
            where
            cScaledUp = ratio `timesUp` c
            cScaledDown = ratio `timesDown` c
        _ ->
            (ERChebPoly coeffsScaled, errBound)
    where
    (errBound, coeffsScaled) =
        Map.mapAccum processTerm 0 coeffs
    processTerm errBoundPrev coeff =
        (errBoundPrev + errBoundHere, coeffScaledUp)
        where
        errBoundHere = coeffScaledUp - coeffScaledDown
        coeffScaledDown = ratio `timesDown` coeff
        coeffScaledUp = ratio `timesUp` coeff    

chplScaleDown r p = chplBall2Down $ ballScale r p
chplScaleUp r p = chplBall2Up $ ballScale r p

{-|
    Multiply a polynomial by itself, rounding downwards and upwards.
-}
ballSquare ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
    ERChebPoly box b ->
    (ERChebPoly box b, b)
ballSquare p = ballMultiply p p
