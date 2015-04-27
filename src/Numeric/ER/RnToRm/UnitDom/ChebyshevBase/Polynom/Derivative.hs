{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Derivative
    Description :  (internal) derivative of polynomials  
    Copyright   :  (c) 2009 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Internal module for "Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom".
    
    Implementation of safely rounded derivative of polynomials.
-}
module Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Derivative where

import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Basic
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Ring

import qualified Numeric.ER.Real.Base as B
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainBoxMappable, DomainIntBox)
import Numeric.ER.Misc

import qualified Data.Map as Map

{-|
    Differentiate a polynomial using one of its variables. 
-}
chplDifferentiate ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
    ERChebPoly box b ->
    varid {-^ variable to differentiate over -} ->
    (ERChebPoly box b, ERChebPoly box b)
chplDifferentiate p diffVar = chplBall2DownUp $ ballDifferentiate p diffVar 

{-|
    Differentiate a polynomial using one of its variables. 
-}
ballDifferentiate ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
    ERChebPoly box b ->
    varid {-^ variable to differentiate over -} ->
    (ERChebPoly box b, b)
ballDifferentiate (ERChebPoly coeffs) diffVar =
    (ERChebPoly diffCoeffs, diffRadius)
    where
    (diffCoeffs, diffRadius) =
        -- ((term |-> coeff), radius)
        Map.foldWithKey extractTerm (Map.empty, 0) coeffs
    extractTerm term c prevBall =
        addDiffTerms (diffVarDegree - 1) prevBall
        where
        diffVarDegree = DBox.findWithDefault 0 diffVar term
        cConstUp = c * (B.fromIntegerUp $ toInteger diffVarDegree)
        cConstDown = c `timesDown` (B.fromIntegerDown $ toInteger diffVarDegree)
        cConstErr = cConstUp - cConstDown
        cNonconstUp = 2 * cConstUp
        cNonconstDown = 2 `timesDown` cConstDown
        cNonconstErr = cNonconstUp - cNonconstDown
        addDiffTerms degreeToAdd ball@(coeffs, radius)
            | degreeToAdd < 0 = ball
            | degreeToAdd == 0 =
                addTermWithDegree (0 :: Int) cConstDown cConstErr
            | otherwise =
                addDiffTerms (degreeToAdd - 2) $
                    addTermWithDegree degreeToAdd cNonconstUp cNonconstErr
            where
            addTermWithDegree diffVarDegree c cErr =
                (newCoeffs, radius + cErr + newCoeffErr)
                where
                newCoeffs = Map.insert newTerm newCoeffDown coeffs 
                newCoeffUp = oldCoeff + c
                newCoeffDown = oldCoeff `plusDown` c
                newCoeffErr = newCoeffUp - newCoeffDown
                oldCoeff = 
                    case Map.lookup newTerm coeffs of
                        Nothing -> 0
                        Just c -> c
                newTerm = DBox.insert diffVar degreeToAdd term  
            addRadius (p,r) = (p, r + radius) 
--             
--                
--        (centrePolyWithDiffTerm, prevRadius + )
--        Map.insertWith Map.union substVarDegree (Map.singleton termNoSubstVar c) prevPolynomMap
    

    
    