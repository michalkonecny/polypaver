{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Compose
    Description :  (internal) composition of polynomials
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Internal module for "Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom".
    
    Implementation of pointwise consistently rounded polynomial composition.
-}
module Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.ComposeInner
where

import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Basic
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Ring
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Bounds
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Enclosure
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.EnclosureInner

import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Base as B
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainIntBox, DomainBoxMappable)

import Numeric.ER.Real.Approx.Interval
import Numeric.ER.Misc

import qualified Data.Map as Map

{-|
    Compose a polynomial and an enclosure, producing a correcly rounded enclosure,
    assuming the second polynomial maps [-1,1] into [-1,1].
-}
ienclCompose ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ max degree for result -} -> 
    Int {-^ max approx size for result -} ->
    ERChebPoly box b {-^ @f@ -} ->
    varid {-^ variable @v@ to substitute in @f@ -} -> 
    ((ERChebPoly box b, ERChebPoly box b), Bool)
         {-^ enclosure of a function @f_v@ to substitute for @v@ 
             that maps @[-1,1]@ into @[-1,1]@  -} ->
    ((ERChebPoly box b, ERChebPoly box b), Bool)
        {-^ lower bound and upper bound -}


ienclCompose maxDegree maxSize p@(ERChebPoly coeffs) substVar substEncl =
    result
{------------------------------
 The algorithm: separate from the polynomial 
 all terms for each degree of the substituted variable,
 giving rise to a number of polynomials.
 These polynomials are then used as coefficients multiplying
 the enclosure evaluations of the Chebyshev polynomials 
 over the substituted enclosure.
-------------------------------}
    where
    result =
        Map.fold (+::) (ienclConst 0) $ Map.mapWithKey evalDegree degreePolynomialMap
    degreePolynomialMap =
        Map.foldWithKey extractTerm Map.empty coeffs
    extractTerm term c prevPolynomMap =
        Map.insertWith Map.union substVarDegree (Map.singleton termNoSubstVar c) prevPolynomMap
        where
        substVarDegree = DBox.findWithDefault 0 substVar term
        termNoSubstVar = DBox.delete substVar term
    evalDegree degree degreeCoeffs =
        ienclMultiply maxDegree maxSize (substPolyDegrees !! degree) ((chplNeg degreePoly, degreePoly), True)
        where
        degreePoly = ERChebPoly degreeCoeffs
    substPolyDegrees =
        ienclEvalTs maxSize maxDegree substEncl

{------------------------------
 The following algorithm is quite wasteful when the polynomial
 contains other variables besides the one being substituted.
-------------------------------}
--chplComposeWithEncl maxDegree maxSize p@(ERChebPoly coeffs) substVar substEncl =
--    result
--    where
--    result =
--        foldl (+:) (enclConst 0) $ map evalTerm $ Map.toList coeffs
--    evalTerm (term, c) =
--        enclScale c $ 
--            foldl (enclMultiply maxDegree maxSize) (enclConst 1) $ 
--                map evalVar $ DBox.toList term
--    evalVar (var, degree) =
--        case var == substVar of
--            True ->
--                substPolyDegrees !! degree
--            False ->
--                (chplNeg varPoly, varPoly)
--        where
--        varPoly = 
--            ERChebPoly $ Map.singleton (DBox.singleton var degree) 1
--    substPolyDegrees =
--        enclEvalTs maxSize maxDegree substEncl

        

--{-|
--    Compose two polynomials, rounding upwards
--    provided the second polynomial maps [-1,1] into [-1,1].
---}
--enclComposeMany ::
--    (B.ERRealBase b, 
--     DomainBox box varid Int, Ord box, Show varid,
--     DomainIntBox boxra varid (ERInterval b),
--     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
--    Int {-^ max degree for result -} -> 
--    Int {-^ max approx size for result -} ->
--    ERChebPoly box b ->
--    Map.Map varid (ERChebPoly box b, ERChebPoly box b) 
--     {-^ variables to substitute and the enclosures to substitute for each of them respectively  -} ->
--    (ERChebPoly box b, ERChebPoly box b)
--        {-^ lower bound (negated) and upper bound -}
--enclComposeMany maxDegree maxSize p@(ERChebPoly coeffs) substitutions =
----    unsafePrintReturn
----    (
----        "ChebyshevBase.Polynom.Compose: enclComposeMany:"
----        ++ "\n maxDegree = " ++ show maxDegree        
----        ++ "\n maxSize = " ++ show maxSize        
----        ++ "\n p = " ++ show p
----        ++ "\n substitutions = " ++ show substitutions
----        ++ "\n terms... \n" ++ (unlines $ map (show . (\t -> map evalVar (DBox.toList t) ) . fst) $ Map.toList coeffs)  
----        ++ "\n result = "        
----    )
--    result
--    where
--    result =
--        foldl (+:) (enclConst 0) $ map evalTerm $ Map.toList coeffs
--    evalTerm (term, c) =
--        enclScale maxDegree maxSize c $ 
--            foldl (enclMultiply maxDegree maxSize) (enclConst 1) $ 
--                map evalVar $ DBox.toList term
--    evalVar (varID, degree) =
--        case Map.lookup varID substDegrees of
--            Nothing ->
--                (chplNeg varPoly, varPoly)
--            Just pvDegrees ->
--                pvDegrees !! degree
--        where
--        varPoly = 
--            ERChebPoly $ Map.singleton (DBox.singleton varID degree) 1
--    substDegrees =
--        Map.map mkPVDegrees substitutions
--    mkPVDegrees pvEncl =
--        enclEvalTs maxDegree maxSize pvEncl
        