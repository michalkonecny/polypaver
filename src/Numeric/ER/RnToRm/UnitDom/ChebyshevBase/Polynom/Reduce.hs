{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Reduce
    Description :  (internal) uniformly roudned polynomial size reductions  
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Internal module for "Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom".
    
    Implementation of field arithmetic over polynomials 
    with pointwise rounding uniform over the whole unit domain.
-}

module Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Reduce 

where

import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Basic

import qualified Numeric.ER.Real.Base as B
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainIntBox)
import Numeric.ER.Misc

import qualified Data.List as List
import qualified Data.Map as Map

chplReduceTermCount ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) =>
    Int -> 
    ERChebPoly box b -> 
    (ERChebPoly box b, ERChebPoly box b)
chplReduceTermCount maxTermCount p@(ERChebPoly coeffs) 
    | currentCount <= maxTermCount = (p,p)
    | otherwise =
        (ERChebPoly lessCoeffsDown, ERChebPoly lessCoeffsUp)
    where
    currentCount = chplCountTerms p
    lessCoeffsDown =
        Map.insertWith plusDown chplConstTermKey (- err) lessCoeffs
    lessCoeffsUp =
        Map.insertWith plusUp chplConstTermKey err lessCoeffs
    err = 
        sum $ map fst smallCoeffTerms
    lessCoeffs =
        Map.fromList $ map snd $ largeCoeffTerms
    (smallCoeffTerms, largeCoeffTerms) = 
                splitAt (Map.size coeffs - maxTermCount) $
                    List.sort $ 
                        map (\(t,c)->(abs c, (t,c))) $ Map.toList coeffs

chplReduceTermCountDown m = fst . chplReduceTermCount m
chplReduceTermCountUp m = snd . chplReduceTermCount m


{-|
    Convert a polynomial to a lower-order one that is dominated by (resp. dominates)
    it closely on the domain [-1,1].
-}
chplReduceDegree ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box) => 
    Int {-^ new maximal order -} ->
    ERChebPoly box b -> 
    (ERChebPoly box b, ERChebPoly box b) {-^ lower and upper bounds with limited degree -}
chplReduceDegree maxOrder (ERChebPoly coeffs) =
    (ERChebPoly newCoeffsDown, ERChebPoly newCoeffsUp)
    where
    newCoeffsUp =
        Map.insertWith plusUp chplConstTermKey highOrderCompensation coeffsLowOrder
    newCoeffsDown =
        Map.insertWith plusDown chplConstTermKey (-highOrderCompensation) coeffsLowOrder
    highOrderCompensation =
        Map.fold (\ new prev -> prev + (abs new)) 0 coeffsHighOrder
    (coeffsHighOrder, coeffsLowOrder) =        
        Map.partitionWithKey (\ k v -> chplTermOrder k > maxOrder) coeffs

chplReduceDegreeDown m = fst . chplReduceDegree m
chplReduceDegreeUp m = snd . chplReduceDegree m


