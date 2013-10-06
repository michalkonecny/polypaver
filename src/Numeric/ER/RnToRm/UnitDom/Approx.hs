{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.Approx
    Description :  class abstracting function enclosures on @[-1,1]^n@
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Approximation of continuous real functions 
    defined on the unit rectangle domain of a certain dimension.
    
    To be imported qualified, usually with the synonym UFA.    
-}
module Numeric.ER.RnToRm.UnitDom.Approx
(
    ERUnitFnApprox(..),
    keyPointsConsistencyCheck,
    keyPointsPointwiseConsistencyCheck
)
where

import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.RnToRm.Approx as FA
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainIntBox)
import Numeric.ER.BasicTypes

import Numeric.ER.Misc

import qualified Data.Map as Map

{-|
    This class extends 'ERFnApprox' by:
    
    * assuming that the domain of the function enclosures is always @[-1,1]^n@ for some @n@;
    
    * allowing the construction of basic function enclosures
      where the domain has to be known.
-}

class (FA.ERFnApprox box varid domra ranra fa) => 
    ERUnitFnApprox box varid domra ranra fa
    | fa -> box varid domra ranra
    where
    {-| 
        A function enclosure with no information about the function's values.
    -}
    bottomApprox :: fa
    {-|
        Construct a constant enclosure for a tuple of functions.
    -}
    const :: [ranra] -> fa
    {-| 
        Construct the exact enclosure of an affine function on @[-1,1]^n@. 
    -} 
    affine :: 
        [ranra] {-^ values at 0 -} ->
        Map.Map varid ([ranra]) {-^ ascents of each base vector -} -> 
        fa
    {-|
        Return the coefficients of an affine function @a_0 + a_1x_1 + a_2x_2 + ... + a_nx_n@
        that majors the given function over its entire domain. 
    -}
    getAffineUpperBound ::
        fa ->
        ([ranra], Map.Map varid ([ranra]))
    {-|
        A simple and limited composition of functions.
        
        It is primarily intended to be used for precomposition with affine functions.
     -} 
    composeWithThin ::
        fa {-^ enclosure of @f@ -} ->
        Map.Map varid fa
         {-^ specifies the variables to substitute and for each such variable @v@, 
             gives an /exact/ enclosure of a function @f_v@ to substitute for @v@ -} ->
        fa 
        {-^ enclosure of @f[v |-> f_v]@ 
                
            BEWARE: Enclosure is probably incorrect where values of @f_v@ are outside the domain of @v@ in @f@.
        -}
    {-| 
        Find close upper and lower bounds of the volume of the entire enclosure.
        A negative volume means that the enclosure is certainly inconsistent.
        
        Explicitly specify the variables to identify the dimension of the domain.
    -}    
    volume :: [varid] -> fa -> ranra
    {-|
        Intersect two enclosures and measure the global improvement as one number.
        
        (Use 'RA.intersectMeasureImprovement' defined in module "Numeric.ER.Real.Approx" 
         to measure the improvement using a function enclosure.) 
        
        Explicitly specify the variables to identify the dimension of the domain.
    -}        
    intersectMeasureImprovement ::
        EffortIndex -> 
        [varid] ->
        fa -> 
        fa -> 
        (fa, ranra)
            {-^ enclosure intersection and measurement of improvement analogous to the one 
                returned by the pointwise 'RA.intersectMeasureImprovement' -}
    {-| 
        Safely integrate a @[-1,1]^n -> R^m@ function enclosure
        with some initial condition (origin and function at origin).
    -}    
    integrate :: 
        EffortIndex {-^ how hard to try -} ->
        fa {-^ function to integrate -} ->
        varid {-^ @x@ = variable to integrate by -} ->
        domra {-^ origin in terms of @x@; this has to be exact! -} ->
        fa {-^ values at origin -} ->
        fa

        
{-|
   Check that a pointwise operation previously performed on function approximations is 
   consistent with the same operation performed on
   selected points in the domain of these functions.
   The selected points are the centres of all faces of all dimensions,
   which includes the corners.
   
   The result of this function is the list of points in which 
   the consistency check failed.  The result of the operation
   is also included both for the real number version and the
   function approximation version.
-}        
keyPointsPointwiseConsistencyCheck ::
    (ERUnitFnApprox box varid domra ranra fa) =>
    ([ranra] -> ranra)  {-^ function @G@ acting on real numbers -} ->
    [fa] {-^ approximations of input functions -} ->
    fa {-^ alleged approximation of @G@ applied pointwise to the above function approximations -} ->
    [(box, ranra, ranra)]
keyPointsPointwiseConsistencyCheck g fIns fRes =
    keyPointsConsistencyCheck gPointwise fRes
    where
    gPointwise ptB =
        g $ map ((\[x] -> x) . FA.eval ptB) fIns
        
{-|
   Check that a function approximation is consistent with
   a real function that is meant to compute the same function.
   
   The result of this function is the list of points in which 
   the consistency check failed.  The result of the operation
   is also included both for the real number version and the
   function approximation version.
-}        
keyPointsConsistencyCheck ::
    (ERUnitFnApprox box varid domra ranra fa) =>
    (box -> ranra)  {-^ function @G@ acting on tuples of real numbers -} ->
    fa {-^ alleged approximation of @G@ over a domain box -} ->
    [(box, ranra, ranra)]
keyPointsConsistencyCheck g fRes =
    filter (isInConsistent) $ map testPoint points
    where
    points = map DBox.fromList $ allCombinations $ map getVarPoints varDoms
    varDoms = map (\v -> (v,unitInterval)) $ FA.getVariables fRes
    unitInterval = (-1) RA.\/ 1
    getVarPoints (var, dom) =
        (var, [domL, domM, domR])
        where
        (domL, domR) = RA.bounds dom
        (domM, _) = RA.bounds $ (domL + domR)/2
    testPoint ptB =
        (ptB, gResPt, fResPt)
        where
        gResPt = g ptB
        [fResPt] = FA.eval ptB fRes
    isInConsistent (_, gResPt, fResPt) =
        RA.isDisjoint gResPt fResPt
        