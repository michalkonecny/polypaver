{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-|
    Module      :  Numeric.ER.RnToRm.Approx
    Description :  classes abstracting function approximations
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Approximation of a real functions with rectangular domains.
    
    To be imported qualified, usually with the synonym FA.    
-}
module Numeric.ER.RnToRm.Approx
(
    ERFnApprox(..),
    ERFnDomApprox(..),
    bisectUnbisectDepth,
    keyPointsConsistencyCheck,
    keyPointsPointwiseConsistencyCheck,
    getKeyPoints,
    getKeyPointsForDomainBox,
    ERFnApproxApprox(..)
)
where

import Prelude hiding (const)

import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainIntBox)
import Numeric.ER.BasicTypes

import Numeric.ER.Misc

import qualified Data.Map as Map

{-|
    A class of types that approximate first-order real functions
    @R^n -> R^m@ using some type of graph enclosures.  The domains
    of the functions cannot be specified by operations in this class,
    but they can be investigated and some operations require the
    knowledge of the domain (eg evaluation at a point).

    This class extends 'RA.ERApprox' so that we could perform point-wise
    operations on the functions.

    This class is associated with:
    
    * two real number types (instances of 'RA.ERIntApprox') for working with parts of the function's domain and range;
    
    * a type of boxes indexed by variables (instance of 'DomainBox') for working with
      multiple-dimension function domains.
-}
class 
    (RA.ERApprox fa, RA.ERIntApprox fa, RA.ERIntApprox domra, RA.ERIntApprox ranra, 
     DomainBox box varid domra) => 
    ERFnApprox box varid domra ranra fa
    | fa -> box varid domra ranra
    where
    {-| Check internal consistency and report problem if any. -}
    check :: 
        String {-^ indentification of caller location for easier debugging -} -> 
        fa -> fa
    domra2ranra :: 
        fa {-^ this parameter is not used except for type checking -} -> 
        domra -> ranra
    ranra2domra :: 
        fa {-^ this parameter is not used except for type checking -} -> 
        ranra -> domra 
    {-| 
        Get the internal degree of quality (usually polynomial degree) 
        of the approximation. 
    -}
    getDegree :: fa -> Int
    {-| 
        Set an upper bound on the degree of this function approximation.
        
        This reduces the degree immediately if necessary and also
        affects all operations performed with this value later.

        May also set the maximum size of the approximations to a default
        based on the degree and the dimension of this enclosure.
    -}
    setMaxDegree :: Int -> fa -> fa
    {-| 
        Get the current uppend bound on the degree associated 
        with this function approximation. 
    -}
    getMaxDegree :: fa -> Int
    {-| 
        Get the internal size of the approximation 
        (usually number of polynomial terms). 
    -}
    getSize :: fa -> Int
    {-| 
        Set an upper bound on the size of this function approximation.
        
        This reduces the size immediately if necessary and also
        affects all operations performed with this value later.
    -}
    setMaxSize :: Int -> fa -> fa
    {-| 
        Get the current uppend bound on the size associated 
        with this function approximation. 
    -}
    getMaxSize :: fa -> Int
    {-| 
        List all variables that are actually used in the approximation.
    -}
    getVariables :: fa -> [varid]
    {-| 
        Give a close upper bound of the precision of the range 
        at the best approximated point in the domain.
    -}
    getBestPrecision :: fa -> Precision
    {-| 
        Find some upper and lower bounds of the function over its domain.
    -}    
    getRangeApprox :: fa -> [ranra]
    {-| 
        Combine several functions with the same domain into one /tuple function/. 
    -}
    tuple :: [fa] -> fa
    {-|
        Reveal how many functions are bundled together.
    -}
    getTupleSize :: fa -> Int
    {-| 
        Modify a tuple of functions in a way 
        that does not treat the tuple elements uniformly.
    -}
    applyTupleFn ::
--        (ERFnApprox box varid domra ranra fa2) => 
--        ([fa2] -> [fa2]) -> (fa -> fa)
        ([fa] -> [fa]) -> (fa -> fa)
    {-| 
        Find close upper and lower bounds of the volume of the entire enclosure.
        A negative volume means that the enclosure is certainly not consistent.
    -}    
    volume :: fa -> ranra
    {-|
        Multiply a function approximation by a real number approximation.
    -}
    scale :: ranra -> fa -> fa
    {-|
        Intersect one approximation by another but only on a box within its domain.
    -}
    partialIntersect ::
        EffortIndex -> 
        box {-^ the subdomain; defined by clipping the range of some variables -} ->
        fa {-^ the enclosure to be used on the subdomain (but defined on the whole domain) -} ->
        fa {-^ function to improve by intersecting its subdomain -} -> 
        fa
    {-|
        Intersect two enclosures and measure the global improvement as one number.
        
        (Use 'RA.intersectMeasureImprovement' defined in module "Numeric.ER.Real.Approx" 
         to measure the improvement using a function enclosure.) 
    -}        
    intersectMeasureImprovement ::
        EffortIndex -> 
        fa -> 
        fa -> 
        (fa, ranra)
            {-^ enclosure intersection and measurement of improvement analogous to the one 
                returned by pointwise 'intersectMeasureImprovement' -}
    {-|
        Evaluate the function at the given point, rounding outwards.
    -}
    eval :: box -> fa -> [ranra]
    {-|
        Evaluate the function at the given point, rounding outwards.
    -}
    evalInner :: box -> fa -> [ranra]
    {-|
        Fix some variables in the function to the given exact values.
    -}
    partialEval :: box -> fa -> fa
    {-|
        Usually an expensive and fairly inaccurate composition without
        any restrictions.
     -} 
    compose ::
        fa {-^ enclosure of @f@, @f@ is non-decreasing in variable @var@ -} ->
        varid {-^ variable @var@ to get substituted in @f@ -} ->
        fa {-^ enclosure of @f_var@, to be substituted for @var@ -} ->        
        fa
        {-^ enclosure of @f[var |-> f_var]@ 
                
            BEWARE: Enclosure is probably incorrect where values of 
            @f_v@ are outside the domain of @v@ in @f@.
        -}
    {-|
        A simple and limited composition of functions applicable
        only when the range-defining function is known to be non-decreasing.
     -} 
    composeNonDecreasing ::
        fa {-^ enclosure of @f@, @f@ is non-decreasing in variable @var@ -} ->
        varid {-^ variable @var@ to get substituted in @f@ -} ->
        fa {-^ enclosure of @f_var@, to be substituted for @var@ -} ->        
        fa
        {-^ enclosure of @f[var |-> f_var]@ 
                
            BEWARE: Enclosure is probably incorrect where values of 
            @f_v@ are outside the domain of @v@ in @f@.
        -}
    {-|
        A simple and limited composition of functions applicable
        only when the range-defining function is known to be non-increasing. 
     -} 
    composeNonIncreasing ::
        fa {-^ enclosure of @f@, @f@ is non-increasing in variable @var@ -} ->
        varid {-^ variable @var@ to get substituted in @f@ -} ->
        fa {-^ enclosure of @f_var@, to be substituted for @var@ -} ->        
        fa
        {-^ enclosure of @f[var |-> f_var]@ 
                
            BEWARE: Enclosure is probably incorrect where values of 
            @f_v@ are outside the domain of @v@ in @f@.
        -}
        
    {-|
        Obtain an enclosure of a partial derivative of the approximated function.
        (Computed using automatic differentiation.)
    -}
    getPartialDerivative ::
        fa {-^ an approximation of @f@ -} ->
        varid {-^ a variable @x@ -} ->
        fa {-^ an approximation of @df/dx@ -}

{-|
    This class extends 'ERFnApprox' by:
    
    * making the domain of the function enclosure available for inspection;
    
    * allowing the construction of basic function enclosures
      where the domain has to be specified.
-}
class 
    (ERFnApprox box varid domra ranra fa,
     DomainIntBox box varid domra) => 
    ERFnDomApprox box varid domra ranra fa
    | fa -> box varid domra ranra
    where
    {-| 
        A function enclosure with no information about the function's values.
    -}
    bottomApprox :: 
        box {-^ the domain of the function -} -> 
        Int {-^ how many functions are bundled in this tuple -} -> 
        fa
    {-|
        Construct a constant enclosure for a tuple of functions.
    -}
    const :: box -> [ranra] -> fa
    {-|
        Construct the exact enclosure for a projection function
        (ie a variable).
    -}
    proj :: box ->  varid -> fa
    {-|
        Return the domain of the function enclosure.
    -}
    dom :: fa -> box
    {-| 
        Split the domain into two halves, yielding two function enclosures.
    -}
    bisect :: 
        varid {-^ variable (axis) to split on -} -> 
        Maybe domra {-^ where exactly to split (this has to be exact) -} -> 
        fa -> 
        (fa, fa)
    {-| 
        Merge function enclosures with neighbouring domains.
    -}
    unBisect :: 
        varid {-^ variable (axis) to glue on -} -> 
        (fa, fa) -> 
        fa
    {-| 
        Safely integrate a @R^n -> R^m@ function enclosure
        with some initial condition (origin and function at origin).
    -}    
    integrate :: 
        EffortIndex {-^ how hard to try -} ->
        fa {-^ function to integrate -} ->
        varid {-^ @x@ = variable to integrate by -} ->
        box {-^ integration range -} ->
        domra {-^ origin in terms of @x@; this has to be thin! -} ->
        fa {-^ values at origin -} ->
        fa
    {-| 
        Safely integrate a @R -> R^m@ function enclosure.
    -}    
    integrateUnary :: 
        EffortIndex {-^ how hard to try -} ->
        fa {-^ unary function to integrate -} ->
        domra {-^ integration range -} ->
        domra {-^ origin -} ->
        [ranra] {-^ values at origin -} ->
        fa
    -- default implementation reduces this to integrateMeasureImprovement:
    integrateUnary ix fD support origin vals =
        integrate ix fD defaultVar (DBox.unary support) origin (const (DBox.noinfo) vals)
    {-| 
        Safely integrate a @R^n -> R^m@ function enclosure
        intersecting it with a prior enclosure for the result.
        
        The prior enclosure could contains one of more initial value.
    -}    
    integrateMeasureImprovement :: 
        EffortIndex {-^ how hard to try -} ->
        fa {-^ function to integrate -} ->
        varid {-^ variable to integrate by -} ->
        box {-^ integration domain -} ->
        domra 
            {-^ a sub-domain with relevant new information - 
                either about initial value(s) or about derivative -} ->
        fa {-^ approximation to result, including initial value(s) -} -> 
        (fa, fa) 
            {-^ improved result and measurement of improvement analogous to the one 
                returned by pointwise 'intersectMeasureImprovement' -}
    {-| 
        Safely integrate a @R -> R^m@ function enclosure
        intersecting it with a prior enclosure for the result.
        
        The prior enclosure could contains one of more initial value.
    -}    
    integrateMeasureImprovementUnary :: 
        EffortIndex {-^ how hard to try -} ->
        fa {-^ unary function to integrate -} ->
        domra {-^ integration domain -} ->
        domra 
            {-^ a sub-domain with relevant new information - 
                either about initial value(s) or about derivative -} ->
        fa {-^ approximation to result, including initial value(s) -} -> 
        (fa, fa) 
            {-^ improved result and measurement of improvement analogous to the one 
                returned by pointwise 'intersectMeasureImprovement' -}
    -- default implementation reduces this to integrateMeasureImprovement:
    integrateMeasureImprovementUnary ix fD support origin fP =
        integrateMeasureImprovement ix fD defaultVar (DBox.unary support) origin fP
        
        
{-|
    Recursively perform a number of bisections and then
    glue the bits back together.  
    
    This way we can ensure that
    a piece-wise enclosure has a partition that goes
    to at least the given depth. 
-}
bisectUnbisectDepth ::
    (ERFnDomApprox box varid domra ranra fa) =>
    Int {-^ required depth of bisection -} ->
    fa -> 
    fa
bisectUnbisectDepth depth f =
    aux splitVars depth f
    where
    splitVars = concat $ repeat $ DBox.keys $ dom f
    aux (var : restVars) depthsToGo f 
        | depthsToGo <= 0 = f
        | otherwise =
            unBisect var (fLDone, fRDone)
        where
        fLDone = aux restVars depthsToGoM1 fL
        fRDone = aux restVars depthsToGoM1 fR
        (fL, fR) = bisect var Nothing f
        depthsToGoM1 = depthsToGo - 1

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
    (ERFnDomApprox box varid domra ranra fa) =>
    ([ranra] -> ranra)  {-^ function @G@ acting on real numbers -} ->
    [fa] {-^ approximations of input functions -} ->
    fa {-^ alleged approximation of @G@ applied pointwise to the above function approximations -} ->
    [(box, ranra, ranra)]
keyPointsPointwiseConsistencyCheck g fIns fRes =
    keyPointsConsistencyCheck gPointwise fRes
    where
    gPointwise ptB =
        g $ map ((\[x] -> x) . eval ptB) fIns
        
{-|
   Check that a function approximation is consistent with
   a real function that is meant to compute the same function.
   
   The result of this function is the list of points in which 
   the consistency check failed.  The result of the operation
   is also included both for the real number version and the
   function approximation version.
-}        
keyPointsConsistencyCheck ::
    (ERFnDomApprox box varid domra ranra fa) =>
    (box -> ranra)  {-^ function @G@ acting on tuples of real numbers -} ->
    fa {-^ alleged approximation of @G@ over a domain box -} ->
    [(box, ranra, ranra)]
keyPointsConsistencyCheck g fRes =
    filter (isInConsistent) $ map testPoint points
    where
    points = getKeyPoints fRes
    testPoint ptB =
        (ptB, gResPt, fResPt)
        where
        gResPt = g ptB
        [fResPt] = eval ptB fRes
    isInConsistent (_, gResPt, fResPt) =
        RA.isDisjoint gResPt fResPt

getKeyPoints ::
    (ERFnDomApprox box varid domra ranra fa) =>
    fa -> [box]
getKeyPoints fa =
    getKeyPointsForDomainBox $ dom fa
    
getKeyPointsForDomainBox ::
    (DBox.DomainBox box varid ira, RA.ERIntApprox ira) =>
    box -> [box]
getKeyPointsForDomainBox domB =
    points
    where
    points = map DBox.fromList $ allCombinations $ varDomPoints
    varDomPoints = map (\(v,dom) -> (v, getDomPoints dom)) $ DBox.toList $ domB
    getDomPoints dom =
        [domL, domM, domR]
        where
        (domL, domR) = RA.bounds dom
        (domM, _) = RA.bounds $ (domL + domR)/2


{-|
    A class of types that approximate function enclosures of first-order real functions
    @R^n -> R^m@ eg using a pair of function enclosures.  The domains
    of the functions can be neither specified nor investigated 
    by operations in this class.

    This class extends 'RA.ERApproxApprox' so that we could perform point-wise
    operations on the function enclosures.

    This class is associated with:
    
    * a real number type (instance of 'RA.ERIntApprox') for working with parts of the function's domain
    
    * a real number approximation approximation for approximating the function enclosure 
      range at an individual point or uniformly over many points;
    
    * a type of boxes indexed by variables (instance of 'DomainBox') for working with
      multiple-dimension function domains.
-}
class 
    (RA.ERIntApprox domra, RA.ERApproxApprox ranraa, 
     DomainBox box varid domra) => 
    ERFnApproxApprox box varid domra ranraa fa
    | fa -> box varid domra ranraa
    where
    evalAA :: box -> fa -> [ranraa]

    