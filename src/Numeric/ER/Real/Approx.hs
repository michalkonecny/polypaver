{-|
    Module      :  Numeric.ER.Real.Approx
    Description :  classes abstracting exact reals
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Definitions of classes that describe what is
    required from arbitrary precision approximations
    of exact real numbers.
    
    We introduce two levels of abstraction for these
    approximations:
    
        * 'ERApprox' = 
            Approximating a real number by a *set* of real numbers
            that includes the approximated number.            
            Precision is measured using some fixed measure on the sets.
            Operations are "safe" wrt inclusion.
            The sets can sometimes be "anti-consistent" - being smaller than
            the empty set in the inclusion order.
                    
        * 'ERInnerOuterApprox' = 
            Like 'ERApprox' with the addition of operations that are "inner rounded"
            in the sense that each element of the rounded result set can 
            be obtained by the same operation performed on some elements of the arument set(s).

        * 'ERIntApprox' =
            Like ERApprox but assuming that the sets are 
            *intervals* of real numbers with finitely
            representable endpoints.
    
    To be imported qualified, usually with the synonym RA.
-}
module Numeric.ER.Real.Approx
(
    ERApprox(..),
    eqSingletons,
    leqSingletons,
    ltSingletons,
    effIx2ra,
    ERInnerOuterApprox(..),
    ERIntApprox(..),
    splitIRA,
    equalIntervals,
    exactMiddle,
    maxExtensionR2R,
    maxExtensionInnerR2R,
    ERApproxApprox(..),
    ERIntApproxApprox(..)
)
where

import Numeric.ER.BasicTypes
import qualified Numeric.ER.BasicTypes.ExtendedInteger as EI

import Data.Typeable

{-|
    A type whose elements represent sets that can be used
    to approximate a single extended real number with arbitrary precision.

    Operations are "safe" with respect to inclusion, which means that
    for any numbers admitted by the operand approximations the result
    of the operation is admitted by the result approximation.
   
    The sets can sometimes be "anti-consistent" - being smaller than
    the empty set in the inclusion order.  
    This can be understood as indicating that not only there is no correct real number
    approximated here, but some numbers (ie those in interior of the set)
    are excluded more strongly than the others.
    Prime examples of such sets are directed "inverted" intervals such as [2,1].  
    Such sets arise naturally from "inner rounded" operations - see 'ERInnerOuterApprox'.
-}
class (Fractional ra, Show ra, Eq ra) => ERApprox ra 
    where
    initialiseBaseArithmetic :: ra -> IO ()
    getPrecision :: ra -> Precision 
    {-^ 
            Precision is a measure of the set size.  It can be infinite.
            
            The default interpretation:
            
            * If the diameter of the set is d, then the precision
            should be near floor(- log_2 d).
    -}
    getGranularity :: ra -> Granularity
    -- ^ the lower the granularity the bigger the rounding errors
    setGranularityOuter :: Granularity -> ra -> ra
    -- ^ increase or safely decrease granularity
    setMinGranularityOuter :: Granularity -> ra -> ra
    -- ^ ensure granularity is not below the first arg
    isBottom :: ra -> Bool 
    -- ^ true if this approximation holds no information, ie it admits any real number 
    bottomApprox :: ra 
    -- ^ the bottom approximation - it admits any real number
    isExact :: ra -> Bool 
    -- ^ true if this approximation admits only one real number
    isConsistent :: ra -> Bool
    {- ^ true iff this approximation admits at least one real number -}
    isAnticonsistent :: ra -> Bool
    {- ^ true if this approximation is anti-consistent, which is a computational error 
         unless we used inner rounded operations -}
    toggleConsistency :: ra -> ra
    {- ^ 
        Toggle consistency - anti-consistency of the approximation. 
        Top is toggled with bottom.  
        Exact approximations are the only fixed points for this operation.
    -} 
    isTop :: ra -> Bool
    -- ^ true if this approximation is the most anti-consistent one
    topApprox :: ra 
    -- ^ the top approximation - strongly rejects all real numbers
    isDisjoint :: ra -> ra -> Bool
    isDisjoint a b = not $ isConsistent $ a /\ b
    isInteriorDisjoint :: ra -> ra -> Bool
    isInteriorDisjoint a b = isAnticonsistent $ a /\ b
    isBounded :: ra -> Bool
    {- ^ 
        True iff the approximation excludes infinity
        and, if anti-consistent, does not strongly exclude infinity.
    -}
    plusInfinity :: ra
    -- ^ an exact approximation admitting only the positive infinity
    refines :: ra -> ra -> Bool 
    -- ^ first arg is a subset of the second arg
    maybeRefines :: ra -> ra -> Maybe Bool 
    -- ^ like 'refines' but usable for types where 'refines' is only partially decidable
    (/\) :: ra -> ra -> ra 
    -- ^ join; combining the information in two approximations of the same number
    intersectMeasureImprovement ::
        EffortIndex -> ra -> ra -> (ra, ra)
    {-^ 
            First component of result is the intersection and the second component:
            
             * measures precision improvement of the intersection relative to the first argument
             
             * is a positive number: 1 means no improvement, 2 means doubled precision, etc. 
    -}
    equalReals :: ra -> ra -> Maybe Bool
    -- ^ semantic semi-decidable equality test
    compareReals :: ra -> ra -> Maybe Ordering
    -- ^ semantic semi-decidable comparison
    leqReals :: ra -> ra -> Maybe Bool
    -- ^ semantic semi-decidable less-than-or-equal comparison
    equalApprox :: ra -> ra -> Bool
    -- ^ syntactic equality test
    compareApprox :: ra -> ra -> Ordering
    -- ^ syntactic linear ordering
    double2ra :: Double -> ra
    -- ^ safe approximate conversion
    showApprox :: 
        Int {-^ number of relevant decimals to show -} ->
        Bool {-^ should show granularity -} ->
        Bool {-^ should show internal representation details -} ->
        ra {-^ the approximation to show -} ->
        String
    
{-|
    Assuming the arguments are singletons, equality is decidable.
-}
eqSingletons :: (ERApprox ra) => ra -> ra -> Bool
eqSingletons s1 s2 =  
    case equalReals s1 s2 of 
        Just b -> b
        _ -> False 

{-|
    Assuming the arguments are singletons, @<=@ is decidable.
-}
leqSingletons :: (ERApprox ra) => ra -> ra -> Bool
leqSingletons s1 s2 =  
    case compareReals s1 s2 of 
        Just EQ -> True
        Just LT -> True
        _ -> False 
        
{-|
    Assuming the arguments are singletons, @<@ is decidable.
-}
ltSingletons :: (ERApprox ra) => ra -> ra -> Bool
ltSingletons s1 s2 =  
    case compareReals s1 s2 of 
        Just LT -> True
        _ -> False 
        
{-|    
    This function converts
    an effort index to a real number approximation.
    
    Useful when an effort index is used in a formula
    mixed with real approximations.  
-}
effIx2ra :: 
    (ERApprox ra) =>
    EffortIndex -> ra
effIx2ra = fromInteger . toInteger

{-|
    A type whose elements represent some kind of nominal sets of real numbers
    over which one can perform two kinds of arithmetic:
   
    * "outer rounded": arithmetic that approximates maximal extensions from outside (ie the 'ERApprox' arithmetic)
   
    * "inner rounded": arithmetic that approximates maximal extensions from inside, potentially leading to
      anti-consistent set specifications (eg intervals whose endpoints are not in the usual order)

    Another explanation of the difference:

    * `outer': the approximation contains all the number(s) of interest
    * `inner': all numbers eligible for the approximation are numbers of interest

    Ie inner rounded operations have the property that each real number admitted by the result can
    be obtained as the exact result of the same operation performed on some real numbers admitted
    by the operand approximations.
    
    While in "outer rounded" operations it is desirable to make the result set as small as
    possible in order to reduce the amount of bogus result numbers, 
    in "inner rounded" operations it is desirable to make the result set as large as possible
    to lose less of the genuinely feasible result numbers.
     
    Inner rounded arithmetic is useful eg for proving/disproving inclusions "f(x) subset g(x)"
    where f and g are expressions using arithmetic extended to sets.
    For proving the inclusion, we need an inner rounded approximation of g(x)
    and for disproving the inclusion we need an inner rounded approximation of f(x).
   
    This is an abstraction of Kaucher's extended interval arithmetic    
    [Kaucher, E.: Interval Analysis in the Extended Interval Space IR, 
     Computing, Suppl. 2, 1980, pp. 33-49].
-}
class (ERApprox xra) => ERInnerOuterApprox xra 
    where
    (/\:) :: xra -> xra -> xra
    -- ^ inner rounded intersection
    (+:) :: xra -> xra -> xra
    -- ^ inner rounded addition
    (-:) :: xra -> xra -> xra
    -- ^ inner rounded subtraction
    a -: b = a +: (negate b)
    (*:) :: xra -> xra -> xra
    -- ^ inner rounded multiplication
    (/:) :: xra -> xra -> xra
    -- ^ inner rounded division
    setGranularityInner :: Granularity -> xra -> xra
    -- ^ increase or safely decrease granularity
    setMinGranularityInner :: Granularity -> xra -> xra
    -- ^ ensure granularity is not below the first arg

{-|
   A type whose elements represent sets that can be used
   to approximate a recursive set of closed extended real number intervals 
   with arbitrary precision.
-}
--class (ERApprox sra) => ERSetApprox sra where
--    (\/) :: sra -> sra -> sra -- ^ union; either approximation could be correct

{-|
   A type whose elements represent real *intervals* that can be used
   to approximate a single extended real number with arbitrary precision.

   Sometimes, these types can be used to approximate 
   a closed extended real number interval with arbitrary precision.
   Nevetheless, this is not guaranteed.
-}
class (ERApprox ira) => ERIntApprox ira 
    where
    doubleBounds :: ira -> (Double, Double) 
    floatBounds :: ira -> (Float, Float)
    integerBounds :: ira -> (EI.ExtendedInteger, EI.ExtendedInteger)
    bisectDomain :: 
        Maybe ira {-^ point to split at -} -> 
        ira {-^ interval to split -} -> 
        (ira, ira) -- ^ left and right, overlapping on a singleton
    defaultBisectPt :: ira -> ira
    -- | returns thin approximations of endpoints, in natural order 
    bounds :: ira -> (ira, ira)
    -- | make an interval from thin approximations of endpoints 
    fromBounds :: (ira, ira) -> ira
    {-|
         meet, usually constructing interval from approximations of its endpoints
         
         This does not need to be the meet of the real intervals 
         but it has to be a maximal element in the set of all
         ira elements that are below the two parameters.
    -}
    (\/) :: ira -> ira -> ira
    
{-|
    Return true if and only if the two intervals have equal endpoints.
-}
equalIntervals ::
    (ERIntApprox ira) => ira -> ira -> Bool
equalIntervals d1 d2 =
    d1L == d2L && d1U == d2U
    where
    (==) = eqSingletons
    (d1L, d1U) = bounds d1
    (d2L, d2U) = bounds d2


{-|
    Split an interval to a sequence of intervals whose union is the
    original interval using a given sequence of cut points.
    The cut points are expected to be in increasing order and contained
    in the given interval.  Violations of this rule are tolerated.
-}
splitIRA ::
    (ERIntApprox ira) =>
    ira {-^ an interval to be split -} -> 
    [ira] {-^ approximations of the cut points in increasing order -} -> 
    [ira]
splitIRA interval splitPoints =
    doSplit [] end pointsRev
    where
    (start, end) = bounds interval
    pointsRev = reverse $ start : splitPoints
    doSplit previousSegments nextRight [] = previousSegments
    doSplit previousSegments nextRight (nextLeft : otherPoints) =
        doSplit (nextLeft \/ nextRight : previousSegments) nextLeft otherPoints

{-|
    * Return the endpoints of the interval as well as the exact midpoint.
    
    * To be able to do this, there may be a need to increase granularity.
    
    * All three singleton intervals are set to the same new granularity.
-}        
exactMiddle ::
    (ERIntApprox ira) =>
    ira ->
    (ira,ira,ira,Granularity)
exactMiddle dom =
    case isExact domM of
        True ->
            (domL, domM, domR, gran)
        False ->
            (domLhg, domMhg, domRhg, higherGran)
    where
    (domL, domR) = bounds dom
    gran = max (getGranularity domL) (getGranularity domR)
    domM = (domL + domR) / 2
    higherGran = gran + 1
    domLhg = setMinGranularityOuter higherGran domL
    domRhg = setMinGranularityOuter higherGran domR
    domMhg = (domLhg + domRhg) / 2
     
        
{-| 
    This produces a function that computes the maximal extension of the
    given function.  A maximal extension function has the property:
    f(I) = { f(x) | x in I }.  Here we get this property only for the
    limit function for its 'EffortIndex' tending to infinity.
    For finite effor indices the function may add *outer* rounding
    but it should be reasonably small.
-}
maxExtensionR2R ::
    (ERIntApprox ira) =>
    (EffortIndex -> ira -> [ira]) 
        {-^ returns an *outer* approximation of all extrema within the interval -} ->
    (EffortIndex -> ira -> ira) 
        {-^ an *outer* rounding function behaving well on sequences that intersect to a point -} ->
    (EffortIndex -> ira -> ira) 
        {- ^ an outer rounding function behaving well on sequences that intersect to a non-empty interval -}
maxExtensionR2R getExtremes f ix x
    | not $ isConsistent x =
        toggleConsistency $
            maxExtensionInnerR2R getExtremes f ix $ toggleConsistency x 
    | getPrecision x < effIx2prec ix =
        foldl1 (\/) $ [f ix xL, f ix xR] ++ (getExtremes ix x)
    -- x is thin enough (?), don't bother evaluating by endpoints and extrema:
    | otherwise =
        f ix x
    where
    (xL, xR) = bounds x
        
{-| 
    This produces a function that computes the maximal extension of the
    given function.  A maximal extension function has the property:
    f(I) = { f(x) | x in I }.  Here we get this property only for the
    limit function for its 'EffortIndex' tending to infinity.
    For finite effor indices the function may include *inner* rounding
    but it should be reasonably small.
-}
maxExtensionInnerR2R ::
    (ERIntApprox ira) =>
    (EffortIndex -> ira -> [ira]) 
        {-^ returns an *outer* approximation of all extrema within the interval -} ->
    (EffortIndex -> ira -> ira) 
        {-^ an *outer* rounding function behaving well on sequences that intersect to a point -} ->
    (EffortIndex -> ira -> ira) 
        {- ^ an inner rounding function behaving well on sequences that intersect to a non-empty interval -}
maxExtensionInnerR2R getExtremes f ix x
    | not $ isConsistent x =
        toggleConsistency $
            maxExtensionR2R getExtremes f ix $ toggleConsistency x
    | otherwise =
        foldl1 (\/) $ map toggleConsistency $ [f ix xL, f ix xR] ++ (getExtremes ix x)
    where
    (xL, xR) = bounds x
        
{-|
   A type whose elements are thought of as sets of approximations of real numbers.
   
   Eg intervals of intervals, eg [[0,3],[1,2]] containing all intervals
   whose left endpoint is between 0 and 1 and the right endpoint is between 2 and 3.
   The upper bound interval can sometimes be anti-consistent,
   eg [[0,3],[2,1]] containing all intervals (consistent as well as anti-consistent) 
   with a left endpoint between [0,2] and the right endpoint between [1,3].
-}
class ERApproxApprox xra 
    where
    safeIncludes :: xra -> xra -> Bool
    -- ^ safe inclusion of approximations
    safeNotIncludes :: xra -> xra -> Bool
    -- ^ safe negation of inclusion of approximations
    includes :: xra -> xra -> Maybe Bool
    -- ^ like 'safeIncludes' but usable for types where 'safeIncludes' is only partially decidable
    includes aa1 aa2 
        | safeIncludes aa1 aa2 = Just True
        | safeNotIncludes aa1 aa2 = Just False
        | otherwise = Nothing

class ERIntApproxApprox xira 
    where
    oiBounds :: xira -> ((xira, xira), (xira, xira))
    fromOIBounds :: ((xira, xira), (xira, xira)) -> xira
    
--    safeIncludesMeasure :: xra -> xra -> Double
--    {- ^ positive result means inclusion satisfied, 
--        the more negative the result is, the more likely it is 
--        the inclusion is not satisfied -}
--    safeNotIncludesMeasure :: xra -> xra -> Double
--    includesMeasure :: xra -> xra -> (Maybe Bool, Double)
--    -- ^ when not decided, the higher the number, the larger the overlap that makes it impossible to decide  
--    includesMeasure aa1 aa2 
--        | includesMeasure >= 0 = Just True
--        | safeNotIncludes aa1 aa2 = Just False
--        | otherwise = Nothing
--        where
--        includeMeasure = safeIncludesMeasure aa1 aa2
--        excludeMeasure = safeNotIncludesMeasure aa1 aa2
        