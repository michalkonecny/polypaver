{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.Base
    Description :  class abstracting imprecise function arithmetic on [-1,1]^n
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Classes  abstracting function arithmetic with directed rounding.
    Instances are used to describe a boundary for an approximation
    to a real function on the interval [-1,1]^n.
    
    To be imported qualified, usually with the synonym UFB.
-}
module Numeric.ER.RnToRm.UnitDom.Base where

import Prelude hiding (min, max, recip, const)

import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainIntBox)
import Numeric.ER.BasicTypes
import qualified Numeric.ER.Real.Base as B
import qualified Numeric.ER.Real.Approx as RA

import Numeric.ER.Misc

import qualified Data.Map as Map

import Data.Typeable

class
    (B.ERRealBase b, RA.ERIntApprox ra, Ord ufb, Show ufb,
     DomainBox boxb varid b, DomainIntBox boxra varid ra) => 
    ERUnitFnBase boxb boxra varid b ra ufb
    | ufb -> boxb boxra varid b ra
    where

    {--------------}        
    {----- Miscellaneous associated operations -----}
    {--------------}        

    {-| This should be evaluated before using any of the following operations. -}
    initialiseBaseArithmetic :: ufb -> IO ()
    initialiseBaseArithmetic _ =
        B.initialiseBaseArithmetic (0 :: b)

    {-|
        Convert from the associated interval type to the base type.
        (The types are determined by the given example function.)
    -}
    raEndpoints :: 
        ufb {-^ this parameter is not used except for type checking -} -> 
        ra -> 
        (b,b)
    {-|
        Convert from the base type to the associated interval type. 
        (The types are determined by the given example function.)
    -}
    raFromEndpoints :: 
        ufb {-^ this parameter is not used except for type checking -} -> 
        (b,b) ->
        ra

    {-|
        A linear ordering on basic functions, which can be syntactic and rather arbitrary. 
    -}
    compareApprox :: ufb -> ufb -> Ordering

    showDiGrCmp :: 
        Int {- ^ number of decimal digits to show -} ->
        Bool {-^ whether to show granularity -} ->
        Bool {-^ whether to show internal structure -} ->
        ufb -> String
        
    {--------------}        
    {----- Structural analysis and update of functions -----}
    {--------------}        

    {-|
        Check internal consistency of the basic function, typically absence of NaN.
    -}
    isValid :: ufb -> Bool
    {-| 
        Check internal consistency of the basic function and report problem if any.
    -}
    check :: 
        String {-^ indentification of caller location for easier debugging -} -> 
        ufb -> ufb
    
    {-| 
        Get the granularity of the coefficients inside this basic function.
    -}
    getGranularity :: ufb -> Granularity
    setMinGranularity :: Granularity -> ufb -> ufb
    setGranularity :: Granularity -> ufb -> ufb
    
    {-| 
        Get the degree of this basic function.
        
        If the function is a polynomial, this function should
        return its degree. 
    -}
    getDegree :: ufb -> Int
    {-| 
        Decrease the degree of a basic function, rounding pointwise upwards.
    -}
    reduceDegreeUp :: Int -> ufb -> ufb
    
    {-|
        Get the term size of this basic function.
        
        If the function is a polynomial, this function should
        return the number of terms in the polynomial. 
    -}
    getSize :: ufb -> Int
    {-| 
        Decrease the size of this basic function, rounding pointwise upwards.
    -}
    reduceSizeUp :: Int -> ufb -> ufb
    
    {-|
        Get a list of all variables featured in this basic function.
    -}
    getVariables :: ufb -> [varid]
    
    {--------------}        
    {----- Construction of basic functions -----}
    {--------------}        
    
    {-| Construct a constant basic function. -}
    const :: b -> ufb
    
    {-| Construct an affine basic function. -}
    affine :: 
        b {-^ value at 0 -} ->
        Map.Map varid b {-^ ascent of each base vector -} -> 
        ufb

    {-|
        Return the coefficients of an affine function @a_0 + a_1x_1 + a_2x_2 + ... + a_nx_n@
        that majors the given function over its entire domain. 
    -}
    getAffineUpperBound :: 
        ufb ->
        (b, Map.Map varid b)

    {--------------}
    {----- Pointwise order operations ----------}    
    {--------------}
    
    {-|
        Find an upper bound of a basic function over @[-1,1]^n@.
    -}
    bounds :: EffortIndex -> ufb -> (b,b)
    
    {-|
        Find an upper bound of a basic function over @[-1,1]^n@.
    -}
    upperBound :: EffortIndex -> ufb -> b
    
    {-|
        Find an upper bound of a basic function over @[-1,1]^n@.
    -}
    upperBoundPrecise :: EffortIndex -> ufb -> b
    
    {-|
        Find an upper bound of a basic function over @[-1,1]^n@.
    -}
    lowerBound :: EffortIndex -> ufb -> b
    
    {-|
        Find an upper bound of a basic function over @[-1,1]^n@.
    -}
    lowerBoundPrecise :: EffortIndex -> ufb -> b
    
    {-|
        Approximate the function @max(f1,f2)@ from above.
    -}
    maxUp :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        ufb {-^ @f1@ -} -> 
        ufb {-^ @f2@ -} -> 
        ufb
    {-|
        Approximate the function @min(f1,f2)@ from above.
    -}
    minUp :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        ufb {-^ @f1@ -} -> 
        ufb {-^ @f2@ -} -> 
        ufb
    {-|
        Approximate the function @max(f1,f2)@ from below.
    -}
    maxDown :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        ufb {-^ @f1@ -} -> 
        ufb {-^ @f2@ -} -> 
        ufb
    {-|
        Approximate the function @min(f1,f2)@ from below.
    -}
    minDown :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        ufb {-^ @f1@ -} -> 
        ufb {-^ @f2@ -} -> 
        ufb

    {--------------}        
    {----- Field operations ----------}
    {--------------}        
    
    {-| Pointwise exact negation of a basic function -}
    neg :: ufb -> ufb

    {-|
        Add a scalar to a basic function, rounding upwards.
    -}
    addConstUp :: b -> ufb -> ufb
    
    {-|
        Multiply a basic function by a scalar, rounding upwards.
    -}
    scaleUp :: b -> ufb -> ufb
    
    {-| 
        Multiply a basic function by an approximation of a scalar, 
        rounding upwards. 
    -} 
    scaleApproxUp :: 
        Int {-^ maximum polynomial degree -} -> 
        Int {-^ maximum term count -} -> 
        ra -> ufb -> ufb
     
    {-| Pointwise upwards rounded addition -}
    (+^) :: ufb -> ufb -> ufb
    {-| Pointwise upwards rounded subtraction -}
    (-^) :: ufb -> ufb -> ufb
    {-| Pointwise upwards rounded multiplication -}
    (*^) :: ufb -> ufb -> ufb
    
    {-| 
        Approximate the function @1/f@ from above, assuming
        @f@ does not hit zero in the unit domain.
    -}
    recipUp :: Int -> Int -> EffortIndex -> ufb -> ufb

    {--------------}
    {----- Evaluation and composition of functions -----}
    {--------------}
    
    {-|
        Evaluate a basic function at a point rounding upwards 
        using a basic number for both the point and the result.
    -}
    evalUp :: boxb -> ufb -> b

    {-|
        Evaluate a basic function at a point rounding downwards 
        using a basic number for both the point and the result.
    -}
--    evalDown :: boxb -> ufb -> b

    {-|
        Safely evaluate a basic function at a point using a real number approximation
        for both the point and the result.
    -}
    evalApprox :: boxra -> ufb -> ra
    
    {-|
        Partially evaluate a basic function at a lower-dimensional point 
        given using a real number approximation.
        Approximate the resulting function from above.
    -}
    partialEvalApproxUp :: boxra -> ufb -> ufb

    {-| 
        Compose two basic functions, rounding upwards, 
        assuming @f_v@ ranges within the domain @[-1,1]@. 
    -}
    composeUp ::
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        ufb {-^ function @f@ -} -> 
        varid {-^ variable @v@ to substitute in @f@ -} -> 
        ufb 
         {-^ function @f_v@ to substitute for @v@ 
             that maps @[-1,1]@ into @[-1,1]@  -} ->
        ufb {-^ pointwise upper bound of @f[v |-> f_v]@ -}

    {-| 
        Substitute several variables in a basic function with other basic functions, 
        rounding upwards, assuming each @f_v@ ranges 
        within the domain @[-1,1]@. 
    -} 
    composeManyUp ::
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        ufb {-^ function @f@ -} -> 
        Map.Map varid ufb 
         {-^ variables to substitute and for each variable @v@, 
             function @f_v@ to substitute for @v@ 
             that maps @[-1,1]@ into @[-1,1]@  -} ->
        ufb {-^ pointwise upper bound of @f[v |-> f_v]@ -}

    {-| 
        Compose two basic functions, rounding downwards, 
        assuming @f_v@ ranges within the domain @[-1,1]@. 
    -}
    composeDown ::
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        ufb {-^ function @f@ -} -> 
        varid {-^ variable @v@ to substitute in @f@ -} -> 
        ufb 
         {-^ function @f_v@ to substitute for @v@ 
             that maps @[-1,1]@ into @[-1,1]@  -} ->
        ufb {-^ pointwise lower bound of @f[v |-> f_v]@ -}

    {-| 
        Substitute several variables in a basic function with other basic functions, 
        rounding downwards, assuming each @f_v@ ranges 
        within the domain @[-1,1]@. 
    -} 
    composeManyDown ::
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        ufb {-^ function @f@ -} -> 
        Map.Map varid ufb 
         {-^ variables to substitute and for each variable @v@, 
             function @f_v@ to substitute for @v@ 
             that maps @[-1,1]@ into @[-1,1]@  -} ->
        ufb {-^ pointwise lower bound of @f[v |-> f_v]@ -}

    {--------------}
    {----- Approximate symbolic integration ----------}    
    {--------------}

    {-|
        Approximate the primitive function of @f@ from below and from above.
    -}
    integrate ::
        varid {-^ variable to integrate by -} -> 
        ufb {-^ @f@ -} -> 
        (ufb, ufb)
    
    {-|
        Approximate the derivative of @f@ from below and from above.
    -}
    differentiate ::
        varid {-^ variable to differentiate by -} -> 
        ufb {-^ @f@ -} -> 
        (ufb, ufb)
    
    {-| 
        Measure the volume between a function 
        and the zero hyperplane on the domain @[-1,1]^n@.
    -}
    volumeAboveZeroUp :: 
        [varid] 
            {-^ dimensions to include in the measuring domain; 
                have to include all those present in @f@ -} -> 
        ufb {-^ @f@ -} -> 
        b
    volumeAboveZeroUp vars p =
--    unsafePrint ("chplVolumeAboveZero: returning:" ++ show result) $
--    unsafePrint ("chplVolumeAboveZero: vars = " ++ show vars) $
        result
        where
        result = integUpAtEvenCorners + integDownAtOddCorners
        integUpAtEvenCorners = sumUp $ map (\pt -> evalUp pt integUp) evenCorners
        integDownAtOddCorners = sumUp $ map (\pt -> evalUp pt integDownNeg) oddCorners
        evenCorners = map (DBox.fromList) evenCornersL
        oddCorners = map (DBox.fromList) oddCornersL
        (evenCornersL, oddCornersL) =
            allPairsCombinationsEvenOdd $ zip vars $ repeat (1,-1)
        integUp = integrateByAllVars snd p vars
        integDownNeg = neg $ integrateByAllVars fst p vars
        integrateByAllVars pick p [] = p
        integrateByAllVars pick p (x : xs) =
            integrateByAllVars pick ip xs
            where
            ip = pick $ integrate x p
        
compareReals ix f1 f2 
   | f1IsCertainlyBelowf2 = Just LT
   | f1IsCertainlyAbovef2 = Just GT
--   | f1CertainlyEqualsf2 = Just EQ
   | otherwise = Nothing
   where
   f1IsCertainlyBelowf2 =
--       unsafePrint 
--       (
--          "UFB.compareReals: f1IsCertainlyBelowf2: "
--          ++ "\n f1 = " ++ show f1
--          ++ "\n f2 = " ++ show f2
--          ++ "\n f1 +^ (neg f2) = " ++ show (f1 +^ (neg f2))
--          ++ "\n upperBound ix (f1 +^ (neg f2)) = " ++ show (upperBound ix (f1 +^ (neg f2)))     
--       )
--       $ 
       upperBound ix (f1 +^ (neg f2)) < 0
   f1IsCertainlyAbovef2 = 
--       unsafePrint
--       (
--          "UFB.compareReals: f1IsCertainlyAbovef2: "
--          ++ "\n f1 = " ++ show f1
--          ++ "\n f2 = " ++ show f2
--          ++ "\n f2 +^ (neg f1) = " ++ show (f2 +^ (neg f1))
--          ++ "\n upperBound ix (f2 +^ (neg f1)) = " ++ show (upperBound ix (f2 +^ (neg f1)))     
--       )
--       $ 
       upperBound ix (f2 +^ (neg f1)) < 0
   f1CertainlyEqualsf2 = False

class
    (ERUnitFnBase boxb boxra varid b ra ufb) => 
    ERUnitFnBaseEncl boxb boxra varid b ra ufb
    | ufb -> boxb boxra varid b ra
    where
    boundsEncl :: EffortIndex -> (ufb,ufb) -> (b,b)
    
    {-| Construct a constant basic enclosure (negated lower bound fn, upper bound fn) 
        from bounds given as coeffients (lower bound, upper bound). -}
    constEncl :: (b,b) -> (ufb, ufb)
    
    evalEncl :: boxra -> (ufb,ufb) -> ra
    
    evalEnclInner :: boxra -> (ufb,ufb) -> ra
    
    {-| Enclosure and base constant addition

        IMPORTANT: enclosure = (NEGATED lower bound, upper bound)    
     -}
    addConstEncl :: 
        Int {-^ maximum polynomial degree -} -> 
        Int {-^ maximum term count -} -> 
        b -> (ufb,ufb) -> (ufb, ufb)
      
    {-| Enclosure scaling by a base constant

        IMPORTANT: enclosure = (NEGATED lower bound, upper bound)    
     -}
    scaleEncl :: 
        Int {-^ maximum polynomial degree -} -> 
        Int {-^ maximum term count -} -> 
        b -> (ufb,ufb) -> (ufb, ufb)
      
    {-| Enclosure addition

        IMPORTANT: enclosure = (NEGATED lower bound, upper bound)    
     -}
    addEncl :: 
        Int {-^ maximum polynomial degree -} -> 
        Int {-^ maximum term count -} -> 
        (ufb,ufb) -> (ufb,ufb) -> (ufb, ufb)
      
    {-| Enclosure multiplication 

        IMPORTANT: enclosure = (NEGATED lower bound, upper bound)    
     -}
    multiplyEncl :: 
        Int {-^ maximum polynomial degree -} -> 
        Int {-^ maximum term count -} -> 
        (ufb,ufb) -> (ufb,ufb) -> (ufb, ufb)
      
    {-|
        Approximate the reciprocal of an enclosure, assuming
        @f@ does not hit zero in the unit domain.
        
        IMPORTANT: enclosure = (negated lower bound, upper bound)    
    -}
    recipEncl :: 
        Int {-^ max degree for result -} ->
        Int {-^ max approx size for result -} ->
        EffortIndex -> 
        (ufb,ufb) {-^ enclosure of @f@ -} -> 
        (ufb,ufb)

    {-| 
        Compose two basic functions, rounding downwards and upwards, 
        assuming @f_v@ ranges within the domain @[-1,1]@. 
    -}
    composeEncl ::
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        ufb {-^ function @f@ -} -> 
        varid {-^ variable @v@ to substitute in @f@ -} -> 
        (ufb, ufb) 
         {-^ enclosure of a function @f_v@ to substitute for @v@ 
             that maps @[-1,1]@ into @[-1,1]@  -} ->
        (ufb, ufb) {-^ enclosure of @f[v |-> f_v]@ -}

    {-| 
        Substitute several variables in a basic function with other basic functions, 
        rounding downwards and upwards, assuming each @f_v@ ranges 
        within the domain @[-1,1]@. 
    -} 
    composeManyEncls ::
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        ufb {-^ function @f@ -} -> 
        Map.Map varid (ufb, ufb) 
         {-^ variables to substitute and for each variable @v@, 
             enclosure of a function @f_v@ to substitute for @v@ 
             that maps @[-1,1]@ into @[-1,1]@  -} ->
        (ufb, ufb) {-^ enclosure of @f[v |-> f_v]@ -}


class
    (ERUnitFnBaseEncl boxb boxra varid b ra ufb) => 
    ERUnitFnBaseElementary boxb boxra varid b ra ufb
    | ufb -> boxb boxra varid b ra
    where
    {-|
        Approximate @sqrt(f)@ for enclosures.
    -}
    sqrtEncl :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        EffortIndex {-^ how hard to try when approximating exp as a polynomial -} -> 
        (ufb, ufb) {-^ @f@ -} -> 
        (ufb, ufb)
    {-|
        Approximate @exp(f)@ for enclosures.
    -}
    expEncl :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        EffortIndex {-^ how hard to try when approximating exp as a polynomial -} -> 
        (ufb, ufb) {-^ @f@ -} -> 
        (ufb, ufb)
    {-| 
        Approximate @log(f)@ for enclosures.
    -}
    logEncl :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        EffortIndex {-^ how hard to try when approximating log as a polynomial -} -> 
        (ufb, ufb) {-^ @f@ -} -> 
        (ufb, ufb)
    {-| 
        Approximate @sin(f)@ for enclosures,
        assuming the range of @f@ is within @[-pi/2,pi/2]@.
    -}
    sinEncl :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        EffortIndex {-^ how hard to try when approximating sin as a polynomial -} -> 
        (ufb, ufb) {-^ @f@ -} -> 
        (ufb, ufb)     
    {-|
        Approximate @cos(f)@ for enclosures,
        assuming the range of @f@ is within @[-pi/2,pi/2]@.
    -}
    cosEncl :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        EffortIndex {-^ how hard to try when approximating cos as a polynomial -} -> 
        (ufb, ufb) {-^ @f@ -} -> 
        (ufb, ufb)
    {-|
        Approximate @atan(f)@ for enclosures.
    -}
    atanEncl :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        EffortIndex {-^ how hard to try when approximating cos as a polynomial -} -> 
        (ufb, ufb) {-^ @f@ -} -> 
        (ufb, ufb)

class
    (ERUnitFnBase boxb boxra varid b ra ufb) => 
    ERUnitFnBaseIEncl boxb boxra varid b ra ufb
    | ufb -> boxb boxra varid b ra
    where
    {-| Construct a constant basic inner enclosure 
            (negated lower bound fn, upper bound fn, is enclosure definitely anticonsistent?) 
        from bounds given as coeffients (lower bound, upper bound).
        An inner enclosure @(lnI,hI)@ is anticonsistent
        iff @hI + lnI  <= 0@, ie upper bound is never above lower bound.
    -}
    constIEncl :: (b,b) -> ((ufb, ufb), Bool)
    
    evalIEncl :: boxra -> ((ufb,ufb),Bool) -> ra
    {-|
        Inner enclosure addition.
     -}
    addIEncl :: 
        Int {-^ maximum polynomial degree -} -> 
        Int {-^ maximum term count -} ->
        ((ufb, ufb), Bool) ->
        ((ufb, ufb), Bool) ->
        ((ufb, ufb), Bool)
      
    {-|
        Inner enclosure multiplication.
     -}
    multiplyIEncl :: 
        Int {-^ maximum polynomial degree -} -> 
        Int {-^ maximum term count -} ->
        ((ufb, ufb), Bool) ->
        ((ufb, ufb), Bool) ->
        ((ufb, ufb), Bool)
      
    {-|
        Approximate the reciprocal of an inner enclosure, assuming
        @f@ is positive in the unit domain.
    -}
    recipIEnclPositive :: 
        Int {-^ max degree for result -} ->
        Int {-^ max approx size for result -} ->
        EffortIndex ->
        ((ufb, ufb), Bool) ->
        ((ufb, ufb), Bool) 

    {-| 
        Compose two basic functions, rounding downwards and upwards, 
        assuming @f_v@ ranges within the domain @[-1,1]@. 
    -}
    composeIEncl ::
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        ufb {-^ function @f@ -} -> 
        varid {-^ variable @v@ to substitute in @f@ -} -> 
        ((ufb, ufb), Bool) 
         {-^ inverse enclosure of a function @f_v@ to substitute for @v@ 
             that maps @[-1,1]@ into @[-1,1]@  -} ->
        ((ufb, ufb), Bool) {-^ inverse enclosure of @f[v |-> f_v]@ -}

    {-| 
        Substitute several variables in a basic function with other basic functions, 
        rounding downwards and upwards, assuming each @f_v@ ranges 
        within the domain @[-1,1]@. 
    -} 
    composeManyIEncls ::
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        ufb {-^ function @f@ -} -> 
        Map.Map varid ((ufb, ufb), Bool) 
         {-^ variables to substitute and for each variable @v@ 
             inverse enclosure of a function @f_v@ to substitute for @v@ 
             that maps @[-1,1]@ into @[-1,1]@  -} ->
        ((ufb, ufb), Bool) {-^ inverse enclosure of @f[v |-> f_v]@ -}

class
    (ERUnitFnBaseIEncl boxb boxra varid b ra ufb) => 
    ERUnitFnBaseIElementary boxb boxra varid b ra ufb
    | ufb -> boxb boxra varid b ra
    where
    {-|
        Approximate @sqrt(f)@ for enclosures.
    -}
    sqrtIEncl :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        EffortIndex {-^ how hard to try when approximating exp as a polynomial -} -> 
        ((ufb, ufb), Bool) {-^ @f@ -} -> 
        ((ufb, ufb), Bool)
    {-|
        Approximate @exp(f)@ for enclosures.
    -}
    expIEncl :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        EffortIndex {-^ how hard to try when approximating exp as a polynomial -} -> 
        ((ufb, ufb), Bool) {-^ @f@ -} -> 
        ((ufb, ufb), Bool)
    {-| 
        Approximate @log(f)@ for enclosures.
    -}
    logIEncl :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        EffortIndex {-^ how hard to try when approximating log as a polynomial -} -> 
        ((ufb, ufb), Bool) {-^ @f@ -} -> 
        ((ufb, ufb), Bool)
    {-| 
        Approximate @sin(f)@ for enclosures,
        assuming the range of @f@ is within @[-pi/2,pi/2]@.
    -}
    sinIEncl :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        EffortIndex {-^ how hard to try when approximating sin as a polynomial -} -> 
        ((ufb, ufb), Bool) {-^ @f@ -} -> 
        ((ufb, ufb), Bool)     
    {-|
        Approximate @cos(f)@ for enclosures,
        assuming the range of @f@ is within @[-pi/2,pi/2]@.
    -}
    cosIEncl :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        EffortIndex {-^ how hard to try when approximating cos as a polynomial -} -> 
        ((ufb, ufb), Bool) {-^ @f@ -} -> 
        ((ufb, ufb), Bool)
    {-|
        Approximate @atan(f)@ for enclosures.
    -}
    atanIEncl :: 
        Int {-^ max degree for result -} -> 
        Int {-^ max approx size for result -} ->
        EffortIndex {-^ how hard to try when approximating cos as a polynomial -} -> 
        ((ufb, ufb), Bool) {-^ @f@ -} -> 
        ((ufb, ufb), Bool)
    
