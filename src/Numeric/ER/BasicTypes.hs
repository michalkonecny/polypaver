{-|
    Module      :  Numeric.ER.BasicTypes
    Description :  auxiliary types for exact real number processing 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    auxiliary types for exact real number processing
-}
module Numeric.ER.BasicTypes 
where

import qualified Numeric.ER.BasicTypes.ExtendedInteger as EI

{-|
    Precision represents an upper bound on the measure of 
    an approximation viewed as a set;
    not to be confused with the precision of 
    an 'Numeric.ER.Real.Base.Float.ERFloat' and similar.
     
    In an approximation comprising a number of
    instances of 'Numeric.ER.Real.Base.ERRealBase',
    we will refer to the bit-precision of these base components
    as the 'Granularity' of the approximation.
-}
type Precision = EI.ExtendedInteger

{-|
  The bit size of the floating point numbers (or similar)
  used internally in real number and function approximations.
-}
type Granularity = Int

prec2gran :: Precision -> Granularity
prec2gran = fromInteger . toInteger

{-|
    This type synonym should be used for funciton parameter(s)
    that guide the convergence of the function's result to
    a perfect (exact) result.  
    
    The name should remind us 
    that there is no universally valid relationship between
    this integer the quality (precision) of the result.    
    The only condition usually assumed is that in the limit
    when the effort index rises to infinity, the result 
    should be exact.
-}
type EffortIndex = Integer

effIx2gran :: EffortIndex -> Granularity
effIx2gran  = fromInteger . toInteger

effIx2prec :: EffortIndex -> Precision
effIx2prec = fromInteger . toInteger

effIx2int :: EffortIndex -> Int
effIx2int = fromInteger . toInteger

int2effIx :: Int -> EffortIndex
int2effIx = fromInteger . toInteger

prec2effIx :: Precision -> EffortIndex
prec2effIx = fromInteger . toInteger

gran2effIx :: Granularity -> EffortIndex
gran2effIx = fromInteger . toInteger
 
