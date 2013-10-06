{-|
    Module      :  Numeric.ER.Real.Approx.Elementary
    Description :  abstraction of exact reals capable of elementary operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
        
    To be imported qualified, usually with the synonym RAEL.
-}
module Numeric.ER.Real.Approx.Elementary 
(
    ERApproxElementary(..),
    ERInnerOuterApproxElementary(..)
)
where

import Prelude hiding (exp, log, sin, cos)

import qualified Numeric.ER.Real.Approx as RA 
import Numeric.ER.Real.Approx ((+:),(-:),(*:),(/:)) 
import Numeric.ER.BasicTypes

import Numeric.ER.Real.Arithmetic.Elementary

{-|
    A class defining various common real number operations
    in a approximation-aware fashion, ie introducing effort indices.
    
    All operations here have default implementations based on
    "Numeric.ER.Real.Arithmetic.Elementary".
-}
class (RA.ERIntApprox ra, Ord ra) => (ERApproxElementary ra) 
    where
    abs :: EffortIndex -> ra -> ra
    abs ix = Prelude.abs
    min :: EffortIndex -> ra -> ra -> ra
    min ix = Prelude.min
    max :: EffortIndex -> ra -> ra -> ra
    max ix = Prelude.max
    sqrt :: EffortIndex -> ra -> ra
    sqrt = erSqrt_IR
    exp :: EffortIndex -> ra -> ra
    exp = erExp_IR
    log :: EffortIndex -> ra -> ra
    log = erLog_IR
    (**) :: EffortIndex -> ra -> ra -> ra
    (**) ix b e = exp ix $ e * (log ix b)
    pi :: EffortIndex -> ra
    pi = erPi_R
    sin :: EffortIndex -> ra -> ra
    sin = erSine_IR
    cos :: EffortIndex -> ra -> ra
    cos = erCosine_IR
    tan :: EffortIndex -> ra -> ra
    tan ix r = (sin ix r) / (cos ix r) 
    atan :: EffortIndex -> ra -> ra
    atan = erATan_IR
    
{-|
    A class defining various common real number operations
    in a approximation-aware fashion, ie introducing effort indices.
    
    All operations here have default implementations based on
    "Numeric.ER.Real.Arithmetic.Elementary".
-}
class (RA.ERIntApprox ra, RA.ERInnerOuterApprox ra, Ord ra) => (ERInnerOuterApproxElementary ra) 
    where
    absInner :: EffortIndex -> ra -> ra
    absInner ix = Prelude.abs
    minInner :: EffortIndex -> ra -> ra -> ra
    minInner ix = Prelude.min
    maxInner :: EffortIndex -> ra -> ra -> ra
    maxInner ix = Prelude.max
    sqrtInner :: EffortIndex -> ra -> ra
    sqrtInner = erSqrt_IR_Inner
    expInner :: EffortIndex -> ra -> ra
    expInner = erExp_IR_Inner
    logInner :: EffortIndex -> ra -> ra
    logInner = erLog_IR_Inner
    (**:) :: EffortIndex -> ra -> ra -> ra
    (**:) ix b e = expInner ix $ e *: (logInner ix b)
    sinInner :: EffortIndex -> ra -> ra
    sinInner = erSine_IR_Inner
    cosInner :: EffortIndex -> ra -> ra
    cosInner = erCosine_IR_Inner
    tanInner :: EffortIndex -> ra -> ra
    tanInner ix r = (sinInner ix r) /: (cosInner ix r) 
    atanInner :: EffortIndex -> ra -> ra
    atanInner = erATan_IR_Inner
    
    
    
    