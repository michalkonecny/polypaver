{-# LANGUAGE CPP #-}
-- #define USE_MPFR
{-|
    Module      :  Numeric.ER.Real.DefaultRepr
    Description :  concise names for default real representations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable (requires fenv.h)

    This module supplies default instances for the real number classes
    defined in "Numeric.ER.Real.Approx".
    
    These classes express loosely coupled abstraction layers.    
    To preserve the intended loose coupling, 
    please use these definitions only in functions that do not import or export
    any real numbers or real functions.
-}
module Numeric.ER.Real.DefaultRepr
(
    B, BM, BAP, BMAP, BR,
#ifdef USE_MPFR
    BMPFR,
#endif     
    RA, IRA
)
where

--import 

import Numeric.ER.Real.Base.Float
import Numeric.ER.Real.Base.Rational

import Numeric.ER.Real.Approx.Interval

--import Numeric.ER.Real.Base.BigFloatBase
import Numeric.ER.Real.Base.MachineDouble
import Numeric.ER.Real.Base.CombinedMachineAP

import Numeric.ER.Real.Base.MPFR

type BAP = ERFloat

{-| 
        Limited granularity, but sometimes up to 100x faster
        than ERFloat!
        
        !!! to be safe, one has to run 'initMachineDouble'
-}
type BM = Double

#ifdef USE_MPFR
type BMPFR = MPFR
#endif

{-|
        Use machine 'Double' while the granularity is up to its significant bit length
        and when the granularity grows beyond that, use 'ERFloat'.
        
        !!! to be safe, one has to run 'initMachineDouble'
-}
type BMAP = ERMachineAP BAP
 
--type BBF = BigFloat Prec50 -- seems incomplete on 25/Jun/2008 

{-| very inefficient -}
type BR = ExtendedRational 

{-| 
    the default base type
-}

#ifdef USE_MPFR
type B = BMPFR
--type B = BMAP
--type B = BAP
--type B = BM
--type B = BR
#else
type B = BMAP
--type B = BAP
--type B = BM
--type B = BR
#endif

{-| 
    the default instance of 'Numeric.ER.Real.Approx.ERApprox' 
-}
type RA b = ERInterval b

{-| 
    the default instance of 'Numeric.ER.Real.Approx.ERIntApprox' 
-}
type IRA b = ERInterval b

