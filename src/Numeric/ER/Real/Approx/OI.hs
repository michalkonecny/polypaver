{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-|
    Module      :  Numeric.ER.Real.Approx.OI
    Description :  outer and inner approximations of approximations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    This module offers a transformation of a safely rounded real approximation type into
    a type that approximates these approximations from outside as well as *inside*. 
-}
module Numeric.ER.Real.Approx.OI where

import qualified Numeric.ER.Real.Approx as RA 

{-|
    A pair of approximations that form an "interval" in the lattice of
    approximations. 
    
    Eg outer = [1,4] inner = [3,2] can be thought of as the set of all
    generalised intervals where the left endpoint is between 1 and 3
    and the right endpoint is between 2 and 4 (eg [1,4], [3,4],
    [3,2], [3,3]).
-}
data ERApproxOI ra = 
    ERApproxOI
    {
        eroiOuter :: ra,
        eroiInner :: ra
    }
    deriving (Eq, Ord)

instance (RA.ERApprox ra) => (Show (ERApproxOI ra))
    where
    show (ERApproxOI oi ii) =
        "{ outer = " ++ show oi ++ "; inner = " ++ show ii ++ "}" 

instance (RA.ERApprox ra) => RA.ERApproxApprox (ERApproxOI ra)
    where
    safeIncludes (ERApproxOI oi1 ii1) (ERApproxOI oi2 ii2) =
        oi2 `RA.refines` ii1
    safeNotIncludes (ERApproxOI oi1 ii1) (ERApproxOI oi2 ii2) =
        not $ ii2 `RA.refines` oi1

{- TODO when required: -}    
instance (RA.ERApprox ra) => (Num (ERApproxOI ra))
instance (RA.ERApprox ra) => (Fractional (ERApproxOI ra))
instance (RA.ERApprox ra) => RA.ERApprox (ERApproxOI ra)
    where
    (ERApproxOI oi1 ii1) `leqReals` (ERApproxOI oi2 ii2) =
        oi1 `RA.leqReals` oi2

    