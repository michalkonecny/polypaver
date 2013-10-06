{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.DivisionInner
    Description :  (internal) division of polynomials
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Internal module for "Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom".
    
    Implementation of inner-rounded division 
    applied to basic polynomial enclosures.
-}
module Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.DivisionInner
where

import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Basic
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Reduce
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Eval
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Ring
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Bounds
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Enclosure
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.EnclosureInner
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Division

import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Approx.Elementary as RAEL
import qualified Numeric.ER.Real.Base as B
import Numeric.ER.Real.Approx.Interval
import Numeric.ER.Real.Arithmetic.Elementary
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainIntBox, DomainBoxMappable)
import Numeric.ER.BasicTypes
import Numeric.ER.Misc

import qualified Data.Map as Map

{-|
    Approximate the pointwise reciprocal of a positive polynomial 
    by another polynomial from below and from above
    using the tau method
    as described in [Mason & Handscomb 2003, p 62]. 
-}
ienclRecipPositive ::
    (B.ERRealBase b, 
     DomainBox box varid Int, Ord box, Show varid,
     DomainIntBox boxra varid (ERInterval b),
     DomainBoxMappable boxra boxras varid (ERInterval b) [ERInterval b]) => 
    Int {-^ maximum polynomial degree -} -> 
    Int {-^ maximum term count -} -> 
    EffortIndex {-^ minimum approx degree -} -> 
    Int {-^ degree of tau expansion -} -> 
    ((ERChebPoly box b, ERChebPoly box b), Bool) ->
    ((ERChebPoly box b, ERChebPoly box b), Bool)
ienclRecipPositive maxDegree maxSize ix tauDegr (e@(ln, h), isAC) = 
    ((hnRDown,lRDown), isAC)
    where
    hnRDown = chplNeg hRUp
    lRDown = chplNeg lnRUp
    (_, lnRUp) = enclRecip maxDegree maxSize ix tauDegr (chplNeg ln,ln)
    (_, hRUp) = enclRecip maxDegree maxSize ix tauDegr (chplNeg h,h)
    

--    | lDefinitelyPositive && hDefinitelyPositive =
--        ((hnRDown,lRDown), isAC)
--    | lDefinitelyNegative && hDefinitelyNegative =
--        ienclRecip maxDegree maxSize ix tauDegr ((h, ln), isAC)
--    | otherwise = 
--        error ""
--    where
--    lDefinitelyPositive = chplUpperBound ix ln < 0
--    hDefinitelyPositive = chplLowerBound ix h > 0
--    lDefinitelyNegative = chplLowerBound ix ln > 0
--    hDefinitelyNegative = chplUpperBound ix h < 0
    
