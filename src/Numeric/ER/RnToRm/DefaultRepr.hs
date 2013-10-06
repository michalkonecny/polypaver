{-|
    Module      :  Numeric.ER.Real.DefaultRepr
    Description :  concise names for default function representations
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable (requires fenv.h)

    This module supplies default instances for the real number and function classes
    described in "Numeric.ER.RnToRm".
    
    These classes form loosely coupled boundaries between abstraction layers.
    Nevertheless, we usually have particular implementations in mind, as shown here.
    
    To preserve the intended loose coupling, please use these definitions
    only in functions that cannot infer from their input or output data which type of function enclosures
    they should use.  Eg a function to add 1 to an enclosure should have the type:
    
    > add1 :: (ERFnApprox box varid domra ranra fa) => fa -> fa
    > add1 f = f + 1
    
    and /not/: @add1 :: FAPWP -> FAPWP@
    
-}
module Numeric.ER.RnToRm.DefaultRepr
(
    module Numeric.ER.RnToRm.DefaultRepr,
    module Numeric.ER.BasicTypes.DomainBox.IntMap
)
where

import Numeric.ER.Real.DefaultRepr
import qualified Numeric.ER.RnToRm.Approx as FA
import qualified Numeric.ER.RnToRm.UnitDom.Approx as UFA
import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Approx.Elementary as RAEL
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes

import Numeric.ER.BasicTypes.DomainBox.IntMap

import Numeric.ER.RnToRm.UnitDom.Base
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom
import Numeric.ER.RnToRm.UnitDom.Approx.Interval
import Numeric.ER.RnToRm.UnitDom.Approx.IntervalOI
import Numeric.ER.RnToRm.Approx.DomTransl
import Numeric.ER.RnToRm.Approx.DomEdges
import Numeric.ER.RnToRm.Approx.Tuple
import Numeric.ER.RnToRm.Approx.PieceWise
import Numeric.ER.RnToRm.Approx.Derivatives

--import BinaryDerive

import qualified Data.Map as Map

type P b = ERChebPoly (Box Int) b
type FAPU b = ERFnInterval (P b)
type FAPUOI b = ERFnIntervalOI (P b)

type FAPD b = ERFnDomTranslApprox (Box (DomTransl (IRA b))) VarID (FAPU b) (IRA b)
type FAPDOI b = ERFnDomTranslApprox (Box (DomTransl (IRA b))) VarID (FAPUOI b) (IRA b)

type FAPT b = ERFnTuple (FAPD b)
type FAPE b = ERFnDomEdges VarID (FAPT b)
type FAPWPD b = ERFnPiecewise (Box (IRA b)) VarID (IRA b) (FAPD b)
type FAPWPT b = ERFnPiecewise (Box (IRA b)) VarID (IRA b) (FAPT b)
type FAPWPE b = ERFnPiecewise (Box (IRA b)) VarID (IRA b) (FAPE b)
type FAPWP b = FAPWPE b

type FAPDDiff b = ERFnDerivatives VarID (FAPD b)
type FAPWPDiff b = ERFnDerivatives VarID (FAPWP b)

--type FA = FAPWL
type FA = FAPWP B

