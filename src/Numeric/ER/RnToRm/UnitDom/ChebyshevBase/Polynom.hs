{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom
    Description :  multivariate polynomials in the Chebyshev basis
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Arithmetic of multivariate polynomials 
    represented by their coefficients it the Chebyshev basis.
    
    The polynomials are never to be used outside the domain @[-1,1]^n@.
    
    All operations are rounded in such a way that the resulting polynomial
    is a /point-wise upper or lower bound/ of the exact result. 
-}
module Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom
(
    ERChebPoly(..), TermKey
)
where

import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Basic
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Eval
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Reduce
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Ring
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Derivative
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Bounds
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Enclosure
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.EnclosureInner
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Compose
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.ComposeInner
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Integration
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Derivative
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Division
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.DivisionInner
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Elementary
import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.ElementaryInner

import qualified Numeric.ER.RnToRm.UnitDom.Base as UFB
import qualified Numeric.ER.Real.Base as B
import Numeric.ER.Real.Approx.Interval
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainBoxMappable, DomainIntBox)
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes

import qualified Data.Map as Map

{- code for testing purpose, to be deleted later -}
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.BasicTypes.DomainBox.IntMap
type P = ERChebPoly (Box Int) B
x0 = chplVar 0 :: P
x1 = chplVar 1 :: P
x2 = chplVar 2 :: P
x3 = chplVar 3 :: P
x4 = chplVar 4 :: P
p1 = x1 *^ x1 *^ x1 +^ x1 *^ (x2 +^ (chplConst 2)) *^ (x3 -^ (chplConst 3))

e23 = enclRAConst (ERInterval 2 3)  :: (P,P)
e32 = enclRAConst (ERInterval 3 2)  :: (P,P)
em12 = enclRAConst (ERInterval (-1) 2)  :: (P,P)
e2m1 = enclRAConst (ERInterval 2 (-1))  :: (P,P)
ex0 = enclThin x0
ex0sq = enclMultiply 3 10 ex0 ex0
ep = enclAdd 3 10 (enclConst 2) (enclAdd 3 10 ex0 ex0sq) 

i23 = ienclRAConst (ERInterval 2 3)  :: ((P,P),Bool)
i32 = ienclRAConst (ERInterval 3 2)  :: ((P,P),Bool)
im12 = ienclRAConst (ERInterval (-1) 2)  :: ((P,P),Bool)
i2m1 = ienclRAConst (ERInterval 2 (-1))  :: ((P,P),Bool)
ix0 = ienclThin x0

{- end of code for testing purposes -}

instance 
    (B.ERRealBase rb, RealFrac rb,
     DomainBox box varid Int, Ord box, Show varid,
     DomainBoxMappable boxb boxras varid rb [ERInterval rb],
     DomainBoxMappable boxra boxras varid (ERInterval rb) [ERInterval rb],
     DomainIntBox boxra varid (ERInterval rb)) =>
    (UFB.ERUnitFnBase boxb boxra varid rb (ERInterval rb) (ERChebPoly box rb))
    where
    {----- Miscellaneous associated operations -----}
    raEndpoints _ (ERInterval l h) = (l,h)
    raFromEndpoints _ (l,h) = ERInterval l h
    compareApprox = chplCompareApprox
    showDiGrCmp = chplShow 
    
    {----- Structural analysis and update of functions -----}
    isValid = chplHasNoNaNOrInfty
    check = chplCheck
    getGranularity = chplGetGranularity
    setMinGranularity = chplSetMinGranularity
    setGranularity = chplSetGranularity
    getDegree = chplGetDegree
    reduceDegreeUp = chplReduceDegreeUp
    getSize = chplCountTerms
    reduceSizeUp = chplReduceTermCountUp
    getVariables = chplGetVars
    
    {----- Construction of basic functions -----}
    const = chplConst
    affine = chplAffine
    {----- Pointwise order operations ----------}
    getAffineUpperBound f =
        (const, Map.fromList varCoeffs)
        where
        fAff@(ERChebPoly coeffs) = chplReduceDegreeUp 1 f
        const = Map.findWithDefault 0 chplConstTermKey coeffs
        varCoeffs =
            getVarCoeffs $ Map.toList coeffs
        getVarCoeffs [] = []
        getVarCoeffs ((term, coeff):rest)
            | isLinearTerm = (var, coeff) : (getVarCoeffs rest)
            | otherwise = getVarCoeffs rest
            where
            (isLinearTerm, var) =
                case (DBox.toList term) of
                    [] -> (False, error "ERChebPoly: getAffineUpperBound: internal error - no variable") 
                    [(var, degree)] | degree == 1 -> (True, var)
                    _ -> error "ERChebPoly: getAffineUpperBound: internal error - term not linear"
    bounds = chplBounds  
    upperBound = chplUpperBound
    lowerBound = chplLowerBound
    upperBoundPrecise = chplUpperBoundExpensive
    lowerBoundPrecise = chplLowerBoundExpensive
    maxUp = chplMaxUp
    minUp = chplMinUp
    maxDown = chplMaxDn
    minDown = chplMinDn
    
    {----- Field operations ----------}
    neg = chplNeg
    addConstUp = chplAddConstUp
    scaleUp = chplScaleUp
    scaleApproxUp = chplScaleRAUp
    (+^) = (+^)
    (-^) = (-^)
    (*^) = (*^)
    recipUp md mt ix f = snd $ enclRecip md mt ix (md + 1) (chplNeg f, f)
    
    {----- Evaluation and composition of functions -----}
    evalUp pt f = chplEvalUp f pt
--    evalDown pt f = chplEvalDown f pt
    evalApprox x ufb = chplRAEval (\ b -> ERInterval b b) ufb x
    
    partialEvalApproxUp substitutions ufb =
        snd $ 
        chplPartialRAEval (UFB.raEndpoints ufb) ufb substitutions
    composeUp m n f v fv = snd $ enclCompose m n f v (enclThin fv) 
    composeManyUp m n f subst = snd $ enclComposeMany m n f (Map.map enclThin subst)
    composeDown m n f v fv = chplNeg $ fst $ enclCompose m n f v (enclThin fv) 
    composeManyDown m n f subst = chplNeg $ fst $ enclComposeMany m n f (Map.map enclThin subst)
    
    integrate = chplIntegrate
    differentiate var fb = chplDifferentiate fb var

instance
    (B.ERRealBase rb, RealFrac rb,
     DomainBox box varid Int, Ord box, Show varid,
     DomainBoxMappable boxb boxras varid rb [ERInterval rb],
     DomainBoxMappable boxra boxras varid (ERInterval rb) [ERInterval rb],
     DomainIntBox boxra varid (ERInterval rb)) =>
    (UFB.ERUnitFnBaseEncl boxb boxra varid rb (ERInterval rb) (ERChebPoly box rb))
    where
    boundsEncl = enclBounds
    constEncl (low, high) = (chplConst (-low), chplConst high)
    evalEncl pt encl = enclRAEval encl pt 
    evalEnclInner pt encl = enclRAEvalInner encl pt
    addConstEncl _ _ = enclAddConst
    scaleEncl = enclScale 
    addEncl = enclAdd
    multiplyEncl = enclMultiply
    recipEncl md mt ix = enclRecip md mt ix (md + 1)
    composeEncl = enclCompose
    composeManyEncls = enclComposeMany

instance
    (B.ERRealBase rb, RealFrac rb,
     DomainBox box varid Int, Ord box, Show varid,
     DomainBoxMappable boxb boxras varid rb [ERInterval rb],
     DomainBoxMappable boxra boxras varid (ERInterval rb) [ERInterval rb],
     DomainIntBox boxra varid (ERInterval rb)) =>
    (UFB.ERUnitFnBaseIEncl boxb boxra varid rb (ERInterval rb) (ERChebPoly box rb))
    where
    constIEncl (low, high) = ((chplConst (-low), chplConst high), low >= high)
    evalIEncl pt ie = ienclRAEval ie pt 
    addIEncl = ienclAdd
    multiplyIEncl = ienclMultiply
    recipIEnclPositive md mt ix = ienclRecipPositive md mt ix (md + 1)
    composeIEncl = ienclCompose
    composeManyIEncls = error "ERChebPoly: composeManyIEncls not yet" -- ienclComposeMany

instance 
    (B.ERRealBase rb, RealFrac rb,
     DomainBox box varid Int, Ord box, Show varid,
     DomainBoxMappable boxb boxras varid rb [ERInterval rb],
     DomainBoxMappable boxra boxras varid (ERInterval rb) [ERInterval rb],
     DomainIntBox boxra varid (ERInterval rb)) =>
    (UFB.ERUnitFnBaseElementary boxb boxra varid rb (ERInterval rb) (ERChebPoly box rb))
    where
    sqrtEncl md ms ix = enclSqrt md ms ix (effIx2int ix `div` 3)    
    expEncl = enclExp
    logEncl = enclLog
    sinEncl = enclSine
    cosEncl = enclCosine
    atanEncl = enclAtan

instance 
    (B.ERRealBase rb, RealFrac rb,
     DomainBox box varid Int, Ord box, Show varid,
     DomainBoxMappable boxb boxras varid rb [ERInterval rb],
     DomainBoxMappable boxra boxras varid (ERInterval rb) [ERInterval rb],
     DomainIntBox boxra varid (ERInterval rb)) =>
    (UFB.ERUnitFnBaseIElementary boxb boxra varid rb (ERInterval rb) (ERChebPoly box rb))
    where
    sqrtIEncl md ms ix = ienclSqrt md ms ix (effIx2int ix `div` 3)
--        error "ERChebPoly: sqrtIEncl not yet" 
    expIEncl md ms ix = ienclExp md ms ix 
--        error "ERChebPoly: expIEncl not yet" -- ienclExp
    logIEncl = error "ERChebPoly: logIEncl not yet" -- ienclLog
    sinIEncl = error "ERChebPoly: sinIEncl not yet" -- ienclSine
    cosIEncl = error "ERChebPoly: cosIEncl not yet" -- ienclCosine
    atanIEncl = error "ERChebPoly: atanIEncl not yet" -- ienclAtan
        