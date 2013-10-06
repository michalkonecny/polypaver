{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Eval
    Description :  (internal) evaluation of polynomials at a point
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Internal module for "Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom".
    
    Implementation of various evaluation functions related to polynomials.
-}
module Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Eval 
where

import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom.Basic

import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Base as B
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainBoxMappable, DomainIntBox)
import Numeric.ER.Real.Approx.Interval
import Numeric.ER.Misc

import qualified Data.Map as Map

{-|
    Evaluate a polynomial at a point, consistently rounding upwards and downwards. 
-}
chplEval ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box, 
     DomainBoxMappable boxb boxbb varid b [ERInterval b]) => 
    ERChebPoly box b ->
    boxb -> 
    (b, b)
chplEval (ERChebPoly coeffs)  vals =
    case resultRA of
        ERInterval low high -> (low, high)
    where
    resultRA =
        sum $ map evalTerm $ Map.toList coeffs
    evalTerm (term, c) =
        foldl (*) (ERInterval c c) $ map evalVar $ DBox.toList term
    evalVar (varID, degree) =
        (DBox.lookup "ERChebPoly.Eval: chplEval: " varID valsDegrees) !! degree
    valsDegrees =
        DBox.map (chebyEvalTs . \a->(ERInterval a a)) $ vals

chplEvalDown, chplEvalUp ::
    (B.ERRealBase b, DomainBox box varid Int, Ord box, 
     DomainBoxMappable boxb boxbb varid b [ERInterval b]) => 
    ERChebPoly box b ->
    boxb -> 
    b
chplEvalUp pt = snd . chplEval pt
chplEvalDown pt = fst . chplEval pt

chebyEvalTs ::
    (Num v) =>
    v -> [v]  
chebyEvalTs val =
    chebyIterate 1 val
    where
    chebyIterate tNm2 tNm1 =
        tNm2 : (chebyIterate tNm1 tN)
        where
        tN = 2 * val * tNm1 - tNm2  

{-|
    Evaluate a polynomial at a real number approximation 
-}
chplRAEval ::
    (B.ERRealBase b, RA.ERApprox ra, 
     DomainBox box varid Int, Ord box,
     DomainBoxMappable boxra boxras varid ra [ra], 
     DomainIntBox boxra varid ra) =>
    (b -> ra) -> 
    ERChebPoly box b ->
    boxra -> 
    ra
chplRAEval b2ra (ERChebPoly coeffs) vals =
    sum $ map evalTerm $ Map.toList coeffs
    where
    evalTerm (term, c) =
        (b2ra c) * (product $ map evalVar $ DBox.toList term)
    evalVar (varID, degree) =
        (DBox.lookup "ERChebPoly.Eval: chplEvalApprox: " varID valsDegrees) !! degree
    valsDegrees =
        DBox.map chebyEvalTs vals
        
{-|
    Substitute several variables in a polynomial with real number approximations,
    rounding downwards and upwards.
-}
chplPartialRAEval ::
    (B.ERRealBase b, RA.ERApprox ra, 
     DomainBox box varid Int, Ord box,
     DomainBoxMappable boxra boxras varid ra [ra],
     DomainIntBox boxra varid ra) =>
    (ra -> (b,b)) ->
    ERChebPoly box b ->
    boxra ->
    (ERChebPoly box b, ERChebPoly box b)
chplPartialRAEval ra2endpts (ERChebPoly coeffs) substitutions =
    (ERChebPoly $ Map.insertWith plusDown chplConstTermKey (- corr) coeffsSubstDown, 
     ERChebPoly $ Map.insertWith plusUp chplConstTermKey corr coeffsSubstUp)
    where
    (coeffsSubstDown, coeffsSubstUp, corr) =
        Map.foldWithKey processTerm (Map.empty, Map.empty, 0) coeffs
    processTerm termKey coeff (coeffsSubstDownPrev, coeffsSubstUpPrev, corrPrev) =
        (Map.insertWith plusDown newTermKey newCoeffDown coeffsSubstDownPrev,
         Map.insertWith plusUp newTermKey newCoeffUp coeffsSubstUpPrev,
         corrPrev + corrVars)
        where
        corrVars = (substValHi - substValLo) * coeff
        (newCoeffDown, newCoeffUp) 
            | coeff > 0 = (coeff `timesDown` substValLo, coeff `timesUp` substValHi)
            | coeff < 0 = (coeff `timesDown` substValHi, coeff `timesUp` substValLo)
            | otherwise = (0,0)
        (substValLo, substValHi) = ra2endpts substVal
        (substVal, newTermKey) =
            DBox.foldWithKey processVar (1, DBox.noinfo) termKey
        processVar varID degree (substValPrev, newTermKeyPrev) =
            case DBox.member varID substitutions of
                True -> 
                    (substValPrev * (evalVar varID degree), newTermKeyPrev)
                False ->
                    (substValPrev, DBox.insert varID degree newTermKeyPrev)
    evalVar varID degree =
        (DBox.lookup "ERChebPoly.Eval: chplPartialEvalApprox: " varID valsDegrees) !! degree
    valsDegrees =
        DBox.map chebyEvalTs substitutions
