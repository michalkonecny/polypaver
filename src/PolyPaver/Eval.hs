{-|
    Module      :  PolyPaver.Eval
    Description :  evaluation of a formula over a box  
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    Evaluation of a formula over a box.
-}

module PolyPaver.Eval 
(
    evalForm,
    evalTerm
)
where

import PolyPaver.Form
import PolyPaver.Vars
import PolyPaver.PPBox
import qualified PolyPaver.Logic as L

import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.RnToRm.DefaultRepr
import qualified Numeric.ER.RnToRm.Approx as FA
import qualified Numeric.ER.RnToRm.UnitDom.Approx as UFA
import qualified Numeric.ER.Real.Approx.Elementary as RAEL
import Numeric.ER.BasicTypes
import qualified Numeric.ER.BasicTypes.DomainBox as DBox 

import Numeric.ER.Misc

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IMap


{-|
    Evaluate the truth value of a formula over a box.
    Also, compute a formula that is equivalent to the original formula over this box but possibly simpler.
-}
evalForm ::
    (L.TruthValue tv) =>
    Int {-^ polynomial degree limit -} -> 
    Int {-^ polynomial term size limit -} -> 
    Int {-^ max split depth for ranges of integration variables -} -> 
    EffortIndex {-^ effort index for regulating model error -} -> 
    PPBox BM {-^ domains of variables -} -> 
    (Int,Int) {-^ precision of emulated FP operations -} -> 
    Form {-^ form to evaluate -} -> 
    (tv, 
     Form) {-^ form with added range bounds in all terms -}
evalForm maxdeg maxsize pwdepth ix ppb fptype form =
    evForm form
    where
    evTerm = evalTerm sampleTV maxdeg maxsize pwdepth ix ppb fptype
    (sampleTV, _) = evForm Verum
    evForm form =
        case form of
          Verum -> (L.fromBool ppb True, form)
          Falsum -> (L.fromBool ppb False, form)
          Predicate (Term (IsInt _, _)) -> evForm Verum -- this "predicate" is only a type declaration
          Predicate _ -> (L.bot Falsum, form) -- predicates not supported yet - therefore must remain undecided
          Not arg -> evOp1 Not L.not arg
          Or left right -> evOp2 Or (L.||) left right 
          And left right -> evOp2 And (L.&&) left right
          Implies left right -> evOp2 Implies (L.~>) left right
          Le lab left right -> evOpT2 (Le lab) (\formWR -> L.less lab formWR ppb) left right 
          Leq lab left right -> evOpT2 (Leq lab) (\formWR -> L.leq lab formWR ppb) left right
          Ge lab left right -> evOpT2 (Ge lab) (\formWR -> L.less lab formWR ppb) right left
          Geq lab left right -> evOpT2 (Leq lab) (\formWR -> L.leq lab formWR ppb) right left
          Eq lab left right ->
            evForm $ And 
                (Leq (lab ++ "<=") left right)
                (Leq (lab ++ ">=") right left)
          Neq lab left right ->
            evForm $ Or 
                (Le (lab ++ "<") left right)
                (Le (lab ++ ">") right left)
          Ni lab left right -> evOpT2 (Ni lab) (\formWR -> flip $ L.includes lab formWR ppb) left right 
    evOp1 op opTV arg =
        (opTV argTV, op argWithRanges)
        where
        (argTV, argWithRanges) = evForm arg
    evOp2 op opTV left right =
        (opTV leftTV rightTV, op leftWithRanges rightWithRanges)
        where
        (leftTV, leftWithRanges) = evForm left
        (rightTV, rightWithRanges) = evForm right
    evOpT2 op opTV left right =
        (tv, formWithRanges)
        where
        tv 
--            | RA.isBottom rightVal || RA.isBottom leftVal = L.bot formWithRanges
            | otherwise = opTV formWithRanges leftVal rightVal 
        formWithRanges = op leftWithRanges rightWithRanges
        (leftVal, leftWithRanges) = evTerm left
        (rightVal, rightWithRanges) = evTerm right

evalTerm ::
    (L.TruthValue tv) =>
    tv {-^ sample truth value to aid type checking -} -> 
    Int {-^ polynomial degree limit -} -> 
    Int {-^ polynomial term size limit -} -> 
    Int {-^ max split depth for ranges of integration variables -} -> 
    EffortIndex {-^ effort index for regulating model error -} -> 
    PPBox BM {-^ domains of variables -} -> 
    (Int,Int) {-^ precision of emulated FP operations -} -> 
    Term {-^ term to evaluate -} -> 
    (FAPUOI BM, Term)
evalTerm sampleTV maxdeg maxsize pwdepth ix ppbOrig fptype@(epsrelbits,epsabsbits) term =
    evTerm term
    where
    evTerm = evTermBox ppbOrig
    evTermBox ppb term =
        (valueFA, Term (term', Just valueRA))
        where
        [valueRA] = FA.getRangeApprox valueFA
        (valueFA, term') = evTermBox' ppb term
    evTermBox' ppb@(skewed, box, isIntVarMap, namesMap) (Term (term, _)) =
        (valueFA, termWithRanges)
        where
        (valueFA, termWithRanges) =
            case term of
                Pi -> (setSizes $ RAEL.pi 10, term)
                Lit val -> (rationalToFA val, term)
                Var varid varName -> (fa, term)
                    where
                    fa =
                        case isConst of
                            True -> -- domain of var thin, so var is a const
                                setSizes $ UFA.const [c]
                            False -> -- domain of var not thin, so safe to proj
                                setSizes $
                                case skewed of
                                    True -> 
                                        UFA.affine [c] 
                                            (Map.map (:[]) $ Map.filter nonZero coeffs)
                                    False ->
                                        UFA.affine [c] 
                                            (Map.singleton varid $ (\(Just cf) -> [cf]) $ Map.lookup varid coeffs)
                    (c, coeffs) = 
                        case IMap.lookup varid box of 
                            Just v -> v
                            Nothing -> 
                                error $ 
                                    "variable " ++ show varName ++ "(" ++ show varid 
                                    ++ ") not in box " ++ show box
                    isConst = ppCoeffsZero Nothing  coeffs
                    nonZero cf = cf `RA.equalReals` 0 /= Just True
                Plus left right -> evOp2 Plus (+) left right
                Minus left right -> evOp2 Minus (-) left right
                Neg arg -> evOp1 Neg negate arg
                Abs arg -> evOp1 Abs (RAEL.abs ix) arg
--            let argEncl = evTerm arg in 
--              case RA.leqReals 0 argEncl of
--                Just True -> -- argument certainly non-negative
--                  argEncl    -- so do nothing
--                _ -> -- otherwise
--                  RAEL.sqrt ix $ argEncl^2 -- do smooth approx of abs           
                Min left right -> evOp2 Min min left right
                Max left right -> evOp2 Max max left right
                Times left right -> evOp2 Times (*) left right
                Square arg -> evOp1 Square (\x -> x^2) arg
                Recip arg -> evOp1 Recip recip arg
                Over left right -> evOp2 Over (/) left right
--                (UFA.const (FA.getRangeApprox $ evTermBox ppb right))
                Sqrt arg -> evOp1 Sqrt (RAEL.sqrt (fromInteger $ toInteger ix)) arg
                Exp arg -> evOp1 Exp (RAEL.exp ix) arg
--                    ix $ -- (fromInteger $ 3*(toInteger maxdeg)+10) $ 
                Sin arg -> evOp1 Sin (RAEL.sin ix) arg
                Cos arg -> evOp1 Cos (RAEL.cos ix) arg
                Atan arg -> evOp1 Atan (RAEL.atan ix) arg
                Hull left right -> evOp2 Hull (RA.\/) left right
                Integral ivarId ivarName lower upper integrand ->
                    evIntegral ivarId ivarName lower upper integrand
                FEpsAbs -> (rationalToFA $ 2^^(- epsabsbits), term) 
                FEpsRel -> (rationalToFA $ 2^^(- epsrelbits), term) 
                FEpsiAbs -> evTermBox' ppb $ plusMinus fepsAbs
                FEpsiRel -> evTermBox' ppb $ plusMinus fepsRel
                FRound arg 
--              | epsabsShownIrrelevant -> -- TOOOOOOOO SLOW
--                  evTerm $
--                  (1 + EpsiRel) * arg
                    | otherwise ->
                        evTermBox' ppb $
                            ((1 + fepsiRel) * arg) + fepsiAbs
--              where
--              epsabsShownIrrelevant =
--                case (L.decide aboveEpsTV, L.decide belowEpsTV) of
--                    (Just True, _) -> True
--                    (_, Just True) -> True
--                    _ -> False 
--              _ = [aboveEpsTV, belowEpsTV, sampleTV]
--              aboveEpsTV = evForm $ Leq sampleLabel EpsAbs arg 
--              belowEpsTV = evForm $ Leq sampleLabel arg (Neg EpsAbs) 
                FPlus left right -> 
                    evTermBox' ppb $ fround (left + right)
                FMinus left right ->
                    evTermBox' ppb $ fround (left - right)        
                FTimes left right ->
                    evTermBox' ppb $ fround (left * right)
                FSquare arg ->
                    evTermBox' ppb $ fround (square arg)
                FSqrt arg ->
                    evTermBox' ppb $ fround $ (1+2*fepsiRel) * (sqrt arg)
                FOver left right ->
                    evTermBox' ppb $ fround (left / right)
                FExp arg ->
                    evTermBox' ppb $ fround $ (1+4*fepsiRel) * (exp arg)

        setSizes :: FAPUOI BM -> FAPUOI BM  
        setSizes = FA.setMaxDegree maxdeg . FA.setMaxSize maxsize
        rationalToFA = setSizes . fromRational
        evOp1 opT opFA arg =
            (opFA argFA, opT argWithRanges)
            where
            (argFA, argWithRanges) = evTermBox ppb arg 
        evOp2 opT opFA left right =
            (opFA leftFA rightFA, opT leftWithRanges rightWithRanges)
            where
            (leftFA, leftWithRanges) = evTermBox ppb left 
            (rightFA, rightWithRanges) = evTermBox ppb right
        
        evIntegral ivarId ivarName lo hi integrand =
    --        unsafePrint
    --        (
    --            "evIntegral:"
    --            ++ "\n lo = " ++ showTerm lo
    --            ++ "\n hi = " ++ showTerm hi
    --            ++ "\n ivarName = " ++ ivarName
    --            ++ "\n integrand = " ++ showTerm integrand
    --            ++ "\n integrationVarDom = " ++ show integrationVarDom
    --            ++ "\n getTermFreeVars integrand = " ++ show (getTermFreeVars integrand)
    --            ++ "\n primitiveFunctionHi = " ++ show primitiveFunctionHi
    --            ++ "\n primitiveFunctionLo = " ++ show primitiveFunctionLo
    --        ) 
    --        $
            case RA.isExact integrationVarDom of
                False ->
                    case ivarId `Set.member` (getTermFreeVars integrand) of
                        True -> -- nonconstant integrand
                            case 0 `RA.leqReals` integrandEnclosure of
                                Just True -> 
                                    (FA.setMaxDegree maxdeg $ primitiveFunctionHi-primitiveFunctionLo,
                                     termWithRanges)
                                _ ->
                                    (UFA.bottomApprox, termWithRanges)
                        False -> -- constant integrand
                            evTermBox' ppb $ 
                                integrand * (hi - lo) -- this is symbolic arithmetic
                True -> -- integrating over measure zero set
                    (0, termWithRanges)
            where
            termWithRanges =
                Integral ivarId ivarName loWithRanges hiWithRanges integrandWithRanges
            primitiveFunctionLo = 
    --            unsafePrintReturn "primitiveFunctionLo = " $
                composeBoundEnclosure loBoundEnclosure
            primitiveFunctionHi = 
    --            unsafePrintReturn "primitiveFunctionHi = " $
                composeBoundEnclosure hiBoundEnclosure
            composeBoundEnclosure boundEnclosure =
                -- the following relies on the assumption that primitiveFunction is isotone 
                RA.fromOIBounds ((rol,roh), (ril, rih))
                where
                ((rol,_  ),(_  ,_  )) = RA.oiBounds $ composeThinBound ol 
                ((_  ,roh),(_  ,_  )) = RA.oiBounds $ composeThinBound oh 
                ((_  ,_  ),(ril,_  )) = RA.oiBounds $ composeThinBound il
                ((_  ,_  ),(_  ,rih)) = RA.oiBounds $ composeThinBound ih 
                ((ol,oh),(il,ih)) = RA.oiBounds boundEnclosure
                composeThinBound b =
                    UFA.composeWithThin
                        primitiveFunction $
                        Map.fromList [(ivarId, boundIntoUnit b)] 
            primitiveFunction =
    --                   unsafePrintReturn "primitive function = " $
                (UFA.const [slopeRA]) * primitiveFunctionUFA
                where
                primitiveFunctionUFA =
                    UFA.integrate
                        ix
                        integrandEnclosure
                        ivarId
                        0 -- an integration start point
                        0 -- value of primitive function at the above start point
            ix = 0 -- 1 -- 5
            (integrandEnclosure, integrandWithRanges) =
    --                   unsafePrintReturn "integrand = " $ FA.setMaxDegree 0 $
                      evTermBox integrationPPB integrand
            integrationPPB 
                | skewed = error "Paralellopiped solving not yet supported for the integral operator."
                | otherwise = 
                    (skewed, integrationBox, 
                     IMap.insert ivarId False isIntVarMap,
                     IMap.insert ivarId ivarName namesMap)
                where
                integrationBox =
                    DBox.insert
                        ivarId
                        integrationVarDomAffine
                        box
            integrationVarDomAffine@(ivarDomAffineConst, ivarDomAffineCoeffs) = affine
                where
                [(_, affine)] = IMap.toList ivbox
                (_, ivbox, _, _) =
                    ppBoxFromRAs isIntVarMap namesMap [(ivarId, integrationVarDomBounds)]
            boundIntoUnit fn =
                (fn - constFA) * invslopeFA
                where
                constFA =
                          FA.setMaxDegree maxdeg $ 
                          FA.setMaxSize maxsize $
                          UFA.const [constRA]
                invslopeFA =
                          FA.setMaxDegree maxdeg $ 
                          FA.setMaxSize maxsize $
                          UFA.const [1/slopeRA]
            (constRA, slopeRA) = constSlopeFromRA integrationVarDomBounds
            integrationVarDom = loRange RA.\/ hiRange
            [loRange] = FA.getRangeApprox loBoundEnclosure
            [hiRange] = FA.getRangeApprox hiBoundEnclosure
            (loBoundEnclosure, loWithRanges) =
    --                   unsafePrintReturn "low bound = " $
                evTermBox ppb lo
            (hiBoundEnclosure, hiWithRanges) =
    --                   unsafePrintReturn "high bound = " $
                evTermBox ppb hi
            integrationVarDomBounds@(integrationVarDomLowBound,integrationVarDomHighBound) = 
                RA.bounds integrationVarDom
            integrationConstant = 0
        
              