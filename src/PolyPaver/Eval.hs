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
import qualified Data.Ratio as Q


{-|
    Evaluate the truth value of a formula over a box.
    Also, compute a formula that is equivalent to the original formula over this box but possibly simpler.
-}
evalForm ::
    (L.TruthValue tv) =>
    Int {-^ polynomial degree limit -} -> 
    Int {-^ polynomial term size limit -} -> 
    EffortIndex {-^ effort index for regulating model error -} -> 
    IRA BM {-^ minIntegrationStepSize -} -> 
    PPBox BM {-^ domains of variables -} -> 
    (Int,Int) {-^ precision of emulated FP operations -} -> 
    Form {-^ form to evaluate -} -> 
    (tv, 
     Form) {-^ form with added range bounds in all terms -}
evalForm maxdeg maxsize ix minIntegrationStepSize ppb@(_, _, isIntVarMap, _) fptype form =
    evForm form
    where
    evTerm = evalTerm sampleTV maxdeg maxsize ix minIntegrationStepSize ppb fptype
    (sampleTV, _) = evForm Verum
    evForm form =
        case form of
            Verum -> (L.fromBool "Verum" ppb True, form)
            Falsum -> (L.fromBool "Falsum" ppb False, form)
            Not arg -> evOp1 Not L.not arg
            Or left right -> evOp2 Or (L.||) left right 
            And left right -> evOp2 And (L.&&) left right
            Implies left right -> evOp2 Implies (L.~>) left right
            Le lab left right -> evOpT2 False (Le lab) (\formWR -> L.less lab formWR ppb) left right 
            Leq lab left right -> evOpT2 False (Leq lab) (\formWR -> L.leq lab formWR ppb) left right
            Ge lab left right -> evOpT2 False (Le lab) (\formWR -> L.less lab formWR ppb) right left
            Geq lab left right -> evOpT2 False (Leq lab) (\formWR -> L.leq lab formWR ppb) right left
            Eq lab left right ->
                evForm $ And 
                    (Leq (lab ++ "<=") left right)
                    (Leq (lab ++ ">=") right left)
            Neq lab left right ->
                evForm $ Or 
                    (Le (lab ++ "<") left right)
                    (Le (lab ++ ">") right left)
            ContainedIn lab left right -> evOpT2 True (ContainedIn lab) (\formWR -> flip $ L.includes lab formWR ppb) left right 
            IsRange lab t lower upper -> 
                evForm $  (Leq (lab ++ "LO") lower t) /\ (Leq (lab ++ "HI") t upper)
            IsIntRange lab t lower upper -> 
                evForm $  (IsInt lab t) /\ (IsRange lab t lower upper)
            IsInt lab t -> (L.fromBool lab ppb $ termIsIntegerType t, form)
    evOp1 op opTV arg =
        (opTV argTV, op argWithRanges)
        where
        (argTV, argWithRanges) = evForm arg
    evOp2 op opTV left right =
        (opTV leftTV rightTV, op leftWithRanges rightWithRanges)
        where
        (leftTV, leftWithRanges) = evForm left
        (rightTV, rightWithRanges) = evForm right
    evOpT2 rightNeedsInnerRounding op opTV left right =
        (tv, formWithRanges)
        where
        tv 
--            | RA.isBottom rightVal || RA.isBottom leftVal = L.bot formWithRanges
            | otherwise = opTV formWithRanges leftVal rightVal 
        formWithRanges = op leftWithRanges rightWithRanges
        (leftVal, leftWithRanges) = evTerm False left 
        (rightVal, rightWithRanges) = evTerm rightNeedsInnerRounding right
    termIsIntegerType (Term (t, _)) =
        case t of
            Lit val -> Q.denominator val == 1
            Var varId _ -> case IMap.lookup varId isIntVarMap of Just res -> res; _ -> False
            Plus left right -> termIsIntegerType2 left right
            Minus left right -> termIsIntegerType2 left right
            Neg arg -> termIsIntegerType arg
            Abs arg -> termIsIntegerType arg
            Min left right -> termIsIntegerType2 left right
            Max left right -> termIsIntegerType2 left right
            Times left right -> termIsIntegerType2 left right
            Square arg -> termIsIntegerType arg
            _ -> False
    termIsIntegerType2 t1 t2 = 
        termIsIntegerType t1 && termIsIntegerType t2
            
     
evalTerm ::
    (L.TruthValue tv) =>
    tv {-^ sample truth value to aid type checking -} -> 
    Int {-^ polynomial degree limit -} -> 
    Int {-^ polynomial term size limit -} -> 
    EffortIndex {-^ effort index for regulating model error -} -> 
    IRA BM {-^ minIntegrationStepSize -} -> 
    PPBox BM {-^ domains of variables -} -> 
    (Int,Int) {-^ precision of emulated FP operations -} ->
    Bool {-^ should compute ranges using inner rounding? -} -> 
    Term {-^ term to evaluate -} -> 
    (FAPUOI BM, Term)
evalTerm sampleTV maxdeg maxsize ix minIntegrationStepSize ppbOrig fptype@(epsrelbits,epsabsbits) needInnerRounding term =
    evTerm term
    where
    evTerm = evTermBox ppbOrig
    evTermBox ppb term =
        (valueFA, Term (term', Just valueRA))
        where
        valueRA
            | needInnerRounding = ilRA RA.\/ ihRA
            | otherwise = valueRAOuter
        [ilRA] = FA.getRangeApprox il
        [ihRA] = FA.getRangeApprox ih
        [valueRAOuter] = FA.getRangeApprox valueFA
        ((ol, oh), (il, ih)) = RA.oiBounds valueFA
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
                Abs arg -> evOp1 Abs absOp arg
                    where
--                    absOp = RAEL.abs ix
                    absOp = setSizes . RAEL.abs ix . setSizes0
                Min left right -> evOp2 Min min left right
                Max left right -> evOp2 Max max left right
                Times left right -> evOp2 Times (*) left right
                Square arg -> evOp1 Square (\x -> x^2) arg
                Recip arg -> evOp1 Recip recip arg
                Over left right -> evOp2 Over divOp left right
                    where
                    divOp = (/)
--                    divOp l r =  l * (UFA.const (map recip $ FA.getRangeApprox r)) 
                        -- the above poor man's division is surprisingly 2x slower than the ordinary division
                        -- at least for erfriemann -d 1 
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
        setSizes0 = FA.setMaxDegree 0
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
--            unsafePrint
--            (
--                "evIntegral:"
--                ++ "\n term = " ++ showTerm (Term (term, Nothing))
--                ++ "\n ppb = " ++ show ppb
--                ++ "\n loRange = " ++ show loRange
--                ++ "\n hiRange = " ++ show hiRange
--                ++ "\n segments = " ++ show segments
--                ++ "\n primitiveFunctionLo = " ++ show primitiveFunctionLo
--                ++ "\n primitiveFunctionHi = " ++ show primitiveFunctionHi
--            ) 
--            $
            case RA.isExact integrationDom of
                False ->
                    case ivarId `Set.member` (getTermFreeVars integrand) of
                        True -> -- nonconstant integrand
                            (setSizes $ primitiveFunctionHi-primitiveFunctionLo,
                             termWithRanges)
--                            case 0 `RA.leqReals` integrandEnclosure of
--                                Just True -> 
--                                    (FA.setMaxDegree maxdeg $ primitiveFunctionHi-primitiveFunctionLo,
--                                     termWithRanges)
--                                _ ->
----                                    (UFA.bottomApprox, termWithRanges)
--                                    integrand
--                                    evTermBox' ppb $ (hi - lo) 
                        False -> -- constant integrand
                            evTermBox' ppb $ 
                                integrand * (hi - lo) -- this is symbolic arithmetic
                True -> -- integrating over measure zero set
                    (0, termWithRanges)
            where
            termWithRanges =
                Integral ivarId ivarName loWithRanges hiWithRanges integrandWithRangesLastSegment
            primitiveFunctionLo = 
    --            unsafePrintReturn "primitiveFunctionLo = " $
                composeBoundEnclosure primitiveFunctionFirstSegment loBoundEnclosureInUnit
            primitiveFunctionHi = 
    --            unsafePrintReturn "primitiveFunctionHi = " $
                composeBoundEnclosure primitiveFunctionLastSegment hiBoundEnclosureInUnit
            loBoundEnclosureInUnit = boundIntoUnit (head segments) loBoundEnclosure
            hiBoundEnclosureInUnit = boundIntoUnit (last segments) hiBoundEnclosure
            boundIntoUnit segment fn =
                (fn - constFA) * invslopeFA
                where
                constFA = setSizes $ UFA.const [constRA]
                invslopeFA = setSizes $ UFA.const [1/slopeRA]
                (constRA, slopeRA) = constSlopeFromRA $ RA.bounds segment
            composeBoundEnclosure primitiveFunction boundEnclosure =
                -- the following relies on the assumption that primitiveFunction is isotone 
                RA.fromOIBounds ((rol,roh), (ril, rih))
                where
                ((rol,_  ),(_  ,_  )) = RA.oiBounds $ composeThinBound ol 
                ((_  ,roh),(_  ,_  )) = RA.oiBounds $ composeThinBound oh 
                ((_  ,_  ),(ril,_  )) = RA.oiBounds $ composeThinBound il
                ((_  ,_  ),(_  ,rih)) = RA.oiBounds $ composeThinBound ih 
                ((ol,oh),(il,ih)) = RA.oiBounds boundEnclosure
                composeThinBound b =
                    UFA.composeWithThin primitiveFunction $ Map.fromList [(ivarId, b)]


            primitiveFunctionFirstSegment = head primitiveFunctionSegments
            primitiveFunctionLastSegment = last primitiveFunctionSegments
            primitiveFunctionSegments =
                integratePiecewise
                    0 -- ix
                    (zip integrandEnclosuresOverSegments segments)
                    ivarId
                    0 -- value of primitive function at the left endpoint
            
            integrandWithRangesLastSegment = last integrandWithRangesOverSegments
            (integrandEnclosuresOverSegments, integrandWithRangesOverSegments) =
                unzip $ map evaluateIntegrandOnSegment segments
            evaluateIntegrandOnSegment segment =
                evTermBox segmentPPB integrand
                where
                segmentPPB = 
--                    | skewed = error "Paralellepiped solving not yet supported for the integral operator."
                    (skewed, segmentBox, 
                     IMap.insert ivarId False isIntVarMap,
                     IMap.insert ivarId ivarName namesMap)
                segmentBox =
                    DBox.insert ivarId segmentAffine box
                segmentAffine = affine
                    where
                    [(_, affine)] = IMap.toList ivbox
                    (_, ivbox, _, _) =
                        ppBoxFromRAs isIntVarMap namesMap [(ivarId, RA.bounds segment)]
            segments 
                | loRangeIntersectsHiRange = [integrationDom]
                | otherwise =
                    loRangeIfNonempty ++
                    midSegments ++
                    hiRangeIfNonempty
                where
                loRangeIntersectsHiRange = not $ loRangeHi < hiRangeLo 
                midSegments = bisect (loRangeHi, hiRangeLo)
                hiRangeIfNonempty
                    | hiRangeLo < hiRangeHi = [hiRange]
                    | otherwise = []
                loRangeIfNonempty
                    | loRangeLo < loRangeHi = [loRange]
                    | otherwise = []
                (loRangeLo, loRangeHi) = RA.bounds loRange
                (hiRangeLo, hiRangeHi) = RA.bounds hiRange
                bisect (lo,hi) 
                    | (minIntegrationStepSize < hi - lo) = 
                        (bisect (lo, mid)) ++ 
                        (bisect (mid, hi))
                    | otherwise = [RA.fromBounds (lo, hi)]
                    where                    
                    mid = fst $ RA.bounds $ (hi + lo) / 2 
--                minIntegrationStepSize
--                    | useBounds > 0 = useBounds
----                    | useBox > 0 = useBox -- TODO
--                    | otherwise = useIx 
--                    where
--                    useBounds = snd $ RA.bounds $ max (loRangeHi - loRangeLo) (hiRangeHi - hiRangeLo)
--                    useIx = snd $ RA.bounds $ (hiRangeLo - loRangeHi) / (fromInteger $ toInteger ix) 
            
            integrationDom = loRange RA.\/ hiRange
            [loRange] = FA.getRangeApprox loBoundEnclosure
            [hiRange] = FA.getRangeApprox hiBoundEnclosure
            (loBoundEnclosure, loWithRanges) = evTermBox ppb lo
            (hiBoundEnclosure, hiWithRanges) = evTermBox ppb hi
            
            integratePiecewise ix integrandEnclosuresSegments ivarId fnAtLeftEndpoint =
                aux fnAtLeftEndpoint integrandEnclosuresSegments
                where
                aux _ [] = []
                aux fnInit ((integrandEnclosure, segment) : rest) =
--                    unsafePrint
--                    (
--                        "integratePiecewise: aux:"
--                        ++ "\n segment = " ++ show segment
--                        ++ "\n slopeRA = " ++ show slopeRA
--                        ++ "\n fnInit = " ++ show fnInit
--                        ++ "\n integrandEnclosure = " ++ show integrandEnclosure
--                        ++ "\n primitiveFunctionUFA = " ++ show primitiveFunctionUFA
--                        ++ "\n primitiveFunction = " ++ show primitiveFunction
--                        ++ "\n fnFinal = " ++ show fnFinal
--                    ) $
                    primitiveFunction : (aux fnFinal rest)
                    where
                    fnFinal = 
                        setSizes $ 
                        FA.partialEval substitution primitiveFunction
                        where
                        substitution = DBox.singleton ivarId (1) 
                    primitiveFunction =
                        fnInit +
                        ((UFA.const [slopeRA]) * primitiveFunctionUFA)
                    primitiveFunctionUFA =
                        UFA.integrate
                            ix
                            integrandEnclosure
                            ivarId
                            (-1) -- an integration start point
                            0 -- value of primitive function at the above start point
                    (_constRA, slopeRA) = constSlopeFromRA segmentBounds 
                    segmentBounds@(segmentLE, segmentRE) = RA.bounds segment


        
              