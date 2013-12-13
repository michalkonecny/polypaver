{-# LANGUAGE FlexibleContexts #-}
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
    prepareForm,
    prepareTerm,
    evalForm,
    evalTerm,
    termIsIntegerType
)
where

import PolyPaver.Form
import PolyPaver.Subterms
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
import Data.Hashable (Hashable, hash)

import qualified Data.Strict.Tuple as SP

prepareForm ::
    (HasDefaultValue l, Eq l, Hashable l) =>
    Form l -> Form Int
prepareForm form =
    addHashesInForm $ expandRoundedOpsInForm form

prepareTerm ::
    (HasDefaultValue l, Eq l, Hashable l) =>
    Term l -> Term TermHash
prepareTerm term =
    addHashesInTerm $ expandRoundedOpsInTerm term

{-|
    Evaluate the truth value of a formula over a box.
    Also, compute a formula that is equivalent to the original formula over this box but possibly simpler.
-}
evalForm ::
    (L.TruthValue tv Int) =>
    Int {-^ polynomial degree limit -} -> 
    Int {-^ polynomial term size limit -} -> 
    EffortIndex {-^ effort index for regulating model error -} -> 
    IRA BM {-^ minIntegrationStepSize -} -> 
    PPBox BM {-^ domains of variables -} -> 
--    (Int,Int) {-^ precision of emulated FP operations -} -> 
    Form TermHash {-^ form to evaluate, with hashes in all sub-terms -} -> 
    (tv,
     (Form (Maybe (IRA BM)))) {-^ form with added range bounds in all terms -}
evalForm maxdeg maxsize ix minIntegrationStepSize ppb@(_, _, isIntVarMap, _) origForm =
    SP.snd $! evForm IMap.empty origForm
    where
    evTerm = evalTerm maxdeg maxsize ix minIntegrationStepSize ppb 
    evForm prevValuesMap form =
        case form of
            Not arg -> evOp1 Not L.not arg
            Or left right -> evOp2 Or (L.||) left right 
            And left right -> evOp2 And (L.&&) left right
            Implies left right -> evOp2 Implies (L.~>) left right
            Le lab left right -> evLess form lab left right 
            Leq lab left right -> evLeq form lab left right
            Ge lab left right -> evLess form lab right left
            Geq lab left right -> evLeq form lab right left
            Eq lab left right ->
                evForm prevValuesMap $ And 
                    (Leq (lab ++ "<=") left right)
                    (Leq (lab ++ ">=") right left)
            Neq lab left right ->
                evForm prevValuesMap $ Or
                    (Le (lab ++ "<") left right)
                    (Le (lab ++ ">") right left)
            ContainedIn lab left right -> 
                evOpT2 form True (ContainedIn lab) (\formWR -> flip $ L.includes lab formWR ppb) left right 
            IsRange lab t lower upper -> 
                evForm prevValuesMap $  (Leq (lab ++ "LO") lower t) /\ (Leq (lab ++ "HI") t upper)
            IsIntRange lab t lower upper -> 
                evForm prevValuesMap $  (IsInt lab t) /\ (IsRange lab t lower upper)
            IsInt lab t@(Term (_,l)) ->
                prevValuesMap SP.:!: 
                 ((L.fromBool lab l ppb $ termIsIntegerType isIntVarMap t),
                  (IsInt lab tWithRanges))
                where
                (_ SP.:!: (_tVal, tWithRanges)) = evTerm False prevValuesMap t
        where
        evOp1 op opTV arg =
            (newValuesMap SP.:!: (opTV argTV, op argWithRanges))
            where
            (newValuesMap SP.:!: (argTV, argWithRanges)) = evForm prevValuesMap arg
        evOp2 op opTV left right =
            (newValuesMap SP.:!: (opTV leftTV rightTV, op leftWithRanges rightWithRanges))
            where
            (intermValuesMap SP.:!: (leftTV, leftWithRanges)) = evForm prevValuesMap left
            (newValuesMap SP.:!: (rightTV, rightWithRanges)) = evForm intermValuesMap right
        evOpT2 form2 rightNeedsInnerRounding op opTV left right =
            (newValuesMap SP.:!: (tv, formWithRanges))
            where
            tv = opTV form2 leftVal rightVal 
            formWithRanges = op leftWithRanges rightWithRanges
            (intermValuesMap SP.:!: (leftVal, leftWithRanges)) = 
                evTerm False prevValuesMap left 
            (newValuesMap SP.:!: (rightVal, rightWithRanges)) = 
                evTerm rightNeedsInnerRounding intermValuesMap right
        evLess form2 = evLessLeq form2 False Le L.less
        evLeq form2 = evLessLeq form2 True Leq L.leq
        evLessLeq _ isLeq formOp _logicOp lab (Term (PlusInfinity, l)) (Term (PlusInfinity, _)) =
            (prevValuesMap SP.:!:
             (L.fromBool lab l ppb isLeq, 
              formOp lab plusInfinityTermWithRange plusInfinityTermWithRange))
        evLessLeq _ isLeq formOp _logicOp lab (Term (MinusInfinity, l)) (Term (MinusInfinity, _)) =
            (prevValuesMap SP.:!:
             (L.fromBool lab l ppb isLeq, 
              formOp lab minusInfinityTermWithRange minusInfinityTermWithRange))
        evLessLeq _ _isLeq formOp _logicOp lab (Term (MinusInfinity, l)) (Term (PlusInfinity, _)) =
            (prevValuesMap SP.:!:
             (L.fromBool lab l ppb True, 
              formOp lab minusInfinityTermWithRange plusInfinityTermWithRange))
        evLessLeq _ _isLeq formOp _logicOp lab (Term (PlusInfinity, l)) (Term (MinusInfinity, _)) =
            (prevValuesMap SP.:!:
             (L.fromBool lab l ppb False, 
              formOp lab plusInfinityTermWithRange minusInfinityTermWithRange))
        evLessLeq _ _isLeq formOp _logicOp lab (Term (MinusInfinity, l)) right =
            (prevValuesMap SP.:!:
             (L.fromBool lab l ppb True,
              formOp lab minusInfinityTermWithRange rightWithRanges))
            where
            (_ SP.:!: (_rightVal, rightWithRanges)) = evTerm False prevValuesMap right
        evLessLeq _ _isLeq formOp _logicOp lab left (Term (MinusInfinity, l)) =
            (prevValuesMap SP.:!:
             (L.fromBool lab l ppb False,
              formOp lab leftWithRanges minusInfinityTermWithRange))
            where
            (_ SP.:!: (_leftVal, leftWithRanges)) = evTerm False prevValuesMap left 
        evLessLeq _ _isLeq formOp _logicOp lab (Term (PlusInfinity, l)) right =
            (prevValuesMap SP.:!:
             (L.fromBool lab l ppb False,
              formOp lab plusInfinityTermWithRange rightWithRanges))
            where
            (_ SP.:!: (_rightVal, rightWithRanges)) = evTerm False prevValuesMap right
        evLessLeq _ _isLeq formOp _logicOp lab left (Term (PlusInfinity, l)) =
            (prevValuesMap SP.:!:
             (L.fromBool lab l ppb True,
              formOp lab leftWithRanges plusInfinityTermWithRange))
            where
            (_ SP.:!: (_leftVal, leftWithRanges)) = evTerm False prevValuesMap left
        evLessLeq form2 _ formOp logicOp lab left right =
            evOpT2 form2 False (formOp lab) (\formWR -> logicOp lab formWR ppb) left right
    plusInfinityTermWithRange = (Term (PlusInfinity, Just $ 1/0))
    minusInfinityTermWithRange = (Term (MinusInfinity, Just $ -1/0))
        

termIsIntegerType :: (IMap.IntMap Bool) -> Term l -> Bool
termIsIntegerType isIntVarMap (Term (t, _)) =
    case t of
        Lit val -> Q.denominator val == 1
        Var varId _ -> case IMap.lookup varId isIntVarMap of Just res -> res; _ -> False
        Plus left right -> termIsIntegerType2 left right
        Minus left right -> termIsIntegerType2 left right
        Neg arg -> termIsIntegerType isIntVarMap arg
        Abs arg -> termIsIntegerType isIntVarMap arg
        Min left right -> termIsIntegerType2 left right
        Max left right -> termIsIntegerType2 left right
        Times left right -> termIsIntegerType2 left right
        IntPower left right -> termIsIntegerType2 left right
        _ -> False
    where
    termIsIntegerType2 t1 t2 = 
        termIsIntegerType isIntVarMap t1 && termIsIntegerType isIntVarMap t2
            
     
evalTerm ::
    Int {-^ polynomial degree limit -} -> 
    Int {-^ polynomial term size limit -} -> 
    EffortIndex {-^ effort index for regulating model error -} -> 
    IRA BM {-^ minIntegrationStepSize -} -> 
    PPBox BM {-^ domains of variables -} -> 
--    (Int,Int) {-^ precision of emulated FP operations -} ->
    Bool {-^ should compute ranges using inner rounding? -} -> 
    (IMap.IntMap (FAPUOI BM, Term (Maybe (IRA BM)))) {-^ cache of memoised results -} ->
    Term Int {-^ term to evaluate, with hashes in all sub-terms -} -> 
    SP.Pair (IMap.IntMap (FAPUOI BM, Term (Maybe (IRA BM))))
     (FAPUOI BM, Term (Maybe (IRA BM)))
evalTerm 
        maxdeg maxsize ix minIntegrationStepSize ppbOrig -- fptype@(epsrelbits,epsabsbits) 
        needInnerRounding prevValuesMapOrig origTerm =
    evTermBox ppbOrig prevValuesMapOrig origTerm
    where
    evTermBox ppb prevValuesMap term@(Term (_, hashValue)) =
        -- check whether the result for this term has been memoised:
        case IMap.lookup hashValue prevValuesMap of
            Just memoisedResult -> (prevValuesMap SP.:!: memoisedResult) -- memoised, reuse!
            _ -> (newValuesMapWithResult SP.:!: result) -- not memoised, compute and memoise!
        where
        newValuesMapWithResult = IMap.insert hashValue result newValuesMap
        result = (valueFA, Term (term', Just valueRA))
        valueRA
            | needInnerRounding = ilRA RA.\/ ihRA
            | otherwise = valueRAOuter
        [ilRA] = FA.getRangeApprox il
        [ihRA] = FA.getRangeApprox ih
        [valueRAOuter] = FA.getRangeApprox valueFA
        ((_ol, _oh), (il, ih)) = RA.oiBounds valueFA
        (newValuesMap SP.:!: (valueFA, term')) = evTermBox' ppb prevValuesMap term
    evTermBox' ppb@(skewed, box, isIntVarMap, namesMap) prevValuesMap (Term (term', _)) =
            case term' of
                Pi -> (prevValuesMap SP.:!: (setSizes $ RAEL.pi 10, Pi))
                Lit val -> (prevValuesMap SP.:!: (rationalToFA val, Lit val))
                PlusInfinity ->
                    unsafePrint
                    ("Warning: Currently PolyPaver cannot prove a statement that has an infinity in a sub-expression.") 
                    (prevValuesMap SP.:!: (setSizes $ UFA.const [1/0], PlusInfinity))
                MinusInfinity -> 
                    unsafePrint
                    ("Warning: Currently PolyPaver cannot prove a statement that has an infinity in a sub-expression.") 
                    (prevValuesMap SP.:!: (setSizes $ UFA.const [-1/0], MinusInfinity))
                Var varid varName -> 
                    (prevValuesMap SP.:!: (fa, Var varid varName))
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
                Square arg -> evOp1 Square (\a -> intPowerOp a 2) arg
                IntPower left right -> evOp2 IntPower intPowerOp left right
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
                FEpsAbs epsrelbits epsabsbits -> 
                    (prevValuesMap SP.:!: (rationalToFA $ 2^^(- epsabsbits), FEpsAbs epsrelbits epsabsbits)) 
                FEpsRel epsrelbits epsabsbits -> 
                    (prevValuesMap SP.:!: (rationalToFA $ 2^^(- epsrelbits), FEpsRel epsrelbits epsabsbits))
                _ ->
                    error $ "Eval: evalTerm applied on a term with an unsupported operation: " ++ show term' 
        where
        setSizes :: FAPUOI BM -> FAPUOI BM  
        setSizes = FA.setMaxDegree maxdeg . FA.setMaxSize maxsize
        setSizes0 = FA.setMaxDegree 0
        rationalToFA = setSizes . fromRational
        evOp1 opT opFA arg =
            (newValuesMap SP.:!: (opFA argFA, opT argWithRanges))
            where
            (newValuesMap SP.:!: (argFA, argWithRanges)) = evTermBox ppb prevValuesMap arg 
        evOp2 opT opFA left right =
            (newValuesMap SP.:!: (opFA leftFA rightFA, opT leftWithRanges rightWithRanges))
            where
            (intermValuesMap SP.:!: (leftFA, leftWithRanges)) = evTermBox ppb prevValuesMap left
            (newValuesMap SP.:!: (rightFA, rightWithRanges)) = evTermBox ppb intermValuesMap right
        
        intPowerOp b e 
            | eL <= eR =
                (pwr eL) RA.\/ (pwr eR)
            | otherwise =
                error $ "Exponent of IntPower does not permit a non-negative integer value: " ++ show e
            where
            _ = [b,e]
            pwr n = b ^ n 
            eL = max 0 eLP
            (eLP, eR) = shrinkIntervalToIntegerBounds eRA
            [eRA] = FA.getRangeApprox e
            
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
                            (newValuesMap SP.:!:
                             (setSizes $ primitiveFunctionHi-primitiveFunctionLo,
                              termWithRanges2))
--                            case 0 `RA.leqReals` integrandEnclosure of
--                                Just True -> 
--                                    (FA.setMaxDegree maxdeg $ primitiveFunctionHi-primitiveFunctionLo,
--                                     termWithRanges)
--                                _ ->
----                                    (UFA.bottomApprox, termWithRanges)
--                                    integrand
--                                    evTermBox' ppb $ (hi - lo) 
                        False -> -- constant integrand
                            evTermBox' ppb prevValuesMap $ 
                                Term (integrandTimesWidth, hash integrandTimesWidth)
                                where
                                integrandTimesWidth = Times integrand (Term (hiMinusLo, hash hiMinusLo))
                                hiMinusLo = Minus hi lo
                True -> -- integrating over measure zero set
                    (newValuesMap SP.:!: (0, termWithRanges2))
            where
            termWithRanges2 =
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
                    (0 :: Int) -- ix
                    (zip integrandEnclosuresOverSegments segments)
                    ivarId
                    0 -- value of primitive function at the left endpoint
            
            integrandWithRangesLastSegment = last integrandWithRangesOverSegments
            (integrandEnclosuresOverSegments, integrandWithRangesOverSegments) =
                unzip $ map evaluateIntegrandOnSegment segments
            evaluateIntegrandOnSegment segment =
                SP.snd $ -- forget memoised results for integrand as its values are over a different box 
                    evTermBox segmentPPB IMap.empty integrand
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
                bisect (lo2,hi2) 
                    | (minIntegrationStepSize < hi2 - lo2) = 
                        (bisect (lo2, mid)) ++ 
                        (bisect (mid, hi2))
                    | otherwise = [RA.fromBounds (lo2, hi2)]
                    where                    
                    mid = fst $ RA.bounds $ (hi2 + lo2) / 2 
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
            (intermValuesMap SP.:!: (loBoundEnclosure, loWithRanges)) = evTermBox ppb prevValuesMap lo
            (newValuesMap SP.:!: (hiBoundEnclosure, hiWithRanges)) = evTermBox ppb intermValuesMap hi
            
            integratePiecewise _ix integrandEnclosuresSegments _ivarId fnAtLeftEndpoint =
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
                    segmentBounds@(_segmentLE, _segmentRE) = RA.bounds segment


        
{--- Expansion that eliminates rounded ops ---}



expandRoundedOpsInForm :: 
    (HasDefaultValue l, Eq l) =>
    Form l -> Form l
expandRoundedOpsInForm form =
    case form of
        Not arg -> expOp1 Not arg
        Or left right -> expOp2 Or left right 
        And left right -> expOp2 And left right
        Implies left right -> expOp2 Implies left right
        Le lab left right -> expT2 Le lab left right 
        Leq lab left right -> expT2 Leq lab left right
        Ge lab left right -> expT2 Ge lab right left
        Geq lab left right -> expT2 Geq lab right left
        Eq lab left right -> expT2 Eq lab left right
        Neq lab left right -> expT2 Neq lab left right
        ContainedIn lab left right -> expT2 ContainedIn lab left right
        IsRange lab t lower upper -> expT3 IsRange lab t lower upper
        IsIntRange lab t lower upper -> expT3 IsIntRange lab t lower upper 
        IsInt lab t -> expT1 IsInt lab t
    where
    expOp1 op arg = op (expandRoundedOpsInForm arg)
    expOp2 op arg1 arg2 = op (expandRoundedOpsInForm arg1) (expandRoundedOpsInForm arg2)
    expT1 op lab t = op lab (expandRoundedOpsInTerm t)
    expT2 op lab t1 t2 = op lab (expandRoundedOpsInTerm t1) (expandRoundedOpsInTerm t2)
    expT3 op lab t1 t2 t3 = op lab (expandRoundedOpsInTerm t1) (expandRoundedOpsInTerm t2) (expandRoundedOpsInTerm t3)

expandRoundedOpsInTerm :: 
    (HasDefaultValue l, Eq l) =>
    Term l -> Term l
expandRoundedOpsInTerm term@(Term (term', l)) =
    case term' of
        Hull t1 t2 -> expOp2 Hull t1 t2
        Plus t1 t2 -> expOp2 Plus t1 t2
        Minus t1 t2 -> expOp2 Minus t1 t2
        Neg t -> expOp1 Neg t
        Times t1 t2 -> expOp2 Times t1 t2
        Square t -> expOp1 Square t
        IntPower t1 t2 -> expOp2 IntPower t1 t2
        Recip t -> expOp1 Recip t
        Over t1 t2 -> expOp2 Over t1 t2
        Abs t -> expOp1 Abs t
        Min t1 t2 -> expOp2 Min t1 t2
        Max t1 t2 -> expOp2 Max t1 t2
        Sqrt t -> expOp1 Sqrt t
        Exp t -> expOp1 Exp t
        Sin t -> expOp1 Sin t
        Cos t -> expOp1 Cos t
        Atan t -> expOp1 Atan t
        Integral ivarId ivarName lower upper integrand ->
            expOp3 (Integral ivarId ivarName) lower upper integrand 
        FEpsiAbs rel abse -> fEpsiAbs rel abse 
        FEpsiRel rel abse -> fEpsiRel rel abse
        FRound rel abse arg -> fround2 rel abse (expandRoundedOpsInTerm arg)
        FPlus rel abse left right -> fround2 rel abse (leftDone + rightDone)
            where
            leftDone = expandRoundedOpsInTerm left
            rightDone = expandRoundedOpsInTerm right
        FMinus rel abse left right -> fround2 rel abse (leftDone - rightDone)        
            where
            leftDone = expandRoundedOpsInTerm left
            rightDone = expandRoundedOpsInTerm right
        FTimes rel abse left right -> fround2 rel abse (leftDone * rightDone)
            where
            leftDone = expandRoundedOpsInTerm left
            rightDone = expandRoundedOpsInTerm right
        FOver rel abse left right -> fround2 rel abse (leftDone / rightDone)
            where
            leftDone = expandRoundedOpsInTerm left
            rightDone = expandRoundedOpsInTerm right
        FSquare rel abse arg -> fround2 rel abse (square argDone)
            where
            argDone = expandRoundedOpsInTerm arg
        FSqrt rel abse arg -> fround2 rel abse $ (1+2*epsiRel) * (sqrt argDone)
            where
            epsiRel = fEpsiRel rel abse
            argDone = expandRoundedOpsInTerm arg
        FSin rel abse arg -> fround2 rel abse $ (1+2*epsiRel) * (sin argDone)
            where
            epsiRel = fEpsiRel rel abse
            argDone = expandRoundedOpsInTerm arg
        FCos rel abse arg -> fround2 rel abse $ (1+2*epsiRel) * (cos argDone)
            where
            epsiRel = fEpsiRel rel abse
            argDone = expandRoundedOpsInTerm arg
        FExp rel abse arg -> fround2 rel abse $ (1+4*epsiRel) * (exp argDone)
            where
            epsiRel = fEpsiRel rel abse
            argDone = expandRoundedOpsInTerm arg
        _ -> term
    where
    fEpsiRel rel abse = plusMinus $ termOp0 $ FEpsRel rel abse
    fEpsiAbs rel abse = plusMinus $ termOp0 $ FEpsAbs rel abse
    fround2 rel abse argDone = ((1 + epsiRel) * argDone) + epsiAbs
        where
        epsiRel = plusMinus $ termOp0 $ FEpsRel rel abse
        epsiAbs = plusMinus $ termOp0 $ FEpsAbs rel abse
    expOp1 op arg = 
        Term (op (expandRoundedOpsInTerm arg), l)
    expOp2 op arg1 arg2 = 
        Term (op (expandRoundedOpsInTerm arg1) (expandRoundedOpsInTerm arg2), l)
    expOp3 op arg1 arg2 arg3 = 
        Term (op (expandRoundedOpsInTerm arg1) (expandRoundedOpsInTerm arg2) (expandRoundedOpsInTerm arg3), l)
    