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
    tv
evalForm maxdeg maxsize pwdepth ix ppb fptype form =
    evForm form
    where
    evTerm = evalTerm sampleTV maxdeg maxsize pwdepth ix ppb fptype
    sampleTV = evForm Verum
    evForm form =
        case form of
          Verum -> L.fromBool ppb True
          Falsum -> L.fromBool ppb False
          Predicate (IsInt _) -> evForm Verum -- this "predicate" is only a type declaration
          Predicate _ -> L.bot Falsum -- predicates not supported yet - therefore must remain undecided
          Not arg -> L.not $ evForm arg
          Or left right ->
               evForm left L.|| evForm right
          And left right ->
              evForm left L.&& evForm right
          Implies left right ->
              evForm left L.~> evForm right
          Le lab left right ->
              L.not $
              L.leq (Leq lab right left) ppb (evTerm right) (evTerm left)          
          Leq _lab left right ->
              L.leq form ppb (evTerm left) (evTerm right)          
          Ge lab left right ->
              L.not $
              L.leq (Leq lab left right) ppb (evTerm left) (evTerm right)          
          Geq _lab left right ->
              L.leq form ppb (evTerm right) (evTerm left)          
          Eq lab left right ->
              (L.leq (Leq lab left right) ppb (evTerm left) (evTerm right))          
              L.&&
              (L.leq (Leq lab right left) ppb (evTerm right) (evTerm left))          
          Neq lab left right ->
              L.not $ 
              (L.leq (Leq lab left right) ppb (evTerm left) (evTerm right))          
              L.&&
              (L.leq (Leq lab right left) ppb (evTerm right) (evTerm left))          
          Ni _lab left right -> 
              if RA.isBottom rightArg || RA.isBottom leftArg then
                  L.bot form
              else 
                  L.includes form ppb rightArg leftArg
              where
              rightArg = evTerm right
              leftArg = evTerm left

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
    FAPUOI BM
evalTerm sampleTV maxdeg maxsize pwdepth ix ppbOrig fptype@(epsrelbits,epsabsbits) term =
    evTerm term
    where
    evTerm = evTermBox ppbOrig
    evTermBox ppb@(skewed, box, _, _) term =
      case term of
          EpsAbs ->
              FA.setMaxDegree maxdeg $
              FA.setMaxSize maxsize $
              fromRational $
              2^^(- epsabsbits)
          EpsRel ->
              FA.setMaxDegree maxdeg $
              FA.setMaxSize maxsize $
              fromRational $
              2^^(- epsrelbits)
          Pi ->
              FA.setMaxDegree maxdeg $
              FA.setMaxSize maxsize $
              RAEL.pi 10
          Lit val -> 
              FA.setMaxDegree maxdeg $
              FA.setMaxSize maxsize $
              fromRational $
              val :: FAPUOI BM
          Var varid varName ->
              case isConst of
                  True -> -- domain of var thin, so var is a const
                      FA.setMaxDegree maxdeg $ 
                      FA.setMaxSize maxsize $
                      UFA.const [c]
                  False -> -- domain of var not thin, so safe to proj
                      FA.setMaxDegree maxdeg $
                      FA.setMaxSize maxsize $
                      case skewed of
                          True -> 
                              UFA.affine [c] (Map.map (:[]) $ Map.filter nonZero coeffs)
                          False ->
                              UFA.affine [c] (Map.singleton varid $ (\(Just cf) -> [cf]) $ Map.lookup varid coeffs)
                  where
                  (c, coeffs) = 
                    case IMap.lookup varid box of 
                        Just v -> v
                        Nothing -> 
                            error $ 
                                "variable " ++ show varName ++ "(" ++ show varid 
                                ++ ") not in box " ++ show box
                  isConst = ppCoeffsZero Nothing  coeffs
                  nonZero cf = cf `RA.equalReals` 0 /= Just True
          Plus left right ->
              evTermBox ppb left + evTermBox ppb right
          Minus left right ->
              evTermBox ppb left - evTermBox ppb right
          Neg arg ->
              - evTermBox ppb arg
          Abs arg ->
            RAEL.abs ix $ evTermBox ppb arg
--            let argEncl = evTerm arg in 
--              case RA.leqReals 0 argEncl of
--                Just True -> -- argument certainly non-negative
--                  argEncl    -- so do nothing
--                _ -> -- otherwise
--                  RAEL.sqrt ix $ argEncl^2 -- do smooth approx of abs           
          Min left right ->
              min (evTermBox ppb left) (evTermBox ppb right)
          Max left right ->
              max (evTermBox ppb left) (evTermBox ppb right)
          Times left right ->
              evTermBox ppb left * evTermBox ppb right
          Square arg ->
              evTermBox ppb arg
          Recip arg ->
              recip $ evTermBox ppb arg
          Over left right ->
              evTermBox ppb left / 
                evTermBox ppb right
--                (UFA.const (FA.getRangeApprox $ evTermBox ppb right))
          Sqrt arg ->
              RAEL.sqrt (fromInteger $ toInteger ix) $
                evTermBox ppb arg
          Exp arg ->
              RAEL.exp 
                    ix $ -- (fromInteger $ 3*(toInteger maxdeg)+10) $ 
                    evTermBox ppb arg                    
          Sin arg ->
              RAEL.sin 
                    ix $ 
                    evTermBox ppb arg                    
          Cos arg ->
              RAEL.cos 
                    ix $ 
                    evTermBox ppb arg                    
          Atan arg ->
              RAEL.atan 
                    ix $ 
                    evTermBox ppb arg                    
          Hull left right ->
              evTermBox ppb left RA.\/ evTermBox ppb right
          Integral lower upper ivarId ivarName integrand ->
              evIntegral ppb lower upper ivarId ivarName integrand
          EpsiAbs ->
              evTermBox ppb $
              (-EpsAbs) `Hull` EpsAbs
          EpsiRel ->
              evTermBox ppb $
              (-EpsRel) `Hull` EpsRel
          Round arg 
--              | epsabsShownIrrelevant -> -- TOOOOOOOO SLOW
--                  evTerm $
--                  (1 + EpsiRel) * arg
              | otherwise ->
                  evTermBox ppb $
                  ((1 + EpsiRel) * arg) + EpsiAbs
--              where
--              epsabsShownIrrelevant =
--                case (L.decide 0 aboveEpsTV, L.decide 0 belowEpsTV) of
--                    (Just True, _) -> True
--                    (_, Just True) -> True
--                    _ -> False 
--              _ = [aboveEpsTV, belowEpsTV, sampleTV]
--              aboveEpsTV = evForm $ Leq sampleLabel EpsAbs arg 
--              belowEpsTV = evForm $ Leq sampleLabel arg (Neg EpsAbs) 
          FPlus left right ->
              evTermBox ppb $
              Round (left + right)
          FMinus left right ->
              evTermBox ppb $
              Round (left - right)        
          FTimes left right ->
              evTermBox ppb $
              Round (left * right)
          FSquare arg ->
              evTermBox ppb $
              Round (Square arg)
          FSqrt arg ->
              evTermBox ppb $
              Round $ (1+2*EpsiRel) * (Sqrt arg)
          FOver left right ->
              evTermBox ppb $
              Round (left / right)
          FExp arg ->
              evTermBox ppb $
              Round $ (1+4*EpsiRel) * (Exp arg)

    evIntegral ppb@(skewed, box, isIntVarMap, namesMap) lo hi ivarId ivarName integrand =
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
                                FA.setMaxDegree maxdeg $
                                primitiveFunctionHi-primitiveFunctionLo
                            _ ->
                                UFA.bottomApprox
                    False -> -- constant integrand
                        evTermBox ppb $ Times integrand (Minus hi lo)
            True -> -- integrating over measure zero set
                0
        where
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
        integrandEnclosure =
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
        [integrationVarDom] = 
--                   unsafePrintReturn "integration domain = " $
                  FA.getRangeApprox $  -- TRANSLATE BACK??!!
                      loBoundEnclosure
                      RA.\/ 
                      hiBoundEnclosure
        loBoundEnclosure =
--                   unsafePrintReturn "low bound = " $
            evTermBox ppb lo
        hiBoundEnclosure =
--                   unsafePrintReturn "high bound = " $
            evTermBox ppb hi
        integrationVarDomBounds@(integrationVarDomLowBound,integrationVarDomHighBound) = 
            RA.bounds integrationVarDom
        integrationConstant = 0
        
              