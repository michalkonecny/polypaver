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
import PolyPaver.PPBox
import qualified PolyPaver.Logic as L

import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.RnToRm.DefaultRepr
import qualified Numeric.ER.RnToRm.Approx as FA
import qualified Numeric.ER.RnToRm.UnitDom.Approx as UFA
import qualified Numeric.ER.Real.Approx.Elementary as RAEL
import Numeric.ER.BasicTypes

import qualified Data.Map as Map
import qualified Data.IntMap as IMap

{-|
    Evaluate the truth value of a formula over a box.
    Also, compute a formula that is equivalent to the original formula over this box but possibly simpler.
-}
evalForm ::
    (L.TruthValue tv) =>
    Int -> Int -> EffortIndex -> PPBox BM -> (Int,Int) -> Form -> tv
evalForm maxdeg maxsize ix box fptype form =
    evForm form
    where
    evForm form =
        case form of
          Verum -> L.fromBool box True
          Falsum -> L.fromBool box False
          Not arg -> L.not $ evForm arg
          Or left right ->
               evForm left L.|| evForm right
          And left right ->
              evForm left L.&& evForm right
          Implies left right ->
              evForm left L.~> evForm right
          Le left right ->
              L.not $
              L.leq (Leq right left) box (evTerm right) (evTerm left)          
          Leq left right ->
              L.leq form box (evTerm left) (evTerm right)          
          Ge left right ->
              L.not $
              L.leq (Leq left right) box (evTerm left) (evTerm right)          
          Geq left right ->
              L.leq form box (evTerm right) (evTerm left)          
          Eq left right ->
              (L.leq (Leq left right) box (evTerm left) (evTerm right))          
              L.&&
              (L.leq (Leq right left) box (evTerm right) (evTerm left))          
          Neq left right ->
              L.not $ 
              (L.leq (Leq left right) box (evTerm left) (evTerm right))          
              L.&&
              (L.leq (Leq right left) box (evTerm right) (evTerm left))          
          Ni left right -> 
              if RA.isBottom rightArg || RA.isBottom leftArg then
                  L.bot form
              else 
                  L.includes form box rightArg leftArg
              where
              rightArg = evTerm right
              leftArg = evTerm left
    evTerm = evalTerm (evForm Verum) maxdeg maxsize ix box fptype

evalTerm ::
    (L.TruthValue tv) =>
    tv -> Int -> Int -> EffortIndex -> PPBox BM -> (Int,Int) -> Term -> FAPUOI BM
evalTerm sampleTV maxdeg maxsize ix box fptype@(epsrelbits,epsabsbits) term =
    evTerm term
    where
    evTerm term =
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
          Var varid _ ->
              case isConst of
                  True -> -- domain of var thin, so var is a const
                      FA.setMaxDegree maxdeg $ 
                      FA.setMaxSize maxsize $
                      UFA.const [c]
                  False -> -- domain of var not thin, so safe to proj
                      FA.setMaxDegree maxdeg $
                      FA.setMaxSize maxsize $ 
                      UFA.affine [c] (Map.map (:[]) $ Map.filter nonZero coeffs)
--                      UFA.affine [c] (Map.singleton varid $ (\(Just cf) -> [cf]) $ Map.lookup varid coeffs)
                  where
                  (c, coeffs) = case IMap.lookup varid box of Just v -> v
                  isConst = ppCoeffsZero coeffs
                  nonZero cf = cf `RA.equalReals` 0 /= Just True
          Plus left right ->
              evTerm left + evTerm right
          Minus left right ->
              evTerm left - evTerm right
          Neg arg ->
              - evTerm arg
          Abs arg ->
            RAEL.abs ix $ evTerm arg
--            let argEncl = evTerm arg in 
--              case RA.leqReals 0 argEncl of
--                Just True -> -- argument certainly non-negative
--                  argEncl    -- so do nothing
--                _ -> -- otherwise
--                  RAEL.sqrt ix $ argEncl^2 -- do smooth approx of abs           
--          Min left right ->
--              min (evTerm left) (evTerm right)
--          Max left right ->
--              max (evTerm left) (evTerm right)
          Times left right ->
              evTerm left * evTerm right
          Square arg ->
              evTerm arg
          Recip arg ->
              recip $ evTerm arg
          Over left right ->
              evTerm left / evTerm right
          Sqrt arg ->
              RAEL.sqrt (fromInteger $ toInteger ix) $
                evTerm arg
          Exp arg ->
              RAEL.exp 
                    ix $ -- (fromInteger $ 3*(toInteger maxdeg)+10) $ 
                    evTerm arg                    
          Sin arg ->
              RAEL.sin 
                    ix $ 
                    evTerm arg                    
          Cos arg ->
              RAEL.cos 
                    ix $ 
                    evTerm arg                    
          Atan arg ->
              RAEL.atan 
                    ix $ 
                    evTerm arg                    
          Hull left right ->
              evTerm left RA.\/ evTerm right
          EpsiAbs ->
              evTerm $
              (-EpsAbs) `Hull` EpsAbs
          EpsiRel ->
              evTerm $
              (-EpsRel) `Hull` EpsRel
          Round arg 
--              | epsabsShownIrrelevant -> -- TOOOOOOOO SLOW
--                  evTerm $
--                  (1 + EpsiRel) * arg
              | otherwise ->
                  evTerm $
                  ((1 + EpsiRel) * arg) + EpsiAbs
              where
              epsabsShownIrrelevant =
                case (L.decide 0 aboveEpsTV, L.decide 0 belowEpsTV) of
                    (Just True, _) -> True
                    (_, Just True) -> True
                    _ -> False 
              _ = [aboveEpsTV, belowEpsTV, sampleTV]
              aboveEpsTV = evForm $ Leq EpsAbs arg 
              belowEpsTV = evForm $ Leq arg (Neg EpsAbs) 
          FPlus left right ->
              evTerm $
              Round (left + right)
          FMinus left right ->
              evTerm $
              Round (left - right)        
          FTimes left right ->
              evTerm $
              Round (left * right)
          FSquare arg ->
              evTerm $
              Round (Square arg)
          FSqrt arg ->
              evTerm $
              Round $ (1+2*EpsiRel) * (Sqrt arg)
          FOver left right ->
              evTerm $
              Round (left / right)
          FExp arg ->
              evTerm $
              Round $ (1+4*EpsiRel) * (Exp arg)
    evForm = evalForm maxdeg maxsize ix box fptype
              