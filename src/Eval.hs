
module Eval where

import qualified Logic as L
import Form
import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.RnToRm.DefaultRepr
import qualified Numeric.ER.RnToRm.Approx as FA
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import qualified Numeric.ER.Real.Approx.Elementary as RAEL

evalForm maxdeg ix box prec form =
  case form of
      Not arg ->
          L.not $ evalForm maxdeg ix box prec arg 
      Or left right ->
          evalForm maxdeg ix box prec left L.|| evalForm maxdeg ix box prec right
      And left right ->
          evalForm maxdeg ix box prec left L.&& evalForm maxdeg ix box prec right
      Implies left right ->
          evalForm maxdeg ix box prec left L.~> evalForm maxdeg ix box prec right
      Le left right ->
          L.not $
          evalTerm maxdeg ix box prec right `L.leq` evalTerm maxdeg ix box prec left
      Leq left right ->
          evalTerm maxdeg ix box prec left `L.leq` evalTerm maxdeg ix box prec right          
      Ge left right ->
          L.not $
          evalTerm maxdeg ix box prec left `L.leq` evalTerm maxdeg ix box prec right
      Geq left right ->
          evalTerm maxdeg ix box prec right `L.leq` evalTerm maxdeg ix box prec left
      Eq left right ->
          (evalTerm maxdeg ix box prec left `L.leq` evalTerm maxdeg ix box prec right)
          L.&&
          (evalTerm maxdeg ix box prec right `L.leq` evalTerm maxdeg ix box prec left)
      Neq left right ->
          L.not $ 
          (evalTerm maxdeg ix box prec left `L.leq` evalTerm maxdeg ix box prec right)
          L.&&
          (evalTerm maxdeg ix box prec right `L.leq` evalTerm maxdeg ix box prec left)
      Ni left right -> 
          if RA.isBottom rightArg || RA.isBottom leftArg then
              L.bot
          else 
              rightArg `L.includes` leftArg
          where
          rightArg = 
              evalTerm maxdeg ix box prec right
          leftArg = 
              evalTerm maxdeg ix box prec left
        
evalTerm maxdeg ix box prec term =
--  let size = maxdeg * DBox.size box in 
  case term of
      EpsAbs ->
          FA.setMaxDegree maxdeg $
--          FA.setMaxSize size $
          fromRational $
--            2^^(-14) -- 16-bit
            2^^(-126) -- 32-bit
--            2^^(-1022) -- 64-bit
      EpsRel ->
          FA.setMaxDegree maxdeg $
--          FA.setMaxSize size $
          fromRational $
--            2^^(-9) -- 16-bit
          2^^(1-prec) -- custom float
--            2^^(-22)-- 32-bit
--            2^^(-51) -- 64-bit
      Pi ->
          FA.setMaxDegree maxdeg $
--          FA.setMaxSize size $
          RAEL.pi 10
      Lit val -> 
          FA.setMaxDegree maxdeg $
--          FA.setMaxSize size $
          fromRational $
          val :: FAPDOI BM
      Var varid ->
          case RA.isExact vardom of
              True -> -- domain of var thin, so var is a const
                  FA.setMaxDegree maxdeg $ 
--                  FA.setMaxSize size $
                  FA.const 
                      DBox.noinfo 
                      [vardom]
              False -> -- domain of var not thin, so safe to proj
                  FA.setMaxDegree maxdeg $
--                  FA.setMaxSize size $ 
                  FA.proj
                      (DBox.singleton varid vardom)
                      varid
              where                
              vardom = DBox.lookup "" varid box
      Plus left right ->
          evalTerm maxdeg ix box prec left + evalTerm maxdeg ix box prec right
      Minus left right ->
          evalTerm maxdeg ix box prec left - evalTerm maxdeg ix box prec right
      Neg arg ->
          - evalTerm maxdeg ix box prec arg
      Abs arg ->
        let argEncl = evalTerm maxdeg ix box prec arg in 
          case RA.leqReals 0 argEncl of
            Just True -> -- argument certainly non-negative
              argEncl    -- so do nothing
            _ -> -- otherwise
              RAEL.sqrt ix $ argEncl^2 -- do smooth approx of abs           
      Times left right ->
          evalTerm maxdeg ix box prec left * evalTerm maxdeg ix box prec right
      Square arg ->
          evalTerm maxdeg ix box prec arg
      Recip arg ->
          recip $ evalTerm maxdeg ix box prec arg
      Over left right ->
          evalTerm maxdeg ix box prec left / evalTerm maxdeg ix box prec right
      Sqrt arg ->
          RAEL.sqrt (fromInteger $ toInteger ix) $
            evalTerm maxdeg ix box prec arg
      Exp arg ->
          RAEL.exp 
                ix $ -- (fromInteger $ 3*(toInteger maxdeg)+10) $ 
                evalTerm maxdeg ix box prec arg                    
      Hull left right ->
          evalTerm maxdeg ix box prec left RA.\/ evalTerm maxdeg ix box prec right
      EpsiAbs ->
          evalTerm maxdeg ix box prec $
          (-EpsAbs) `Hull` EpsAbs
      EpsiRel ->
          evalTerm maxdeg ix box prec $
          (-EpsRel) `Hull` EpsRel
      Round arg ->
          evalTerm maxdeg ix box prec $
          ((1 + EpsiRel) * arg) + EpsiAbs 
      FPlus left right ->
          evalTerm maxdeg ix box prec $
          Round (left + right)
      FMinus left right ->
          evalTerm maxdeg ix box prec $
          Round (left - right)        
      FTimes left right ->
          evalTerm maxdeg ix box prec $
          Round (left * right)
      FSquare arg ->
          evalTerm maxdeg ix box prec $
          Round (Square arg)
      FSqrt arg ->
          evalTerm maxdeg ix box prec $
          Round $ (1+2*EpsiRel) * (Sqrt arg)
      FOver left right ->
          evalTerm maxdeg ix box prec $
          Round (left / right)
      FExp arg ->
          evalTerm maxdeg ix box prec $
          Round $ (1+4*EpsiRel) * (Exp arg)
