
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
    evForm form
    where
    evForm form =
        case form of
          Not arg ->
              L.not $ evForm arg
          Or left right ->
              evForm left L.|| evForm right
          And left right ->
              evForm left L.&& evForm right
          Implies left right ->
              evForm left L.~> evForm right
          Le left right ->
              L.not $
              evTerm right `L.leq` evTerm left
          Leq left right ->
              evTerm left `L.leq` evTerm right          
          Ge left right ->
              L.not $
              evTerm left `L.leq` evTerm right
          Geq left right ->
              evTerm right `L.leq` evTerm left
          Eq left right ->
              (evTerm left `L.leq` evTerm right)
              L.&&
              (evTerm right `L.leq` evTerm left)
          Neq left right ->
              L.not $ 
              (evTerm left `L.leq` evTerm right)
              L.&&
              (evTerm right `L.leq` evTerm left)
          Ni left right -> 
              if RA.isBottom rightArg || RA.isBottom leftArg then
                  L.bot
              else 
                  rightArg `L.includes` leftArg
              where
              rightArg = evTerm right
              leftArg = evTerm left
    evTerm term =
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
          Hull left right ->
              evTerm left RA.\/ evTerm right
          EpsiAbs ->
              evTerm $
              (-EpsAbs) `Hull` EpsAbs
          EpsiRel ->
              evTerm $
              (-EpsRel) `Hull` EpsRel
          Round arg ->
              evTerm $
              ((1 + EpsiRel) * arg) + EpsiAbs 
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
