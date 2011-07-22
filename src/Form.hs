{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Form where

import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Approx.Elementary as RAEL
import qualified Numeric.ER.BasicTypes.DomainBox as DBox 
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.BasicTypes.DomainBox.IntMap
import qualified Numeric.ER.RnToRm.Approx as FA 
import qualified Numeric.ER.RnToRm.Approx.DomTransl as DT
import Numeric.ER.Real.Approx.Interval
import Numeric.ER.RnToRm.DefaultRepr
import Numeric.ER.RnToRm.UnitDom.Approx.Interval
import Numeric.ER.RnToRm.UnitDom.Approx.IntervalOI 

import qualified Data.Sequence as Q
import Data.Ratio
import qualified Data.Set as Set
import qualified TruthValue as TV

-- test related stuff below

import Numeric.ER.Misc

instance Num Term
  where
  fromInteger = Lit . fromInteger
  negate = Neg
  t1 + t2 = Plus t1 t2
  t1 * t2 = Times t1 t2

instance Fractional Term
  where
  fromRational = Lit
  recip = Recip

-- test related stuff above

-- new stuff below

showBox :: Box (IRA BM) -> String
showBox =
  DBox.foldWithKey 
    (\varid vardom boxStr -> "\nx" ++ 
     showSubscript varid ++ " in " ++ 
     show vardom ++ " " ++ boxStr) 
    ""

-- new stuff above

data Term
  = EpsAbs
  | EpsRel
  | Pi
  | Lit Rational
  | Var VarID
  | Plus Term Term
  | Minus Term Term
  | Neg Term
  | Abs Term
  | Times Term Term
  | Square Term
  | Recip Term
  | Over Term Term
  | Sqrt Term
  | Exp Term
  | Hull Term Term
  | EpsiAbs
  | EpsiRel
  | Round Term
  | FPlus Term Term
  | FMinus Term Term
  | FTimes Term Term
  | FOver Term Term
  | FSquare Term
  | FSqrt Term
  | FExp Term
  deriving (Eq,Show,Read) 

showSubscript i 
  | i < 10 = 
      showSubscriptDigit i
  | otherwise = 
      showSubscript (i `div` 10) ++ showSubscriptDigit (i `rem` 10)
  where
  showSubscriptDigit i =
    case i of
        0 -> "0" -- "₀" 
        1 -> "1" -- "₁" 
        2 -> "2" -- "₂" 
        3 -> "3" -- "₃" 
        4 -> "4" -- "₄" 
        5 -> "5" -- "₅" 
        6 -> "6" -- "₆" 
        7 -> "7" -- "₇" 
        8 -> "8" -- "₈" 
        9 -> "9" -- "₉"
        _ -> error "showSubscriptDigit: negative indices not allowed"

data Form
  = Not Form
  | Or Form Form
  | And Form Form
  | Implies Form Form
  | Le Term Term
  | Leq Term Term
  | Ge Term Term
  | Geq Term Term
  | Eq Term Term
  | Neq Term Term
  | Ni Term Term
  deriving (Eq,Show)

evalForm maxdeg ix box prec form =
  case form of
      Not arg ->
          TV.not $ evalForm maxdeg ix box prec arg 
      Or left right ->
          evalForm maxdeg ix box prec left TV.|| evalForm maxdeg ix box prec right
      And left right ->
          evalForm maxdeg ix box prec left TV.&& evalForm maxdeg ix box prec right
      Implies left right ->
          evalForm maxdeg ix box prec left TV.~> evalForm maxdeg ix box prec right
      Le left right ->
          TV.not $
          evalTerm maxdeg ix box prec right `TV.leq` evalTerm maxdeg ix box prec left
      Leq left right ->
          evalTerm maxdeg ix box prec left `TV.leq` evalTerm maxdeg ix box prec right          
      Ge left right ->
          TV.not $
          evalTerm maxdeg ix box prec left `TV.leq` evalTerm maxdeg ix box prec right
      Geq left right ->
          evalTerm maxdeg ix box prec right `TV.leq` evalTerm maxdeg ix box prec left
      Eq left right ->
          (evalTerm maxdeg ix box prec left `TV.leq` evalTerm maxdeg ix box prec right)
          TV.&&
          (evalTerm maxdeg ix box prec right `TV.leq` evalTerm maxdeg ix box prec left)
      Neq left right ->
          TV.not $ 
          (evalTerm maxdeg ix box prec left `TV.leq` evalTerm maxdeg ix box prec right)
          TV.&&
          (evalTerm maxdeg ix box prec right `TV.leq` evalTerm maxdeg ix box prec left)
      Ni left right -> 
          if RA.isBottom rightArg || RA.isBottom leftArg then
              TV.bot
          else 
              rightArg `TV.includes` leftArg
          where
          rightArg = 
--            unsafePrintReturn "RIGHT" $
              evalTerm maxdeg ix box prec right
          leftArg = 
--            unsafePrintReturn "LEFT" $
              evalTerm maxdeg ix box prec left
        
evalTerm maxdeg ix box prec term =
  case term of
      EpsAbs ->
          FA.setMaxDegree maxdeg $ fromRational $
--            2^^(-14) -- 16-bit
            2^^(-126) -- 32-bit
--            2^^(-1022) -- 64-bit
      EpsRel ->
          FA.setMaxDegree maxdeg $ fromRational $
--            2^^(-9) -- 16-bit
          2^^(1-prec) -- custom float
--            2^^(-22)-- 32-bit
--            2^^(-51) -- 64-bit
      Pi ->
          FA.setMaxDegree maxdeg $
          RAEL.pi 10
      Lit val -> 
          FA.setMaxDegree maxdeg $ 
          fromRational val :: FAPDOI BM
      Var varid ->
          case RA.isExact vardom of
              True -> -- domain of var thin, so var is a const
                  FA.setMaxDegree maxdeg $ 
                  FA.const 
                      DBox.noinfo 
                      [vardom]
              False -> -- domain of var not thin, so safe to proj
                  FA.setMaxDegree maxdeg $ 
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



