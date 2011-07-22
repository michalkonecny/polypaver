
module Sqrt where

import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Approx.Elementary as RAEL
import qualified Numeric.ER.RnToRm.Approx as FA
import Numeric.ER.Real.Base.MachineDouble
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.RnToRm.DefaultRepr
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox.IntMap
--import Plotter
--import Prover
import Form

box rad midp = 
    DBox.fromList 
        [
            (0, dom0)
--            ,(1, dom1)
--            (0, 0.5 RA.\/ 2)
            ,(1, 0 RA.\/ 3)
        ] 
        :: Box (IRA BM)
    where
    dom1 = RAEL.sqrt 15 dom0
    dom0 = 
--      (right-left) RA.\/ (right+left) 
      left RA.\/ right
    left = fromRational $ toRational rad :: IRA BM
    right = fromRational $ toRational midp 

x = Var 0
r = Var 1

intvarids = [] 

{-
    { x in dom0 /\ s in dom1 }
    prev := prev0
    curr := curr0
    while grd(x,prev,curr) do { inv(x,curr) }
      prev := curr
      curr := f(x,prev)
    od
    return curr
    { post(x,prev,curr) }
-}

prev0 = x

curr0 = 0.5*x+0.5

grd x prev curr = 
--  (Abs $ FMinus prev curr) `Ge` EpsAbs
  Neq prev curr

inF e = 
  ((-2^126) `Leq` e) `And` (e `Leq` (2^126)) 

f x r = FTimes 0.5 $ FPlus r $ FOver x r 

inv x curr = Ni curr $  Hull (-0.25*x^2+x) (0.25*x^2+1) -- x 1

post x prev curr = Ni curr $ (1+4*EpsiRel)*(Sqrt x)

{- into loop VC -}
vc1 =
  (grd x prev0 curr0) `Implies` (inv x curr0)

{- past loop VC -}
vc2 =
  (Not $ grd x prev0 curr0) `Implies` (post x prev0 curr0)

{- precondition check division -}
vc3 =
  (inv x r) `Implies` (inF $ FOver x r)

{- precondition check addition -}
vc4 =
  (And (inv x r) (inF $ FOver x r)) `Implies` (inF $ FPlus r $ FOver x r)

{- precondition check multiplication -}
vc5 =
  (And (And (inv x r) (inF $ FOver x r)) (inF $ FPlus r $ FOver x r)) 
  `Implies` (inF $ FTimes 0.5 $ FPlus r $ FOver x r)

{- round loop VC -}
vc6 =
  ((inv x r) `And` (grd x r (f x r))) `Implies` (inv x (f x r))

{- out of loop VC -}
vc7 = 
  ((inv x r) `And` (Not $ grd x r (f x r))) `Implies` (post x r (f x r))

