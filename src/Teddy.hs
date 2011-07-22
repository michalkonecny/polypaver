
module Teddy where

import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Approx.Elementary as RAEL
import qualified Numeric.ER.RnToRm.Approx as FA
import Numeric.ER.Real.Base.MachineDouble
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.RnToRm.DefaultRepr
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox.IntMap
import Form

box rad midp = 
    DBox.fromList 
        [
            (0, (-3) RA.\/ 3),
            (1, (-3) RA.\/ 3)
        ] 
        :: Box (IRA BM)

x = Var 0
y = Var 1

intvarids = [] 

teddy = Not $ 
  Or lefteye $
  Or righteye $
  Or nose $
  Or smile $
  Not $
  Or face $
  Or leftear rightear

face = 
  circle 0 0 2

leftear =
  (circle (-1.5) 1.5 0.8) `And` ((y - x) `Geq` 2.5)

rightear =
  (circle 1.5 1.5 0.8) `And` ((x + y) `Geq` 2.5)

lefteye =
  (circle (-1) 1 0.4) `And` (Not $ circle (-0.9) 0.9 0.05)

righteye =
  (circle 1 1 0.4) `And` (Not $ circle 0.9 0.9 0.05)

nose =
  (circle 0 0.3 0.4) `And` (y `Geq` 0.3)

smile =
  (circle 0 0 1) `And` (y `Leq` 0)

-- centre (a,b) radius r 
circle a b r =
  ((x-a)^2 + (y-b)^2) `Leq` (r^2)