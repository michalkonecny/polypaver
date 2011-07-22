
module LevelSlice where

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
            (0, (-1.5) RA.\/ 1.5),
            (1, (-1.5) RA.\/ 1.5)
        ] 
        :: Box (IRA BM)

x = Var 0
y = Var 1

intvarids = [] 

levelslice = 
  ((x^3+y^3-x-y-0.5) `Hull` (x^3+y^3-x-y+0.5)) `Ni` ((-1) `Hull` 1)
