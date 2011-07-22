
module Main where

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
import System.Cmd 
import System.Environment
import Numeric.ER.RnToRm.UnitDom.Approx.IntervalOI
import qualified Numeric.ER.RnToRm.Approx.DomTransl as DT
import Numeric.ER.Misc
import qualified Numeric.ER.RnToRm.UnitDom.Base as UFB
import qualified TruthValue as TV
import qualified Numeric.ER.RnToRm.UnitDom.Approx as UFA
import qualified Data.Map as Map 


main = do
  initMachineDouble
  domxLS : -- left endpoint of variable x domain 
    domxUS : -- right endpoint of variable x domain
--      domyLS : -- left endpoint of variable y domain 
--        domyUS : -- right endpoint of variable y domain
          effortS :
            maxdegreeS : 
--          degreeS : -- reduce to this degree before plotting
            _ <- getArgs
  let domxL = read domxLS :: Double
      domxU = read domxUS :: Double
--      domyL = read domyLS :: Double
--      domyU = read domyUS :: Double
      ix = read effortS -- :: Int
      maxdeg = read maxdegreeS :: Int 
      deg = maxdeg -- read degreeS :: Int 
      domtrLHS = 
        FA.setMaxDegree deg $ 
          evalTerm maxdeg ix (box domxL domxU) domxL $
--    Enter the Terms to be plotted below
--         1/x
--        Sqrt (x+r)
--          (4*(x+1)) / ((x+1) * (x+5) - 4)
--          1/(((x+5)/4) - (1/(x+1)))
            FTimes 0.5 (FPlus r (FOver x r))
      domtrRHS = 
        FA.setMaxDegree deg $ 
          evalTerm maxdeg ix (box domxL domxU) domxL $
            (1+4*EpsiRel)*(Sqrt x)
--    Enter the Terms to be plotted above
      valueAtLeftEndpoint =
          FA.eval (box domxL domxL) domtrRHS
      valueAtRightEndpoint =
          FA.eval (box domxU domxU) domtrRHS
      uencl = DT.erfnUnitApprox domtrRHS
      (uoLn,uoU) = erfnoiOuter uencl
      ((uiLn,uiU),_) = erfnoiInner uencl
      domtrRHSmap = DT.erfnDomTransl domtrRHS 
      enclRHS = translateUfaToDom uencl domtrRHSmap
      domtrLHSmap = DT.erfnDomTransl domtrLHS 
      enclLHS = translateUfaToDom uencl domtrLHSmap
      dom = FA.dom domtrLHS
      dim = DBox.size dom
      (oLn,oU) = erfnoiOuter enclLHS
      ((iLn,iU),_) = erfnoiInner enclRHS
      (domyL,domyU) = RA.doubleBounds $ DBox.lookup "" 1 dom
      in do
--     print domtr
    putStrLn $ "\n\nouter value at left endpoint: " ++ (show valueAtLeftEndpoint)
    putStrLn $ "\n\nouter value at right endpoint: " ++ (show valueAtRightEndpoint)
    writeFile "out/gpscript" $
      "set title 'ix : " ++ effortS ++ 
      "  maxdeg : " ++ maxdegreeS ++
--       "  inner is consistent : " ++ show (UFB.neg iLn <= iU) ++ 
      " '\n" ++ 
      case dim of
        1 -> 
          "set key out\n"
          ++ "set xrange [" ++ show domxL ++ ":" ++ show domxU ++ "]\n" 
          ++ "set xlabel 'x'\n" 
          ++ "OuterHi(x) = " 
          ++ toGnuplotPoly (show oU) ++ "\n" 
          ++ "OuterLo(x) = " 
          ++ toGnuplotPoly (show $ UFB.neg oLn) ++ "\n"
--           ++ "InnerHi(x) = " 
--           ++ toGnuplotPoly (show iU) ++ "\n"
--           ++ "InnerLo(x) = " 
--           ++ toGnuplotPoly (show $ UFB.neg iLn) ++ "\n"
          ++ "plot OuterHi(x)"
--           ++ ", InnerHi(x)"
--           ++ ", InnerLo(x)"
          ++ ", OuterLo(x)"
--          ++ ", sqrt(x)"
--           ++ ", 1/x"
          ++ ", (1+0.5**7)*sqrt(x)"
--           ++ ", (4*(x+1)) / ((x+1) * (x+5) - 4)"
--           ++ ", 1/(((x+5)/4) - (1/(x+1)))"
        2 -> 
          "set key out\n"
          ++ "set hidden3d nooffset\n" 
          ++ "set xrange [" ++ show domxL ++ ":" ++ show domxU ++ "]\n" 
          ++ "set yrange [" ++ show domyL ++ ":" ++ show domyU ++ "]\n" 
          ++ "set xlabel 'x'\n" 
          ++ "set ylabel 'y'\n" 
          ++ "OuterLHSHi(x,y) = " 
          ++ toGnuplotPoly (show $ unsafePrintReturn "oU " oU) ++ "\n" 
          ++ "OuterLHSLo(x,y) = " 
          ++ toGnuplotPoly (show $ unsafePrintReturn "oL " $ UFB.neg oLn) ++ "\n" 
          ++ "InnerRHSHi(x,y) = " 
          ++ toGnuplotPoly (show $ unsafePrintReturn "iU " iU) ++ "\n"
          ++ "InnerRHSLo(x,y) = " 
          ++ toGnuplotPoly (show $ unsafePrintReturn "iL" $ UFB.neg iLn) ++ "\n"
          ++ "splot OuterLHSHi(x,y)"
          ++ ", InnerRHSHi(x,y)"
          ++ ", InnerRHSLo(x,y)"
          ++ ", OuterLHSLo(x,y)"
--          ++ ", sqrt(x+y)"
--          ++ ", (1+0.5**9)*(0.5+(1+0.5**9)*(y+(1+0.5**9)*(x/y)))"
--          ++ ", (1+0.5**7)*sqrt(x)"        
        _ -> error "Plotting only possible for 1 and 2 dimensional enclosures"
    putStrLn $
      (case UFB.compareReals 10 (UFB.neg uoLn) (UFB.neg uiLn) of
        Just LT -> "OuterLHSLo < InnerRHSLo : OK\n"
        Just EQ -> "OuterLHSLo == InnerRHSLo : OK\n"
        Just GT -> "OuterLHSLo > InnerRHSLo : ERROR\n"
        _ -> "Could not determine OI soundness for Lo bounds\n")
      ++
      (case UFB.compareReals 10 uiU uoU of
        Just LT -> "InnerRHSHi < OuterLHSHi : OK\n"
        Just EQ -> "InnerRHSHi == OuterLHSHi : OK\n"
        Just GT -> "InnerRHSHi > OuterLHSHi : ERROR\n"
        _ -> "Could not determine OI soundness for Hi bounds\n")
      ++
      (case UFB.compareReals 10 (UFB.neg uoLn) uoU of
        Just LT -> "OuterLHSLo < OuterLHSHi : OK"
        Just EQ -> "OuterLHSLo == OuterLHSHi : OK"
        Just GT -> "OuterLHSLo > OuterLHSHi : ERROR"
        _ -> "Could not determine consistency of outer bounds")
    system "gnuplot -persist out/gpscript"

toGnuplotPoly = handleSilentZeros . replace "x1" "y" . replace "^" "**"

handleSilentZeros s = if null s then "0" else s  

{- 
   replace taken from :
   http://bluebones.net/2007/01/replace-in-haskell/ 
-}
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace find repl s =
    if take (length find) s == find
        then repl ++ (replace find repl (drop (length find) s))
        else [head s] ++ (replace find repl (tail s))

-- from DT
translateUfaToDom ufa dtrB = -- this is unsafe, use only for printing!
    UFA.composeWithThin ufa $  
        Map.fromAscList $ 
            map mkToUnitUFA $ 
                 DBox.toAscList dtrB
    where
    mkToUnitUFA (var, tr) =
        (var, UFA.affine [co] (Map.singleton var [sl]))
        where
        sl = FA.domra2ranra ufa $ DT.dtrToUnitSlope tr
        co = FA.domra2ranra ufa $ DT.dtrToUnitConst tr


left ix maxdeg rad midp = 
  evalTerm maxdeg ix (box rad midp) rad $ 
    FTimes 0.5 $ FPlus r $ FOver x r

right ix maxdeg rad midp =
  evalTerm maxdeg ix (box rad midp) rad $ 
    (1+4*EpsiRel)*(Sqrt x)

{- visualisation of 2d enlcosures above -}

box rad midp = 
    DBox.fromList 
        [
            (0, dom0)
            ,(1, dom1)
--            (0, 0.5 RA.\/ 2)
--            ,(1, 0 RA.\/ 1)
        ] 
        :: Box (IRA BM)
    where
    dom1 = RAEL.sqrt 15 dom0
    dom0 = left RA.\/ right
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

f x r = FTimes 0.5 $ FPlus r $ FOver x r 

prev0 = 0

curr0 = x

grd x prev curr = Neq prev curr

inv x curr = Ni curr $ Hull x 1

post x prev curr = Ni curr $ (1+4*EpsiRel)*(Sqrt x)

{- past loop VC -}
vc1 =
  (Not $ grd x prev0 curr0) `Implies` (post x prev0 curr0)

{- into loop VC -}
vc2 =
  (grd x prev0 curr0) `Implies` (inv x curr0)
  
{- round loop VC -}
vc3 =
  ((inv x r) `And` (grd x r (f x r))) `Implies` (inv x (f x r))

{- out of loop VC -}
vc4 =
  ((inv x r) `And` (Not $ grd x r (f x r))) `Implies` (post x r (f x r))

vc5 = 
  (Ni r $ Hull x 1) `Implies` (inF $ FTimes 0.5 $ FPlus r $ FOver x r)

inF e = 
  ((-2^126) `Leq` e) `And` (e `Leq` (2^126)) 
