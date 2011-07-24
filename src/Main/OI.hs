
module Main.OI
(
  main
)
where

import Canvas
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.RnToRm.Approx as FA
import Numeric.ER.Real.Base.MachineDouble
import Numeric.ER.Real.DefaultRepr
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox.IntMap
import Main.OI.Plotter
import Main.OI.Prover
import qualified Data.Sequence as Q
import Main.OI.Form
import System.CPUTime
import System.Environment
--import Erf
--import Sqrt
import Teddy
--import LevelSlice 

main = do
    (vcnumberS : 
     radiusS : 
     midpointS : 
     effortS : 
     precisionS :
     maxdegreeS : 
     maxdepthS : 
     _) <- getArgs
    inittime <- getCPUTime
    initMachineDouble -- round upwards
    let vcnumber = read vcnumberS :: Int
        rad = read radiusS :: Double 
        midp = read midpointS :: Double 
        ix = read effortS -- :: Int
        prec = read precisionS -- :: Int
        maxdeg = read maxdegreeS :: Int
        maxdepth = read maxdepthS :: Int
        theorem = 
--          levelslice
          teddy
--          case vcnumber of 
--          {1 -> vc1; 2 -> vc2; 3 -> vc3; 4 -> vc4;
--           5 -> vc5; 6 -> vc6; 7 -> vc7}
        initbox = 
--          (if vcnumber <= 2 then DBox.delete 1 else id) $ 
          box rad midp
        initdepth = 0 in do
    putStr $ showBox initbox ++ "\n\n"
--    putStrLn $ "\nProve\n" ++ show thm ++ "\nfor" ++ showBox initbox ++ "\n\nSearching..."
--    writeFile ("results/erf-true-" ++ maxdegreeS ++ "-" ++ pwdepthS ++ ".gnuplot") $ 
--      "set term postscript eps enhanced\n" ++
--      "set output \"erf-true-" ++ maxdegreeS ++ "-" ++ pwdepthS ++ ".eps\"\n" ++
--      "set datafile missing 'null'\n" ++
--      "set title 'solving erf-true up to x using enclosure degree " ++ maxdegreeS ++ 
--      " and integration depth " ++ pwdepthS ++ "'\n" ++
----      "set key left top\n" ++
--      "set key left top\n" ++
--      "set key Left\n" ++
--      "#set key box linestyle 1\n" ++
--      "set xlabel 'x'\n" ++
--      "plot \"erf-true-" ++ maxdegreeS ++ "-" ++ pwdepthS ++ "\" using 1:2 with linespoints lw 4 title 'solving time',\\\n" ++
--      "     \"erf-true-" ++ maxdegreeS ++ "-" ++ pwdepthS ++ "\" using 1:3 with linespoints lw 4 title 'computed sub-problems'\n"
--    writeFile ("results/erf-true-" ++ maxdegreeS ++ "-" ++ pwdepthS) $ 
--      "proved up to | in seconds | computed boxes \n0 0 0\n"

    stateTV <- atomically $ newTVar []
    currtimeTV <- atomically $ newTVar inittime
    putStrLn "Searching..."
    tid <- myThreadId
    forkIO $ do
      canvas 
        inittime
        currtimeTV
        stateTV 
        (draw initbox) 
        500 
        500
      killThread tid
    plotter 
      maxdeg
      rad
      midp
      ix
      prec
      theorem
      intvarids
      (Q.singleton (0,initbox))
      currtimeTV
      stateTV
      inittime
      maxdepth

--    prover
--      maxdeg -- maximum bound degree
--      (24*3600) -- 24 hour timeout
--      rad -- domain width parameter
--      midp -- midpoint
--      ix
--      prec -- mantissa bit size (read precisionS)
--      theorem -- to be proved, defined in IntegralTest
--      intvarids -- variable IDs of integer variables, defined in IntegralTest
--      (Q.singleton (initdepth,initbox)) -- enqueue initial box
--      1 -- queue length
--      inittime -- inittime
--      0 -- prevtime
--      initdepth -- maxdepth
--      1 -- number computed boxes
--      (volume initbox) -- initial
--      0
