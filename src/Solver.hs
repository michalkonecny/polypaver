
module Solver where

import Control.Concurrent.STM
import Data.List
import Data.Maybe
import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Real.Approx.Interval
import Numeric.ER.Real.DefaultRepr
import qualified Data.IntMap as IMap
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox.IntMap
import qualified Data.Sequence as Q
import Form
import Eval
import qualified Logic as L
import Numeric.ER.Misc
import System.CPUTime

{-
    This bisection search loop postpends undecided boxes 
    until the stack is empty or a false box is found, i.e.     
    implements breadth-first search. 
-}
solver maxdeg maxtime rad midp ix prec form intvarids queue qlength inittime prevtime maxdepth computedboxes initvol truevol -- stateTV 
    | prevtime-inittime > maxtime*1000000000000 = do
        putStr $
          "\nTimeout.\nSearch aborted after " ++
          show maxtime ++
          " seconds.\nComputed : " ++ show computedboxes ++ 
          " boxes.\nReaching max depth : " ++ show maxdepth ++ "\n\n"
    | Q.null queue = do
        currtime <- getCPUTime
        putStr $
          "\nSearch complete.\nTheorem proved true in " ++
          show ((fromInteger (currtime-inittime)) / 1000000000000) ++
          " seconds.\nComputed : " ++ show computedboxes ++ 
          " boxes.\nReaching max depth : " ++ show maxdepth ++ "\n\n"
    | decided && decision = do -- draw green box
        currtime <- getCPUTime
----        enqueueBox stateTV (0,1,0,0.5) box
{-        appendFile 
          ("results/erf-true-" ++ show maxdeg ++ "-" ++ show pwdepth) $
          show (snd $ RA.doubleBounds $ DBox.lookup "" 0 box)
          ++ " " ++ show ((fromInteger (currtime-inittime)) / 1000000000000) 
          ++ " " ++ show computedboxes 
          ++ "\n"-}
{-
-}
--        putStr $ 
--          "\nTrue for " 
--          ++ showBox box 
--          ++ "\nSeconds elapsed : " ++
--          show ((fromInteger (currtime-inittime)) / 1000000000000)
--          ++ "\nProved fraction : " ++ show newtruevol ++ "\n"
--          ++ "\nQueue length : " ++ show qlength 
--          ++   "\nMaxdepth : " ++ show maxdepth ++ 
--          "\nDepth : " ++ show depth ++ "\n"
        solver maxdeg maxtime rad midp ix prec form intvarids boxes (qlength-1) inittime currtime maxdepth (computedboxes+1) initvol newtruevol  -- stateTV  
    | decided = do -- draw red box
        currtime <- getCPUTime
--        enqueueBox stateTV (1,0,0,0.7) box
        putStr $
          "\nCounter example found, search aborted.\nTheorem proved false for " ++
          showBox box ++
          "\nSeconds elapsed : " ++
          show ((fromInteger (currtime-inittime)) / 1000000000000) ++
          "\nComputed  boxes : " ++ show computedboxes ++ 
          "\nMax depth : " ++ show maxdepth ++  
          "\nDepth : " ++ show depth ++ "\n\n"
--        solver maxdeg form intvarids boxes -- stateTV
    | splitdom `RA.equalApprox` splitdomL || splitdom `RA.equalApprox` splitdomR ||
      length thinvarids == dim = do -- draw yellow box
        currtime <- getCPUTime
--        enqueueBox stateTV (1,1,0,0.5) box
        putStr $ 
          "\nCannot split box, search aborted.\nUndecided for : " ++
          showBox cbox ++
          "\nSeconds elapsed : " ++
          show ((fromInteger (currtime-inittime)) / 1000000000000) ++
          "\nComputed boxes : " ++ show computedboxes ++ 
--           "\nQueue length : " ++ show qlength ++
          "\nMaxdepth : " ++ show maxdepth ++  
          "\nDepth : " ++ show depth ++ "\n\n"
--        solver maxdeg form intvarids boxes -- stateTV
    | otherwise = do -- draw transparent sub boxes
        currtime <- getCPUTime
{-        
        putStr $
          "\nSplit domain of x" ++ showSubscript splitvar ++ " in " ++ showBox box 
--        mapM_ (enqueueBox stateTV (0,0,0,0)) [boxL,boxR]
          ++ "\nSeconds elapsed : "
          ++ show ((fromInteger (currtime-inittime)) / 1000000000000)
          ++ "\nQueue length : " ++ show qlength
          ++ "\nMaxdepth : " ++ show (max (depth+1) maxdepth)
          ++ "\nDepth : " ++ show depth ++ "\n"
-}
        {-
        solver maxdeg maxtime rad midp ix prec form intvarids (boxes Q.|> (depth+1,boxL) Q.|> (depth+1,boxR)) (qlength+1) inittime currtime (max (depth+1) maxdepth) (computedboxes+1) initvol truevol -- stateTV
            breadth-first above depth-first below
        -}
        solver maxdeg maxtime rad midp ix prec form intvarids ((depth+1,boxL) Q.<| (depth+1,boxR) Q.<| boxes) (qlength+1) inittime currtime (max (depth+1) maxdepth) (computedboxes+1) initvol truevol -- stateTV
    where
    (depth,box) = Q.index queue 0
    boxes = Q.drop 1 queue
    decided = isJust maybeValue
    decision = fromJust maybeValue
    dim = DBox.size box
    {-
        Maybe Bool Version Below
    -}
    splitdom = DBox.lookup "looking up splitdom in solver" splitvar box
    splitdomL = DBox.lookup "looking up splitdom in solver" splitvar boxL
    splitdomR = DBox.lookup "looking up splitdom in solver" splitvar boxR
    (splitvar,(boxL,boxR)) = L.split thinvarids box value
    maybeValue = L.decide dim value
    value = evalForm maxdeg ix cbox prec form :: Maybe Bool
    {-
        Maybe Bool Version Above
        
        LocalGolbal Version Below
    (splitvar,(boxL,boxR)) = L.split thinvarids box tvlocal -- value
    maybeValue = L.decide dim tvglobal -- value
    L.LG tvlocal tvglobal = 
        evalForm maxdeg ix cbox rad form :: L.LocalGolbal (Integer,Integer) (Maybe Bool)
    -}
    {-
        LocalGolbal Version Above
    -}
    thinvarids = DBox.keys thincbox
    thincbox = DBox.filter RA.isExact cbox -- thin subbox of contracted box
    cbox = contractIntVarDoms box intvarids -- box with contracted integer doms
    newtruevol = truevol+(volume box)/initvol

volume box =
  DBox.fold (\dom vol -> vol * width dom) 1 box

width dom =
  domR-domL
  where
  (domL,domR) = RA.bounds dom

enqueueBox stateTV rgba box =
    atomically $ do
        state <- readTVar stateTV
        writeTVar stateTV ((box,rgba) : state)

showBox :: Box (IRA BM) -> String
showBox =
  DBox.foldWithKey 
    (\varid vardom boxStr -> "\nx" ++ 
     show varid ++ " in " ++ 
     show vardom ++ " " ++ boxStr) 
    ""

contractIntVarDoms :: Box (IRA BM) -> [Int] -> Box (IRA BM)
contractIntVarDoms initbox intvarids =
    foldl'
        (
            \box intvarid 
            -> 
            IMap.adjust contractIntVarDom intvarid box
        )
        initbox
        intvarids

{-
    WARNING: uses floor and therefore
    RA.doubleBounds
-}
contractIntVarDom :: IRA BM -> IRA BM
contractIntVarDom dom
    | domLclg <= domRflr =
        ERInterval (fromInteger domLclg) (fromInteger domRflr)
    | otherwise =
        RA.bottomApprox
    where
--    (domLflr,domRclg) = RA.integerBounds dom
    domLclg = ceiling domL  
    domRflr = floor domR
    domL = erintv_left dom
    domR = erintv_right dom

