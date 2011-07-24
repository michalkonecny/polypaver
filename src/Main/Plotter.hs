
module OI.Plotter where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import Data.List
import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Real.Approx.Interval
import Numeric.ER.Real.DefaultRepr
import qualified Data.IntMap as IMap
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox.IntMap
import qualified Data.Sequence as Q
import Form
import qualified TruthValue as TV 
import Numeric.ER.Misc
import System.CPUTime

{-
    This bisection search loop enqueues decided true boxes 
    until the stack is empty or a false box is found.
-}
plotter maxdeg rad midp ix prec form intvarids queue currtimeTV stateTV inittime maxdepth
    | Q.null queue = do
        currtime <- getCPUTime
        updateCurrtime currtimeTV currtime
        putStrLn $ 
          "Search complete." -- \nVC proved true in "
          -- ++ show ((fromInteger (currtime-inittime)) / 1000000000000) ++
          -- " seconds."
    | decided && decision = do -- draw green box
        enqueueBox stateTV (0,1,0,0.5) box
        currtime <- getCPUTime
        updateCurrtime currtimeTV currtime
--        putStrLn "VC true over"
--        print box
--        print qlength
        plotter maxdeg rad midp ix prec form intvarids boxes currtimeTV stateTV inittime maxdepth
    | decided = do -- draw red box
        enqueueBox stateTV (1,0,0,0.7) box
        currtime <- getCPUTime
        updateCurrtime currtimeTV currtime
--        putStrLn "Counter example found, search aborted.\nVC false over"
--        print box
        plotter maxdeg rad midp ix prec form intvarids boxes currtimeTV stateTV inittime maxdepth
    | splitdom `RA.equalApprox` splitdomL || splitdom `RA.equalApprox` splitdomR ||
      length thinvarids == dim ||
      depth > maxdepth = do -- draw yellow box
        enqueueBox stateTV (1,1,0,0.5) box
        currtime <- getCPUTime
        updateCurrtime currtimeTV currtime
--        putStrLn "Cannot split thin box, search aborted.\nVC undecided over"
--        print cbox
        plotter maxdeg rad midp ix prec form intvarids boxes currtimeTV stateTV inittime maxdepth
    | otherwise = do -- draw transparent sub boxes
--        putStrLn "Split box"
--        print box
        mapM_ (enqueueBox stateTV (0,0,0,0)) [boxL,boxR]
        currtime <- getCPUTime
        updateCurrtime currtimeTV currtime
--        print qlength
{-
        plotter maxdeg rad midp ix prec form intvarids (boxes Q.|> (depth+1,boxL) Q.|> (depth+1,boxR)) currtimeTV stateTV inittime maxdepth
        breadth-first above depth-first below
-}
        plotter maxdeg rad midp ix prec form intvarids ((depth+1,boxL) Q.<| (depth+1,boxR) Q.<| boxes) currtimeTV stateTV inittime maxdepth
    where
    (depth,box) = Q.index queue 0
    boxes = Q.drop 1 queue
    decided = isJust maybeValue
    decision = fromJust maybeValue
    dim = DBox.size box
    splitdom = DBox.lookup "looking up splitdom in prover" splitvar box
    splitdomL = DBox.lookup "looking up splitdom in prover" splitvar boxL
    splitdomR = DBox.lookup "looking up splitdom in prover" splitvar boxR
    {-
        Maybe Bool Version Below
    -}
    (splitvar,(boxL,boxR)) = TV.split thinvarids box value
    maybeValue = TV.decide dim value
    value = evalForm maxdeg ix cbox prec form :: Maybe Bool
    {-
        Maybe Bool Version Above
        
        LocalGolbal Version Below
    -}
--    (splitvar,(boxL,boxR)) = TV.split thinvarids box tvlocal -- value
--    maybeValue = TV.decide dim tvglobal -- value
--    TV.LG tvlocal tvglobal = 
--        evalForm maxdeg ix cbox rad form :: TV.LocalGolbal (Integer,Integer) (Maybe Bool)
    {-
        LocalGolbal Version Above
    -}
    thinvarids = DBox.keys thincbox
    thincbox = DBox.filter RA.isExact cbox -- thin subbox of contracted box
    cbox = contractIntVarDoms box intvarids -- box with contracted integer doms

enqueueBox stateTV rgba box =
  atomically $ do
    state <- readTVar stateTV
    writeTVar stateTV ((box,rgba) : state)
        
updateCurrtime currtimeTV currtime = 
  atomically $ do
    writeTVar currtimeTV currtime 

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

