{-# LANGUAGE DeriveDataTypeable #-}

{-|
    Module      :  PolyPaver.Solver
    Description :  the main counter-example search algorihtm 
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    The main counter-example search algorihtm.
-}
module PolyPaver.Solver where

import PolyPaver.Form
import PolyPaver.Eval
import qualified PolyPaver.Logic as L

import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Real.Approx.Interval
import Numeric.ER.Real.DefaultRepr
import qualified Data.IntMap as IMap
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox.IntMap
import qualified Data.Sequence as Q
import Numeric.ER.Misc

import Data.List
import Data.Maybe

import System.Console.CmdArgs
--import Control.Concurrent.STM
import System.CPUTime

data Order = 
    B | D
    deriving (Show,Data,Typeable)

data Report =
    NO | VOL
    deriving (Show,Data,Typeable)

data FPType =
    B32 | B64
    deriving (Show,Data,Typeable)


data Constants = Constants
    {maxdegree :: Int
    ,maxdepth :: Int
    ,effortindex :: Int
    ,maxseconds :: Int
    ,formula :: Form
    ,intvars :: [Int]
    ,initvolume :: IRA BM}

loop 
    order report fptype startdeg maxdeg bisections maxdep 
    ix maxtime prec form intvarids 
    queue 
    qlength inittime prevtime computedboxes 
    initvol 
    truevol
    =
    loopAux maxdep queue qlength prevtime computedboxes truevol startdeg
    where
    loopAux maxdep queue qlength prevtime computedboxes truevol currdeg
        | prevtime-inittime > maxtime*1000000000000 = do
            putStr $
              "\nTimeout.\nSearch aborted after " ++
              show maxtime ++
              " seconds.\nComputed : " ++ show computedboxes ++ 
              " boxes.\nReaching max depth : " ++ show maxdep ++ "\n\n"
        | Q.null queue = do
            currtime <- getCPUTime
            putStr $
              "\nSearch complete.\nTheorem proved true in " ++
              show ((fromInteger (currtime-inittime)) / 1000000000000) ++
              " seconds.\nComputed : " ++ show computedboxes ++ 
              " boxes.\nReaching max depth : " ++ show maxdep ++ "\n\n"
        | decided && decision = do -- formula true on this box
            currtime <- getCPUTime
            putStr reportTrueS
            loopAux
                maxdep 
                boxes (qlength-1) currtime (computedboxes+1) newtruevol startdeg
        | decided = do -- formula false on this box
            currtime <- getCPUTime
            putStr $
              "\nCounter example found, search aborted.\nTheorem proved false for " ++
              showBox box ++
              "\nSeconds elapsed : " ++
              show ((fromInteger (currtime-inittime)) / 1000000000000) ++
              "\nComputed  boxes : " ++ show computedboxes ++ 
              "\nMax depth : " ++ show maxdep ++  
              "\nDepth : " ++ show depth ++ "\n\n"
        | currdeg < maxdeg = do -- try raising the degree before splitting
            currtime <- getCPUTime
            loopAux
                maxdep 
                queue qlength currtime computedboxes truevol
                (currdeg + 1)  
        | depth >= bisections ||
          splitdom `RA.equalApprox` splitdomL || 
          splitdom `RA.equalApprox` splitdomR ||
          length thinvarids == dim = do -- formula undecided and cannot split any further
            currtime <- getCPUTime
            putStr $ 
              "\nCannot split box, search aborted.\nUndecided for : " ++
              showBox cbox ++
              "\nSeconds elapsed : " ++
              show ((fromInteger (currtime-inittime)) / 1000000000000) ++
              "\nComputed boxes : " ++ show computedboxes ++ 
--           "\nQueue length : " ++ show qlength ++
              "\nMaxdepth : " ++ show maxdep ++  
              "\nDepth : " ++ show depth ++ "\n\n"
        | otherwise = do -- formula undecided on this box, will split it
            currtime <- getCPUTime
--         putStr reportSplitS
            bisectAndRecur currtime
            {-
            loop 
                order report fptype maxdeg bisections (max (depth+1) maxdep) ix maxtime prec form intvarids  
                (boxes Q.|> (depth+1,boxL) Q.|> (depth+1,boxR)) 
                (qlength+1) inittime currtime (computedboxes+1) 
                initvol 
                truevol 
                breadth-first above depth-first below
            loop 
                order report fptype maxdeg bisections (max (depth+1) maxdep) ix maxtime prec form intvarids 
                ((depth+1,boxL) Q.<| (depth+1,boxR) Q.<| boxes) 
                (qlength+1) inittime currtime (computedboxes+1) 
                initvol 
                truevol 
            -}
        where
        bisectAndRecur currtime =
            case order of 
                B -> 
                    loopAux
                        (max (depth+1) maxdep) 
                        (boxes Q.|> (depth+1,boxL) Q.|> (depth+1,boxR)) 
                        (qlength+1) currtime (computedboxes+1) truevol startdeg
                D ->
                    loopAux 
                        (max (depth+1) maxdep) 
                        ((depth+1,boxL) Q.<| (depth+1,boxR) Q.<| boxes) 
                        (qlength+1) currtime (computedboxes+1) truevol startdeg
        reportTrueS =
            case report of
                 VOL -> 
                     "Proved fraction : " ++ show newtruevol ++ "\n"
                 NO -> 
                     ""
{-      reportSplitS =
            case report of
                 VOL -> 
                     "Proved fraction : " ++ show truevol ++ "\n"
                 NO -> 
                     ""-}
        (depth,box) = Q.index queue 0
        boxes = Q.drop 1 queue
        decided = isJust maybeValue
        decision = fromJust maybeValue
        dim = DBox.size box
        splitdom = DBox.lookup "looking up splitdom in solver" splitvar box
        splitdomL = DBox.lookup "looking up splitdom in solver" splitvar boxL
        splitdomR = DBox.lookup "looking up splitdom in solver" splitvar boxR
        (splitvar,(boxL,boxR)) = L.split thinvarids box value
        maybeValue = L.decide dim value
        value = 
            case fptype of
                 B32 -> evalForm currdeg ix cbox (23,-126) form :: Maybe Bool
                 B64 -> evalForm currdeg ix cbox (52,-1022) form :: Maybe Bool
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

--enqueueBox stateTV rgba box =
--    atomically $ do
--        state <- readTVar stateTV
--        writeTVar stateTV ((box,rgba) : state)

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

