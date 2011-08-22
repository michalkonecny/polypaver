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
import PolyPaver.PPBox
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


--data Constants = Constants
--    {maxdegree :: Int
--    ,maxdepth :: Int
--    ,effortindex :: Int
--    ,maxseconds :: Int
--    ,formula :: Form
--    ,intvars :: [Int]
--    ,initvolume :: IRA BM}

loop 
    order report fptype noBoxSkewing
    origstartdeg maxdeg improvementRatioThreshold 
    maxsize
    mindepth maxdepth maxDepthReached
    ix maxtime prec form 
--    intvarids 
    queue 
    qlength inittime prevtime computedboxes 
    problemvol
    truevol
    =
    loopAux 
        form maxDepthReached 
        queue qlength prevtime 
        computedboxes problemvol truevol 
        Nothing Nothing
    where
    loopAux 
        form maxDepthReached 
        queue qlength prevtime 
        computedboxes problemvol truevol 
        maybeCurrdeg maybePrevMeasure
        =
        do
        if (qlength > 0) then reportBox else return ()
        trySolvingBox
        where
        trySolvingBox
            | Q.null queue = 
                do
                currtime <- getCPUTime
                putStr $
                  "\nSearch complete.\nTheorem proved true in " ++
                  show ((fromInteger (currtime-inittime)) / 1000000000000) ++
                  " seconds.\nComputed : " ++ show computedboxes ++ 
                  " boxes.\nReaching max depth : " ++ show maxDepthReached ++ "\n\n"
            | depth < mindepth = 
                do
                putStrLn $ "initial splitting at depth " ++ show depth
                currtime <- getCPUTime
    --         putStr reportSplitS
                bisectAndRecur form currtime
            | prevtime-inittime > maxtime*1000000000000 = 
                do
                putStr $
                  "\nTimeout.\nSearch aborted after " ++
                  show maxtime ++
                  " seconds.\nComputed : " ++ show computedboxes ++ 
                  " boxes.\nReaching max depth : " ++ show maxDepthReached ++ "\n\n"
            | decided && decision = -- formula true on this box
                do
                currtime <- getCPUTime
                reportVolume
                loopAux
                    form maxDepthReached 
                    boxes (qlength-1) currtime
                    (computedboxes+1) problemvol newtruevol 
                    Nothing Nothing
            | decided = -- formula false on this box
                do
                currtime <- getCPUTime
                putStr $
                  "\nCounter example found, search aborted.\nTheorem proved false for " ++
                  ppShow box ++
                  "\nSeconds elapsed : " ++
                  show ((fromInteger (currtime-inittime)) / 1000000000000) ++
                  "\nComputed  boxes : " ++ show computedboxes ++ 
                  "\nMax depth : " ++ show maxDepthReached ++  
                  "\nDepth : " ++ show depth ++ "\n\n"
            | currdeg < maxdeg && undecidedMeasureImproved = -- try raising the degree before splitting
                do
                putStrLn $ "raising degree to " ++ show (currdeg + 1)
                currtime <- getCPUTime
                loopAux
                    form maxDepthReached 
                    queue qlength currtime 
                    computedboxes problemvol truevol
                    (Just $ currdeg + 1)
                    (Just undecidedMeasure)
            | depth >= maxdepth || 
              not splitSuccess ||
              length thinvarids == dim = -- cannot split any further
                do
                currtime <- getCPUTime
                putStr $ 
                  "\nCannot split box, search aborted.\nUndecided for : " ++
                  ppShow box ++
                  "\nSeconds elapsed : " ++
                  show ((fromInteger (currtime-inittime)) / 1000000000000) ++
                  "\nComputed boxes : " ++ show computedboxes ++ 
    --           "\nQueue length : " ++ show qlength ++
                  "\nMaxdepth : " ++ show maxDepthReached ++  
                  "\nDepth : " ++ show depth ++ "\n\n"
            | otherwise = -- formula undecided on this box, will split it
                do
                currtime <- getCPUTime
                reportSplit
                bisectAndRecur undecidedSimplerForm currtime

        (depth, startdeg, box) = Q.index queue 0
        dim = DBox.size box
        boxes = Q.drop 1 queue

        bisectAndRecur form currtime =
            case order of 
                B -> 
                    loopAux
                        form (max (depth+1) maxDepthReached) 
                        (boxes Q.|> (depth+1,newstartdeg,boxL) Q.|> (depth+1,newstartdeg,boxR)) 
                        (qlength+1) currtime 
                        (computedboxes+1) newproblemvol truevol 
                        Nothing Nothing
                D ->
                    loopAux 
                        form (max (depth+1) maxDepthReached) 
                        ((depth+1,newstartdeg,boxL) Q.<| (depth+1,newstartdeg,boxR) Q.<| boxes) 
                        (qlength+1) currtime 
                        (computedboxes+1) newproblemvol truevol 
                        Nothing Nothing
        (splitSuccess, maybeHP, (boxL,boxR)) 
            = L.split thinvarids box noBoxSkewing value
        newproblemvol
            =
            case maybeHP of
                Nothing -> problemvol
                _ -> problemvol - (ppVolume box) + (ppVolume boxL) + (ppVolume boxR)

        decided = isJust maybeDecision
        decision = fromJust maybeDecision
        maybeDecision = L.decide dim value
        value = 
            case fptype of
                 B32 -> evalForm currdeg maxsize ix box (23,-126) form :: L.TVM -- Maybe Bool
                 B64 -> evalForm currdeg maxsize ix box (52,-1022) form :: L.TVM -- Maybe Bool

        newstartdeg =
            (origstartdeg + currdeg) `div` 2
        currdeg =  
            case maybeCurrdeg of Just currdeg -> currdeg; _ -> startdeg
        (L.TVMUndecided undecidedSimplerForm undecidedMeasure _) = value
        undecidedMeasureImproved = 
            case maybePrevMeasure of
                Nothing -> True
                Just prevUndecidedMeasure ->
                    prevUndecidedMeasure / undecidedMeasure > improvementRatioThreshold

        thinvarids = DBox.keys thincbox
        thincbox = DBox.filter (ppCoeffsZero . snd) box -- thin subbox of contracted box

        reportBox
            =
            do 
            putStrLn $ "proving over box: " ++ ppShow box
            return ()
        
        newtruevol = truevol + (ppVolume box)
        reportVolume 
            =
            case report of
                 VOL ->
                     putStrLn $
                        "Proved fraction : " ++ show (newtruevol / problemvol)
                 NO -> return ()
        
        reportSplit 
            =
            do
            case maybeHP of
                Nothing -> return ()
                Just hp ->
                    do
                    putStrLn $
                        "skewing using the hyperplane " ++ show hp
--                    putStrLn $
--                        "  original box = " ++ ppShow box
--                    putStrLn $
--                        "  boxL = " ++ ppShow boxL
--                    putStrLn $
--                        "  boxR = " ++ ppShow boxR
--                _ -> return ()
            putStrLn $ 
                "splitting at depth " ++ show depth 
                ++ ", new queue size is " ++ show (qlength + 1)
            return ()
