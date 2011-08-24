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
import qualified PolyPaver.Plot as Plot

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
import Control.Concurrent (threadDelay)
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

loop
    plotSize plotStepDelayMs
    order report fptype noBoxSkewing
    origstartdeg maxdeg improvementRatioThreshold 
    maxsize
    mindepth maxdepth maxDepthReached
    ix maxtime prec originalForm 
--    intvarids 
    initbox
    =
    do
    -- possibly initialise plotting:
    mstateTV <- case plotSize of
        (0,0) -> return Nothing
        (w,h) -> 
            do
            stateTV <- Plot.initPlot initbox w h
            return $ Just stateTV
    -- take the clock reading:
    inittime <- getCPUTime
    -- start looping:
    loopAux
        mstateTV inittime
        maxDepthReached

        (Q.singleton (0,[],origstartdeg,originalForm,initbox)) -- initial queue with one box only
        1 -- queue length
        inittime -- prevtime

        1 -- number computed boxes
        (ppVolume initbox) -- initial volume
        0 -- volume of proved boxes

        Nothing Nothing
    where
    loopAux 
        mstateTV inittime
        maxDepthReached 
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
                stopProver 
            | depth < mindepth = 
                do
                reportInitSplit
                currtime <- getCPUTime
                bisectAndRecur form currtime [boxLNoHP, boxRNoHP] problemvol
            | prevtime-inittime > maxtime*1000000000000 = 
                do
                putStr $
                  "\nTimeout.\nSearch aborted after " ++
                  show maxtime ++
                  " seconds.\nComputed : " ++ show computedboxes ++ 
                  " boxes.\nReaching max depth : " ++ show maxDepthReached ++ "\n\n"
                stopProver 
            | decided && decision = -- formula true on this box
                do
                currtime <- getCPUTime
                reportProved
                loopAux
                    mstateTV inittime
                    maxDepthReached 
                    boxes (qlength-1) currtime
                    (computedboxes+1) problemvol newtruevol 
                    Nothing Nothing
            | decided = -- formula false on this box
                do
                currtime <- getCPUTime
                plotBox red
                putStr $
                  "\nCounter example found, search aborted.\nTheorem proved false for " ++
                  ppShow box ++
                  "\nSeconds elapsed : " ++
                  show ((fromInteger (currtime-inittime)) / 1000000000000) ++
                  "\nComputed  boxes : " ++ show computedboxes ++ 
                  "\nMax depth : " ++ show maxDepthReached ++  
                  "\nDepth : " ++ show depth ++ "\n\n"
                stopProver 
            | currdeg < maxdeg && undecidedMeasureImproved = -- try raising the degree before splitting
                do
                putStrLn $ "raising degree to " ++ show (currdeg + 1)
                currtime <- getCPUTime
                loopAux
                    mstateTV inittime
                    maxDepthReached 
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
                stopProver 
            | otherwise = -- formula undecided on this box, will split it
                do
                currtime <- getCPUTime
                reportSplit
                bisectAndRecur undecidedMaybeSimplerForm currtime [boxL, boxR] newproblemvol

        (depth, skewParents, startdeg, form, box) = Q.index queue 0
        dim = DBox.size box
        boxes = Q.drop 1 queue

        bisectAndRecur form currtime newBoxes newproblemvol =
            case order of 
                B -> 
                    loopAux
                        mstateTV inittime
                        (max (depth+1) maxDepthReached) 
                        (boxes Q.>< (Q.fromList newBoxes2)) 
                        newQLength currtime 
                        (computedboxes+1) newproblemvol truevol 
                        Nothing Nothing
                D ->
                    loopAux 
                        mstateTV inittime
                        (max (depth+1) maxDepthReached) 
                        ((Q.fromList newBoxes2) Q.>< boxes) 
                        newQLength currtime 
                        (computedboxes+1) newproblemvol truevol 
                        Nothing Nothing
            where
            newQLength =
                qlength - 1 + (length newBoxes2)
            newBoxes2 
                = map prepareBox $ filter intersectsAllSkewParents newBoxes
            intersectsAllSkewParents box
                = and $ map couldIntersectParent skewParents
                where
                couldIntersectParent parentBox
                    = ppIsectInterior parentBox box /= Just False
            prepareBox box =            
                (depth+1,newSkewParents, newstartdeg, form,box)
        (splitSuccess, maybeHP, (boxL,boxR))
            = L.split thinvarids box noBoxSkewing value
        (_, _, (boxLNoHP,boxRNoHP))
            = L.split thinvarids box True value
        (newproblemvol, undecidedMaybeSimplerForm, newSkewParents)
            =
            case maybeHP of
                Nothing -> (problemvol, undecidedSimplerForm, skewParents)
                _ -> (problemvol - (ppVolume box) + (ppVolume boxL) + (ppVolume boxR), 
                      originalForm,
                    -- when skewing, the new boxes are not sub-boxes of box and thus we cannot
                    -- rely on the simplification of form performed while evaluating it over box
                      box : skewParents)

        newtruevol = truevol + (ppVolume box)

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
                
        reportInitSplit
            =
            do
            plotBox yellow
            putStrLn $ "initial splitting at depth " ++ show depth
            
        reportProved
            =
            do
            plotBox green
            case report of
                 VOL ->
                     putStrLn $
                        "Proved fraction : " ++ show (newtruevol / problemvol)
                 NO -> return ()
        
        reportSplit
            =
            do
            plotBox yellow
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

        stopProver =
            case mstateTV of
                Nothing -> return ()
                Just stateTV -> Plot.waitForClose stateTV
        
        plotBox colour 
            =
            case mstateTV of
                Nothing -> return ()
                Just stateTV ->
                    do
                    Plot.addBox stateTV colour box
                    threadDelay $ 1000 * plotStepDelayMs
        green = (0.1,0.6,0.1,0.4)
        red = (0.1,0.6,0.1,0.4)
        yellow = (0.6,0.6,0.1,0.4)
        
            