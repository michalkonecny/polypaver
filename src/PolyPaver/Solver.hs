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
module PolyPaver.Solver 
(
    Order(..),
    Report(..),
    loop
)
where

import PolyPaver.Form
import PolyPaver.PPBox
import PolyPaver.Eval
import PolyPaver.Vars
import qualified PolyPaver.Logic as L
import qualified PolyPaver.Plot as Plot

import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Real.Approx.Interval
import Numeric.ER.Real.DefaultRepr
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox.IntMap
import qualified Data.Sequence as Q
import Numeric.ER.Misc

import Data.List
import Data.Maybe
import qualified Data.Map as Map

import System.Console.CmdArgs
import Control.Concurrent (threadDelay)
import System.CPUTime

data Order = 
    B | D
    deriving (Show,Data,Typeable)

data Report =
    ReportNONE | ReportNORMAL | ReportALL 
    deriving (Show,Data,Typeable)

loop
    plotSize plotStepDelayMs
    order report epsrelbits epsabsbits boxSkewing splitGuessing
    origstartdeg maxdeg improvementRatioThreshold 
    maxsize
    mindepth maxdepth maxDepthReached
    ix maxtime prec originalForm 
--    intvarids 
    initppb@(_, initbox, varNames)
    =
    do
    putStrLn $ "proving: " ++ showForm originalForm
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

        (Q.singleton (0,[],origstartdeg,originalForm,0,initppb)) -- initial queue with one box only
        1 -- queue length
        1 -- greatest computed queue size
        inittime -- prevtime

        0 -- number of computed boxes
        (ppVolume initppb) -- initial volume
        0 -- volume of proved boxes

        Nothing Nothing
    where
    loopAux 
        mstateTV inittime
        maxDepthReached 
        queue qlength maxQLength prevtime 
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
                  "\nSearch complete." ++ 
                  "\nConjecture proved true in " ++
                    show ((fromInteger (currtime-inittime)) / 1000000000000) ++
                    " seconds." ++
                  "\nComputed boxes : " ++ show computedboxes ++ 
                  "\nGreatest queue size : " ++ show maxQLength ++  
                  "\nGreatest depth : " ++ show maxDepthReached ++ "\n\n"
                stopProver 
            | depth < mindepth = 
                do
                reportInitSplit
                currtime <- getCPUTime
                bisectAndRecur form currtime [boxLNoHP, boxRNoHP] True splitVarNoHP
            | prevtime-inittime > maxtime*1000000000000 = 
                do
                putStr $
                  "\nSearch aborted." ++ 
                  "\nTimed out after " ++ show maxtime ++ 
                  " second" ++ (if maxtime == 1 then "." else "s.") ++ 
                  "\nComputed boxes : " ++ show computedboxes ++ 
                  reportQLengthS ++
                  "\nGreatest queue size : " ++ show maxQLength ++  
                  "\nGreatest depth : " ++ show maxDepthReached ++ 
                  "\n\n"
                stopProver 
            | decided && decision = -- formula true on this box
                do
                currtime <- getCPUTime
                reportProved
                loopAux
                    mstateTV inittime
                    maxDepthReached 
                    boxes (qlength-1) maxQLength currtime
                    (computedboxes+1) problemvol newtruevol 
                    Nothing Nothing
            | decided = -- formula false on this box
                do
                currtime <- getCPUTime
                plotBox red
                putStr $
                  "\nSearch aborted." ++ 
                  "\nConjecture proved false in " ++ 
                  show ((fromInteger (currtime-inittime)) / 1000000000000) ++
                  " second" ++ (if maxtime == 1 then "." else "s.") ++ 
                  "\nConjecture proved false for " ++
                  ppShow ppb ++
                  "\nComputed  boxes : " ++ show computedboxes ++ 
                  reportQLengthS ++
                  "\nGreatest queue size : " ++ show maxQLength ++  
                  "\nDepth : " ++ show depth ++
                  "\nGreatest depth : " ++ show maxDepthReached ++  
                  "\nFormula : " ++ showForm form ++
                  "\nFormula details: \n" ++ formDebug ++
                  "\n\n" 
                stopProver 
            | currdeg < maxdeg && undecidedMeasureImproved = -- try raising the degree before splitting
                do
                putStrLn $ "raising degree to " ++ show (currdeg + 1)
                currtime <- getCPUTime
                loopAux
                    mstateTV inittime
                    maxDepthReached 
                    queue qlength maxQLength currtime 
                    computedboxes problemvol truevol
                    (Just $ currdeg + 1)
                    (Just undecidedMeasure)
            | depth >= maxdepth = 
                do
                currtime <- getCPUTime
                putStr $ 
                  "\nSearch aborted." ++ 
                  "\nReached maximum depth " ++ show maxdepth ++ 
--                  "\nUndecided for : " ++
--                  ppShow ppb ++
                  " after " ++
                  show ((fromInteger (currtime-inittime)) / 1000000000000) ++
                  " seconds." ++ 
                  "\nComputed boxes : " ++ show computedboxes ++
                  reportQLengthS ++
                  "\nGreatest queue size : " ++ show maxQLength ++  
                  "\n\n"
                stopProver
            | not splitSuccess ||
              length thinvarids == dim = -- cannot split any further
                do
                currtime <- getCPUTime
                putStr $ 
                  "\nSearch aborted." ++ 
                  "\nCould not split undecided box : " ++ ppShow ppb ++ 
                  " after " ++
                  show ((fromInteger (currtime-inittime)) / 1000000000000) ++
                  " seconds." ++
                  "\nComputed boxes : " ++ show computedboxes ++ 
                  reportQLengthS ++
                  "\nGreatest queue size : " ++ show maxQLength ++  
                  "\nDepth : " ++ show depth ++ 
                  "\nGreatest depth : " ++ show maxDepthReached ++  
                  "\n\n"
                stopProver 
            | otherwise = -- formula undecided on this box, will split it
                do
                currtime <- getCPUTime
                reportSplit
                bisectAndRecur undecidedMaybeSimplerForm currtime [boxL, boxR] False splitVar

        (depth, skewAncestors, startdeg, form, prevSplitVar, ppb@(skewed, box, _)) = Q.index queue 0
        dim = DBox.size box
        boxes = Q.drop 1 queue

        bisectAndRecur form currtime newBoxes isSimpleSplit splitVar =
            case order of 
                B -> 
                    loopAux
                        mstateTV inittime
                        (max (depth+1) maxDepthReached) 
                        (boxes Q.>< (Q.fromList $ map prepareBox newBoxes2)) 
                        newQLength newMaxQLength currtime 
                        (computedboxes+1) newproblemvol truevol 
                        Nothing Nothing
                D ->
                    loopAux 
                        mstateTV inittime
                        (max (depth+1) maxDepthReached) 
                        ((Q.fromList $ map prepareBox newBoxes2) Q.>< boxes) 
                        newQLength newMaxQLength currtime 
                        (computedboxes+1) newproblemvol truevol 
                        Nothing Nothing
            where
            newMaxQLength = max newQLength maxQLength
            newQLength = qlength - 1 + newBoxes2length
            newBoxes2length = length newBoxes2
            newBoxes2
                = filter intersectsAllSkewAncestors newBoxes
                where
                intersectsAllSkewAncestors ppb
                    = and $ map couldIntersect skewAncestors
                    where
                    couldIntersect ancestor
                        = ppIntersect ancestor ppb /= Just False
            prepareBox ppb =
                (depth+1, newSkewAncestors, newstartdeg, form, splitVar, ppb)
            newSkewAncestors
                | isSimpleSplit = skewAncestors
                | otherwise
                    = case maybeHP of 
                        Nothing -> skewAncestors
                        _ -> ppb : skewAncestors 
                    -- when skewing, part of the skewed box stretches outside of the original box - 
                    -- when splitting this box and its subboxes, need to drop any that are completely outside this box
            newproblemvol
                | isSimpleSplit = problemvol
                | otherwise
                    =
                    case (maybeHP, newBoxes2length) of
                        (Nothing, 2) -> problemvol -- no skewing or dropping of boxes - a clean split
                        _ -> problemvol - (ppVolume ppb) + (sum $ map ppVolume newBoxes2)
        (splitSuccess, maybeHP, splitVar, (boxL,boxR))
            = L.split thinvarids maybeVar ppb boxSkewing splitGuessing value
        (_, _, splitVarNoHP, (boxLNoHP,boxRNoHP))
            = L.split thinvarids maybeVar ppb False Nothing value
        undecidedMaybeSimplerForm
            =
            case maybeHP of
                Nothing -> undecidedSimplerForm
                _ -> originalForm
                    -- when skewing, the new boxes are not sub-boxes of box and thus we cannot
                    -- rely on the simplification of form performed while evaluating it over box

        newtruevol = truevol + (ppVolume ppb)

        decided = isJust maybeDecision
        decision = fromJust maybeDecision
        maybeDecision = L.decide dim value
        value =
            evalForm currdeg maxsize ix ppb (epsrelbits,epsabsbits) form :: L.TVM
--            case fptype of
--                 B32 -> evalForm currdeg maxsize ix box (23,-126) form :: L.TVM -- Maybe Bool
--                 B32near -> evalForm currdeg maxsize ix box (24,-126) form :: L.TVM -- Maybe Bool
--                 B64 -> evalForm currdeg maxsize ix box (52,-1022) form :: L.TVM -- Maybe Bool
--                 B64near -> evalForm currdeg maxsize ix box (53,-1022) form :: L.TVM -- Maybe Bool
        L.TVDebugReport formDebug = 
            evalForm currdeg maxsize ix ppb (epsrelbits,epsabsbits) form :: L.TVDebugReport

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

        maybeVar =
            case splitGuessing of
                Nothing -> Nothing
                _ -> Just $ advanceVar thinvarids prevSplitVar
            where
            advanceVar forbiddenVars var
                | allVarsForbidden = var
                | newVar `elem` forbiddenVars = advanceVar forbiddenVars newVar
                | otherwise = newVar
                where
                allVarsForbidden = length forbiddenVars == dim
                newVar = (var + 1) `mod` dim
                

        thinvarids = DBox.keys thincbox
        thincbox = DBox.filter (ppCoeffsZero . snd) box -- thin subbox of contracted box

        -- reporting
        reportBox
            =
            case report of
                ReportNONE -> return ()
                _ ->
                    do
                    putStrLn $ replicate 100 '*'
                    putStrLn $ "proving over box" ++ show computedboxes ++  ": " ++ ppShow ppb
                    case depth < mindepth of
                        True -> return ()
                        _ ->
                            case report of
                                ReportALL ->
                                    putStrLn $ " evaluation result = " ++ show value
                                _ ->
                                    putStrLn $ " evaluation result = " ++ show maybeDecision
                
        reportQLengthS
            = 
            case order of
                D -> "\nQueue size : " ++ show qlength
                _ -> ""
        reportInitSplit
            =
            do
            plotBox yellow
            case report of
                ReportNONE -> return ()
                _ ->
                    do
                    putStrLn $ "initial splitting at depth " ++ show depth
            
        reportProved
            =
            do
            plotBox green
            case report of
                ReportNONE -> return ()
                _ ->
                    do
                    putStrLn $
                        "Proved fraction : " ++ show provedFraction
--                    putStrLn $ formDebug
            where
            provedFraction
                | problemvol `RA.equalReals` 0 == Just True = 1
                | otherwise = (newtruevol / problemvol)
        reportSplit
            =
            do
            plotBox yellow
            case report of
                ReportALL ->
                    case maybeHP of
                        Nothing -> return ()
                        Just ((hp, _), form, vagueness) ->
                            do
                            putStrLn $
                                "skewing using the hyperplane " ++ showAffine hp
                                ++ "\n  vagueness = " ++ show vagueness
                                ++ "\n  derived from the formula " ++ showForm form
                _ -> return ()
            case report of
                ReportNONE -> return ()
                _ ->
                    putStrLn $ 
                        "splitting at depth " ++ show depth
                        ++ reportDepth 
                        ++ ", new queue size is " ++ show (qlength + 1)
                    where
                    reportDepth 
                        | skewed = " domain of skewed variable _" ++ showVar varNames splitVar ++ "_"
                        | otherwise = " domain of variable " ++ showVar varNames splitVar 
            return ()

        -- plotting
        stopProver =
            case mstateTV of
                Nothing -> return ()
                Just stateTV -> Plot.waitForClose stateTV
        
        plotBox colour 
            =
            case mstateTV of
                Nothing -> return ()
                _ | depth > 16 -> return ()
                Just stateTV ->
                    do
                    Plot.addBox stateTV colour box
                    threadDelay $ 1000 * plotStepDelayMs
        green = (0.1,0.6,0.1,0.4)
        red = (0.6,0.1,0.1,1)
        yellow = (0.6,0.6,0.1,0.05)
        
            