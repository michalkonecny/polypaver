{-# LANGUAGE DeriveDataTypeable #-}

{-|
    Module      :  PolyPaver.ProverLoop
    Description :  the main counter-example search algorihtm 
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    The main counter-example search algorihtm.
-}
module PolyPaver.ProverLoop
(
    Order(..),
    ReportLevel(..),
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

data ReportLevel =
    ReportNONE | ReportNORMAL | ReportALL 
    deriving (Show,Data,Typeable)

data ProverResult
    = Proved { proverResultCPUTime :: Integer }
    | Disproved { proverResultCPUTime :: Integer }
    | GaveUp { proverResultCPUTime :: Integer, proverResultReason ::  String }
    deriving (Data,Typeable)
    
instance Show ProverResult where
    show (Proved duration) = "PROVED in " ++ showDuration duration
    show (Disproved duration) = "DISPROVED in " ++ showDuration duration
    show (GaveUp duration reason) = "GAVE UP: " ++ reason ++ " after " ++ showDuration duration 


loop
    plotSize plotStepDelayMs
    order reportLevel epsrelbits epsabsbits boxSkewing splitGuessing
    origstartdeg maxdeg improvementRatioThreshold 
    maxsize
    pwdepth
    mindepth maxdepth maxDepthReached
    ix maxtime prec originalForm 
--    intvarids 
    initppb@(_, initbox, varIsInts, varNames)
    =
    do
    putStrLn $ "Trying to decide the conjecture: " ++ showForm False originalForm
    putStrLn $ "over the box " ++ ppShow initppb
    -- possibly initialise plotting:
    mstateTV <- case plotSize of
        (w,h) 
            | dim /= 2 || w <= 0 || h <= 0 -> return Nothing
            | otherwise ->
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
    dim = DBox.size initbox
    loopAux 
            mstateTV inittime
            maxDepthReached 
            queue qlength maxQLength prevtime 
            computedboxes problemvol truevol 
            maybeCurrdeg maybePrevMeasure 
        | qlength == 0 = reportProvedEverywhere
        | otherwise = do { reportBox; tryNextBox } 
        where
        reportProvedEverywhere = 
            do
            currtime <- getCPUTime
            putStr $
              "\nSearch complete.  Conjecture proved TRUE in " ++
                showDuration (currtime-inittime) ++ "." ++
              "\nComputed boxes: " ++ show computedboxes ++ 
              "\nGreatest queue size: " ++ show maxQLength ++  
              "\nGreatest depth: " ++ show maxDepthReached ++ "\n\n"
            stopProver $ Proved $ currtime-inittime
        tryNextBox
            | depth < mindepth = 
                do
                reportInitSplit
                currtime <- getCPUTime
                bisectAndRecur form currtime [boxLNoHP, boxRNoHP] True splitVarNoHP
            | prevtime-inittime > maxtime*1000000000000 = 
                do
                putStr $
                  "\nSearch aborted." ++ 
                  "\nTIMED OUT after " ++ show maxtime ++ 
                  " second" ++ (if maxtime == 1 then "." else "s.") ++ 
                  "\nComputed boxes: " ++ show computedboxes ++ 
                  "\nQueue size: " ++ show qlength ++
                  "\nGreatest queue size: " ++ show maxQLength ++  
                  "\nGreatest depth: " ++ show maxDepthReached ++ 
                  "\n\n"
                stopProver $ GaveUp (prevtime-inittime) "TIMED OUT"
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
                  "\nSearch aborted. Conjecture proved FALSE in " ++ 
                  showDuration (currtime-inittime) ++ "." ++
                  "\nConjecture proved false for " ++
                  ppShow ppb ++
                  "\nComputed  boxes: " ++ show computedboxes ++ 
                  "\nQueue size: " ++ show qlength ++
                  "\nGreatest queue size: " ++ show maxQLength ++  
                  "\nDepth: " ++ show depth ++
                  "\nGreatest depth: " ++ show maxDepthReached ++  
                  "\nFormula: " ++ showForm False form ++
                  "\nFormula details: \n" ++ formDebug ++
                  "\n\n" 
                stopProver $ Disproved (currtime-inittime)
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
                  "\nReached MAXIMUM DEPTH " ++ show maxdepth ++ 
--                  "\nUndecided for : " ++
--                  ppShow ppb ++
                  " after " ++
                  showDuration (currtime-inittime) ++ "." ++
                  "\nComputed boxes : " ++ show computedboxes ++
                  "\nQueue size : " ++ show qlength ++
                  "\nGreatest queue size : " ++ show maxQLength ++  
                  "\n\n"
                stopProver $ GaveUp (currtime-inittime) "REACHED MAXIMUM DEPTH"
            | not splitSuccess ||
              length thinvarids == dim = -- cannot split any further
                do
                currtime <- getCPUTime
                putStr $ 
                  "\nSearch aborted." ++ 
                  "\nFAILED TO SPLIT undecided box : " ++ ppShow ppb ++ 
                  " after " ++
                  showDuration (currtime-inittime) ++ "." ++
                  "\nComputed boxes : " ++ show computedboxes ++ 
                  "\nQueue size : " ++ show qlength ++
                  "\nGreatest queue size : " ++ show maxQLength ++  
                  "\nDepth : " ++ show depth ++ 
                  "\nGreatest depth : " ++ show maxDepthReached ++  
                  "\n\n"
                stopProver $ GaveUp (currtime-inittime) "FAILED TO SPLIT"
            | otherwise = -- formula undecided on this box, will split it
                do
                currtime <- getCPUTime
                reportSplit
                bisectAndRecur undecidedMaybeSimplerForm currtime [boxL, boxR] False splitVar

        (depth, skewAncestors, startdeg, form, prevSplitVar, ppb@(skewed, box, _, _)) = Q.index queue 0
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
        maybeDecision = L.decide (value :: L.TVM)
        (value, formWithRanges) =
            evalForm currdeg maxsize pwdepth ix ppb (epsrelbits,epsabsbits) form
--            case fptype of
--                 B32 -> evalForm currdeg maxsize ix box (23,-126) form :: L.TVM -- Maybe Bool
--                 B32near -> evalForm currdeg maxsize ix box (24,-126) form :: L.TVM -- Maybe Bool
--                 B64 -> evalForm currdeg maxsize ix box (52,-1022) form :: L.TVM -- Maybe Bool
--                 B64near -> evalForm currdeg maxsize ix box (53,-1022) form :: L.TVM -- Maybe Bool
        (L.TVDebugReport formDebug, _) = 
            evalForm currdeg maxsize pwdepth ix ppb (epsrelbits,epsabsbits) form

        newstartdeg =
            (origstartdeg + currdeg) `div` 2
        currdeg =  
            case maybeCurrdeg of Just currdeg -> currdeg; _ -> startdeg
        (L.TVMUndecided undecidedSimplerForm undecidedMeasure _ _) = value
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
        thincbox = DBox.filter (ppCoeffsZero Nothing . snd) box -- thin subbox of contracted box

        -- reporting
        reportBox
            =
            case reportLevel of
                ReportNONE -> return ()
                _ ->
                    do
                    putStrLn banner
                    putStrLn identifyBox
                    case depth < mindepth of
                        True -> return ()
                        _ ->
                            case reportLevel of
                                ReportALL ->
                                    putStrLn $ " Evaluation result: " ++ show value
                                _ ->
                                    putStrLn $ " Evaluation result: " ++ show maybeDecision
                
        banner = replicate 100 '*'
        identifyBox =
            "Deciding over box" ++ show computedboxes 
            ++ "(depth=" ++ show depth ++ ", queue size=" ++ show qlength ++ ")"            
            ++ ": " ++ ppShow ppb
        reportInitSplit
            =
            do
            plotBox yellow
            case reportLevel of
                ReportNONE -> return ()
                _ ->
                    do
                    putStrLn $ "Initial splitting at depth " ++ show depth
            
        reportProved
            =
            do
            plotBox green
            case reportLevel of
                ReportNONE -> return ()
                _ ->
                    do
                    reportFraction
--                    putStrLn $ formDebug

        reportFraction
            =
            putStrLn $
                "Proved fraction : " ++ show provedFraction
                ++ " (Proved volume : " ++ show oldornewtruevol
                ++ " ; Overall volume : " ++ show problemvol ++ ")"
            where
            provedFraction
                | problemvol `RA.equalReals` 0 == Just True = 1
                | otherwise = (oldornewtruevol / problemvol)
            oldornewtruevol
                | decided && decision = newtruevol
                | otherwise = truevol
                
        reportSplit
            =
            do
            plotBox yellow
            case reportLevel of
                ReportALL ->
                    case maybeHP of
                        Nothing -> return ()
                        Just ((hp, _), form, lab, vagueness) ->
                            do
                            putStrLn $
                                "Skewing using the hyperplane " ++ showAffine hp
                                ++ "\n  label = " ++ lab
                                ++ "\n  vagueness = " ++ show vagueness
                                ++ "\n  derived from the formula " ++ showForm False form
                _ -> return ()
            case reportLevel of
                ReportNONE -> return ()
                _ ->
                    do
                    putStrLn $ 
                        "Splitting at depth " ++ show depth
                        ++ reportVar 
                        ++ reportQSize 
                    reportFraction
                    where
                    reportQSize =
                        case order of
                            D -> ", new queue size is " ++ show (qlength + 1)
                            B -> ""
                    reportVar
                        | skewed = " domain of skewed variable _" ++ showVar varNames splitVar ++ "_"
                        | otherwise = " domain of variable " ++ showVar varNames splitVar
                         
            return ()

        -- plotting
        stopProver result =
            case mstateTV of
                Nothing -> return result
                Just stateTV ->
                    do 
                    Plot.waitForClose stateTV
                    return result
        
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
        
showDuration durationInPicoseconds =
    (show $ ((fromInteger durationInPicoseconds) / 1000000000000)) ++ " s"