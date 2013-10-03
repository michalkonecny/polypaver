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
    solveAndReportOnConsole
)
where

import PolyPaver.Args
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
import qualified Data.IntMap as IMap

import System.Console.CmdArgs
import Control.Concurrent (threadDelay)
import System.CPUTime

data ReportLevel =
    ReportNONE | ReportNORMAL | ReportALL 
    deriving (Show,Data,Typeable)

data ProverResult
    = Proved { proverResultCPUTime :: Integer }
    | Disproved { proverResultCPUTime :: Integer }
    | GaveUp 
        { 
            proverResultCPUTime :: Integer, 
            proverResultReason ::  String, 
            proverResultProvedFraction ::  Double
        }
    deriving (Data,Typeable)
    
instance Show ProverResult where
    show (Proved duration) = "PROVED in " ++ showDuration duration
    show (Disproved duration) = "DISPROVED in " ++ showDuration duration
    show (GaveUp duration reason percent) = 
        "GAVE UP: " ++ reason ++ " after " ++ showDuration duration
        ++ " (proved fraction: " ++ show percent ++ ")" 


solveAndReportOnConsole
    args
    originalForm 
    initppb@(_, initbox, varIsInts, varNames)
    =
    do
    putStrLn $ "Trying to decide the conjecture: " ++ (showForm 10000 False originalForm)
    putStrLn $ "over the box " ++ ppShow initppb
    -- possibly initialise plotting:
    mstateTV <- case (plotWidth args, plotHeight args) of
        (w,h) 
            | dim /= 2 || w <= 0 || h <= 0 -> return Nothing
            | otherwise ->
                do
                stateTV <- Plot.initPlot initppb w h
                return $ Just stateTV
    -- take the clock reading:
    inittime <- getCPUTime
    -- start looping:
    loopAux
        (order args)
        mstateTV inittime
        0 -- maxDepthReached

        (Q.singleton (0,[],origstartdeg,originalForm,0,initppb)) -- initial queue with one box only
        1 -- queue length
        1 -- greatest computed queue size
        inittime -- prevtime

        0 -- number of computed boxes
        (ppVolume initppb) -- initial volume
        0 -- volume of proved boxes

        Nothing Nothing
    where
    reportLevel 
        | quiet args = ReportNONE
        | verbose args = ReportALL
        | otherwise = ReportNORMAL
    minIntegrationStepSize = 2^^(- (minIntegrExp args)) 
        -- ^ approximate step to use in piecewise numerical integration
    improvementRatioThreshold = 1.2
        -- ^ when to try raising degree/effort and when to give up and split
    origstartdeg = startDegree args
    dim = DBox.size initbox
    loopAux 
            loopOrder
            mstateTV inittime
            maxDepthReached 
            queue qlength maxQLengthReached prevtime 
            computedboxes problemvol truevol 
            maybeCurrdeg maybePrevMeasure 
        | qlength == 0 =
            case loopOrder of
                BFSFalsifyOnly -> 
                    do
                    currtime <- getCPUTime
                    abort currtime "FAILED TO FALSIFY" "FAILED TO FALSIFY during BFSFalsifyOnly" 
                _ -> reportProvedEverywhere
        | otherwise = do { reportBox; tryNextBox } 
        where
        reportProvedEverywhere = 
            do
            currtime <- getCPUTime
            putStr $
              "\nSearch complete.  Conjecture proved TRUE in " ++
                showDuration (currtime-inittime) ++ "." ++
              "\nComputed boxes: " ++ show computedboxes ++ 
              "\nGreatest queue size: " ++ show maxQLengthReached ++  
              "\nGreatest depth: " ++ show maxDepthReached ++ "\n\n"
            stopProver $ Proved $ currtime-inittime
        timeout = (toInteger $ time args) * second
            where
            second = 1000000000000
        tryNextBox
            -- detect timeout:
            | prevtime - inittime > timeout =  
                do
                abort prevtime "TIMED OUT" "TIMED OUT"
            -- split when forced:
            | depth < minDepth args = 
                do
                reportInitSplit
                currtime <- getCPUTime
                bisectAndRecur form currtime [boxLNoHP, boxRNoHP] True splitVarNoHP
            -- formula is true on this box:
            | decided && decision =
                do
                currtime <- getCPUTime
                reportProved
                loopAux
                    loopOrder
                    mstateTV inittime
                    maxDepthReached 
                    boxes (qlength-1) maxQLengthReached currtime
                    (computedboxes+1) problemvol newtruevol 
                    Nothing Nothing
            -- formula is false on this box:
            | decided =
                do
                currtime <- getCPUTime
                plotBox red
                putStr $
                  "\nConjecture shown FALSE in " ++ 
                  showDuration (currtime-inittime) ++ "." ++
                  "\nConjecture is false for " ++
                  ppShow ppb ++
                  "\nComputed  boxes: " ++ show computedboxes ++ 
                  "\nQueue size: " ++ show qlength ++
                  "\nGreatest queue size: " ++ show maxQLengthReached ++  
                  "\nDepth: " ++ show depth ++
                  "\nGreatest depth: " ++ show maxDepthReached ++  
                  "\nFormula: " ++ showForm 1000 False form ++
                  "\nFormula details: \n" ++ formDebug ++
                  "\n\n" 
                stopProver $ Disproved (currtime-inittime)
            -- formula undecided, raise degree:
            | currdeg < degree args && undecidedMeasureImproved =
                do
                case reportLevel of
                    ReportNONE -> return ()
                    _ -> putStrLn $ "Raising degree to " ++ show (currdeg + 1)
                currtime <- getCPUTime
                loopAux
                    loopOrder
                    mstateTV inittime
                    maxDepthReached 
                    queue qlength maxQLengthReached currtime 
                    computedboxes problemvol truevol
                    (Just $ currdeg + 1)
                    (Just undecidedMeasure)
            -- formula undecided, reached maximum depth:
            | depth >= maxDepth args = 
                do
                currtime <- getCPUTime
                abort currtime "REACHED MAXIMUM DEPTH" ("Reached MAXIMUM DEPTH " ++ show (maxDepth args))
            -- formula undecided, reached maximum queue size:
            | qlength >= (case loopOrder of BFSFalsifyOnly -> 5000 ; _ -> (maxQueueLength args)) = 
                do
                currtime <- getCPUTime
                abort currtime "REACHED MAXIMUM QUEUE SIZE" ("Reached MAXIMUM QUEUE SIZE " ++ show (maxQueueLength args))
            -- formula undecided, cannot split the box any further:
            | not splitSuccess ||
              length thinVars == dim =
                do
                currtime <- getCPUTime
                abort currtime "FAILED TO SPLIT" ("FAILED TO SPLIT undecided box : " ++ ppShow ppb)
            -- split the box:
            | otherwise =
                do
                currtime <- getCPUTime
                reportSplit
                bisectAndRecur undecidedMaybeSimplerForm currtime [boxL, boxR] False splitVar

        abort currtime shorterMsg longerMsg =
            case loopOrder of
                DFSthenBFS -> restartAsBFS
                _ -> doAbort
            where
            firstRunResult = 
                GaveUp (currtime-inittime) shorterMsg (fst $ RA.doubleBounds $ provedFraction False)
            doAbort =
                do
                putStr abortReport
                reportFraction False -- ie not takeCurrentBoxIntoAccount
                stopProver firstRunResult
            abortReport =
              "\nSearch aborted." ++ 
              "\n" ++ longerMsg ++ " after " ++ showDuration (currtime-inittime) ++ "." ++
              "\nComputed boxes : " ++ show computedboxes ++ 
              "\nGreatest queue size : " ++ show maxQLengthReached ++  
              "\nGreatest depth : " ++ show maxDepthReached ++ 
              "\n"
            restartAsBFS =
                do
                putStr abortReport
                reportFraction False -- ie not takeCurrentBoxIntoAccount
                secondRunResult <- loopAux
                    BFSFalsifyOnly
                    mstateTV currtime
                    0 -- maxDepthReached
                    initqueueDifficultPoint 
                    1 -- queue length
                    1 -- greatest computed queue size
                    currtime 
                    0 -- number of computed boxes
                    (ppVolume initppbDifficultPoint) 
                    0 -- volume of proved boxes
                    Nothing Nothing
                return $ case secondRunResult of Disproved _ -> secondRunResult; _ -> firstRunResult
            initqueueDifficultPoint =
                Q.singleton queueElem
            queueElem@(_, _, _, _, _, initppbDifficultPoint) =
                Q.index queue $ min 10 $ Q.length queue - 1

        (depth, skewAncestors, startdeg, form, prevSplitVar, ppb@(skewed, box, _, _)) = Q.index queue 0
            -- beware: "form" above has ranges left over in it from a parent box - do not show them
        boxes = Q.drop 1 queue

        bisectAndRecur form currtime newBoxes isSimpleSplit splitVar =
            case loopOrder of
                DFS -> bisectAndRecurDFS
                DFSthenBFS -> bisectAndRecurDFS
                BFS -> bisectAndRecurBFS
                BFSFalsifyOnly -> bisectAndRecurBFS
            where
            bisectAndRecurBFS =
                    loopAux
                        loopOrder
                        mstateTV inittime
                        (max (depth+1) maxDepthReached) 
                        (boxes Q.>< (Q.fromList $ map prepareBox newBoxes2)) 
                        newQLength newMaxQLength currtime 
                        (computedboxes+1) newproblemvol truevol 
                        Nothing Nothing
            bisectAndRecurDFS =
                    loopAux
                        loopOrder
                        mstateTV inittime
                        (max (depth+1) maxDepthReached) 
                        ((Q.fromList $ map prepareBox newBoxes2) Q.>< boxes) 
                        newQLength newMaxQLength currtime 
                        (computedboxes+1) newproblemvol truevol 
                        Nothing Nothing
            newMaxQLength = max newQLength maxQLengthReached
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
            = L.split thinVarsMaybeNonIntVars maybeVar ppb (boxSkewing args) maybeSplitGuessingLimit value
        (_, _, splitVarNoHP, (boxLNoHP,boxRNoHP))
            = L.split thinVarsMaybeNonIntVars maybeVar ppb False Nothing value
        maybeVar =
            case maybeSplitGuessingLimit of
                Nothing -> Nothing
                _ -> Just $ advanceVar thinVarsMaybeNonIntVars prevSplitVar
            where
            advanceVar forbiddenVars var
                | allVarsForbidden = var
                | newVar `elem` forbiddenVars = advanceVar forbiddenVars newVar
                | otherwise = newVar
                where
                allVarsForbidden = length forbiddenVars == dim
                newVar = (var + 1) `mod` dim
        maybeSplitGuessingLimit = case splitGuessing args of -1 -> Nothing; n -> Just n

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
            evalForm 
                currdeg (maxSize args) ix minIntegrationStepSize ppb -- (epsrelbits,epsabsbits) 
                form
--            case fptype of
--                 B32 -> evalForm currdeg maxsize ix box (23,-126) form :: L.TVM -- Maybe Bool
--                 B32near -> evalForm currdeg maxsize ix box (24,-126) form :: L.TVM -- Maybe Bool
--                 B64 -> evalForm currdeg maxsize ix box (52,-1022) form :: L.TVM -- Maybe Bool
--                 B64near -> evalForm currdeg maxsize ix box (53,-1022) form :: L.TVM -- Maybe Bool
        (L.TVDebugReport formDebug, _) = 
            evalForm 
                currdeg (maxSize args) ix minIntegrationStepSize ppb -- (epsrelbits,epsabsbits) 
                form
        ix = fromInteger $ toInteger $ effort args
        
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


        thinVarsMaybeNonIntVars
            | (splitIntFirst args) && canSplitIntVar = thinVarsAndNonIntVars
            | otherwise = thinVars
            where
            canSplitIntVar = not $ null thickIntVars
            thinVarsAndNonIntVars = thinVars ++ thickNonIntVars
            (thickIntVars, thickNonIntVars) = partition isIntV thickVars
                where
                isIntV var = 
                    case IMap.lookup var varIsInts of Just res -> res; _ -> False
            thickVars = DBox.keys thickbox 
            thickbox = DBox.filter (not . ppCoeffsZero Nothing . snd) box -- thick projection of box
        thinVars = DBox.keys thinbox
            where
            thinbox = DBox.filter (ppCoeffsZero Nothing . snd) box -- thin projection of box

        -- reporting
        reportBox
            =
            case reportLevel of
                ReportNONE -> return ()
                _ ->
                    do
                    putStrLn banner
                    putStrLn identifyBox
                    case depth < minDepth args of
                        True -> return ()
                        _ ->
                            case reportLevel of
                                ReportALL ->
                                    putStrLn $ " Evaluation result: " ++ show value
                                _ ->
                                    do
                                    putStrLn $ " Evaluation result: " ++ show maybeDecision
                
        banner = "**** time = " ++ showDuration (prevtime - inittime) ++ replicate 50 '*'
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
                ReportNORMAL ->
                    do
                    reportFraction True
                ReportALL ->
                    do
                    reportFraction True
                    putStrLn $ formDebug

        reportFraction takeCurrentBoxIntoAccount
            =
            putStrLn $
                "Proved fraction : " ++ show (provedFraction takeCurrentBoxIntoAccount)
                ++ " (Proved volume : " ++ show (oldornewtruevol takeCurrentBoxIntoAccount)
                ++ " ; Overall volume : " ++ show problemvol ++ ")"
        provedFraction takeCurrentBoxIntoAccount
            | problemvol `RA.equalReals` 0 == Just True = 1
            | otherwise = ((oldornewtruevol takeCurrentBoxIntoAccount) / problemvol)
        oldornewtruevol takeCurrentBoxIntoAccount
            | takeCurrentBoxIntoAccount =
                if decided && decision then newtruevol else truevol
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
                                ++ "\n  derived from the formula " ++ showForm 1000 False form
                _ -> return ()
            case reportLevel of
                ReportNONE -> return ()
                _ ->
                    do
                    putStrLn $ 
                        "Splitting at depth " ++ show depth
                        ++ reportVar 
                    reportFraction True
                    where
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
        
        plotStepDelayMs = 0
        
showDuration durationInPicoseconds =
    (show durationInSeconds) ++ " s (" ++ show days ++ "d, " ++ show hours ++ "h, " ++ show mins ++ "min, " ++ show secs ++ "s)" 
    where
    durationInSeconds = (fromInteger durationInPicoseconds) / 1000000000000 
    (minsAll, secs) = quotRem (Prelude.round durationInSeconds) 60
    (hoursAll, mins) = quotRem minsAll 60
    (days, hours) = quotRem hoursAll 24
    _ = [secs, mins, hours, days :: Int]
      