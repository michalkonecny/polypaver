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
    PaverResult(..),
    PaverProgress(..),
    PavingState(..),
    BoxToDo(..),
    tryToDecideFormOnBoxByPaving
)
where

import PolyPaver.Args
import PolyPaver.Form
import PolyPaver.APBox
import PolyPaver.Eval
--import PolyPaver.Vars
import qualified PolyPaver.Logic as L
--import qualified PolyPaver.Plot as Plot

import qualified Numeric.ER.Real.Approx as RA
--import Numeric.ER.Real.Approx.Interval
import Numeric.ER.Real.DefaultRepr
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
--import Numeric.ER.BasicTypes.DomainBox.IntMap
import qualified Data.Sequence as Q
--import Numeric.ER.Misc

import Data.List
--import Data.Maybe
--import qualified Data.Map as Map
import qualified Data.IntMap as IMap

import qualified Data.Strict.Maybe as SM
import qualified Data.Strict.Tuple as SP

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue

import System.CPUTime

data PaverResult =
    PaverResult
    {
        paverResult_formTruthOrMessage :: Either String Bool,
        paverResult_lastPPB :: APBox Double, 
        paverResult_durationInPicosecs :: Integer,
        paverResult_state :: PavingState Double
    }
--    deriving (Data,Typeable)
    
data PaverProgress =
    PaverProgress
    {
        paverProgress_message :: ! String,
        paverProgress_durationInPicosecs :: ! Integer,
        paverProgress_maybeState :: ! (SM.Maybe (PavingState Double)),
        paverProgress_maybeCurrentBoxToDo :: ! (SM.Maybe (BoxToDo Double)),
        paverProgress_maybeNewBoxDone :: ! (SM.Maybe (SP.Pair (APBox Double) (SM.Maybe Bool)))
    }
    
data PavingState b =
    PavingState
    {
        pavingState_trueFraction :: ! (IRA b),
        pavingState_computedBoxes :: ! Int,
        pavingState_maxQLengthReached :: ! Int,
        pavingState_maxDepthReached :: ! Int 
    }
    
data BoxToDo b =
    BoxToDo
    {
        boxToDo_depth :: ! Int,
        boxToDo_queueSize :: ! Int,
        boxToDo_startDeg :: ! Int,
        boxToDo_form :: ! (Form Int),
        boxToDo_prevSplitVar :: ! Int,
        boxToDo_ppb :: ! (APBox b)
    }

{-|
   Attempt to decide @form@ over @box@ by evaluating @form@ over various sub-boxes of @box@.
   These sub-boxes are obtain by adaptive bisection.
   
   The program sends several 'PaverProgress' records on the @out@ channel for each box it evaluates.
   It also sends one 'PaverResult' record when it is finished with paving.
-}
tryToDecideFormOnBoxByPaving :: 
    [TBQueue (Either PaverProgress PaverResult)] {-^ @out@ - Channels to send progress reports to -} ->
    Args {-^ @args@ - A record with various parameters -} -> 
    Form () {-^ @form@ - A logical formula -} ->
    APBox Double {-^ @box@ - An axis-parallel box in R^n -} -> 
    IO ()
tryToDecideFormOnBoxByPaving
    outChannels
    args
    originalFormRaw 
    initppb@(APBox initbox varIsInts _varNames)
    =
    do
    inittime <- getCPUTime
    pave inittime
    where
    -- fixed or derived parameters:
    improvementRatioThreshold = 1.2
        -- ^ when to try raising degree/effort and when to give up and split
    originalStartDeg = startDegree args
    dim = DBox.size initbox
    originalForm = prepareForm originalFormRaw
    
    pave inittime = -- continue with inittime available in the scope
        pavingLoop 
        -- pavingLoop processes first box in a given queue and calls itself with an updated queue
            -- the following values change from box to box: 
            (0 :: Int) -- maxDepthReached
            (Q.singleton firstBoxToDo) -- initial queue with one box only
            (1 :: Int) -- greatest computed queue size
            (0 :: Int) -- number of computed boxes
            (boxVolume initppb) -- initial volume
            0 -- volume of proved boxes
            Nothing Nothing
        where
        firstBoxToDo =
            BoxToDo 
            {
                boxToDo_depth = 0,
                boxToDo_queueSize = 1,
                boxToDo_startDeg = originalStartDeg,
                boxToDo_form = originalForm,
                boxToDo_prevSplitVar = 0,
                boxToDo_ppb = initppb    
            }
        pavingLoop 
                maxDepthReached 
                queue
                maxQLengthReached 
                computedboxes 
                problemvol 
                truevol 
                maybeCurrdeg 
                maybePrevMeasure 
            | qlength == 0 =
                do
                case (order args) of
--                    BFSFalsifyOnly -> 
--                        stopPaverGiveUp False "Problem proved true while trying to falsify it." 
                    _ -> 
                        stopPaverProved
            | otherwise = 
                do
                -- detect timeout:
                currtime <- getCPUTime
                case currtime - inittime > timeout of
                    True -> stopPaverGiveUp False "TIMED OUT"
                    _ ->
                        do 
                        reportBoxStart
                        tryNextBox
            where
            qlength = Q.length queue
            timeout = (toInteger $ time args) * second
                where
                second = 1000000000000
            
            tryNextBox
                -- exceeded maximum depth:
                | depth > maxDepth args = 
                    stopPaverGiveUp False ("Exceeded MAXIMUM DEPTH " ++ show (maxDepth args) ++ ".")
                -- formula undecided, exceeded maximum queue size:
                | qlength > maxQueueLength args = 
                    stopPaverGiveUp False ("Exceeded MAXIMUM QUEUE SIZE " ++ show (maxQueueLength args) ++ ".")
                -- split when forced:
                | depth < minDepth args = 
                    do
                    reportBoxInitSplit
                    bisectAndRecur formRaw [boxL, boxR] True splitVar
                -- formula is true on this box:
                | decidedAndTrue =
                    do
                    reportBoxProved
                    pavingLoop
                        maxDepthReached 
                        queueTail 
                        maxQLengthReached 
                        (computedboxes+1) 
                        problemvol 
                        newtruevol 
                        Nothing Nothing
                -- formula is false on this box:
                | decided =
                    do
                    reportBoxDisproved
                    stopPaverDisproved
                -- formula undecided, raise degree:
                | currentDeg < degree args && undecidedMeasureImproved =
                    do
                    reportRaiseDegree
                    pavingLoop
                        maxDepthReached 
                        queue maxQLengthReached
                        computedboxes problemvol truevol
                        (Just $ currentDeg + 1)
                        (Just undecidedMeasure)
                -- formula undecided, cannot split the box any further:
                | not splitSuccess ||
                  length thinVars == dim =
                    do
                    stopPaverGiveUp False ("FAILED TO SPLIT undecided box.")
                -- split the box:
                | otherwise =
                    do
                    reportBoxSplit
                    bisectAndRecur undecidedSimplerForm [boxL, boxR] False splitVar
    

            BoxToDo depth _ startDeg formRaw _prevSplitVar ppb = boxToDo 
                -- beware: "formRaw" above has ranges left over in it from a parent box - do not show them
            boxToDo = Q.index queue 0
            queueTail = Q.drop 1 queue


            {- definitions related to evaluating the formula over the box -}
    
            [decided, decidedAndTrue] = 
                case maybeFormTruth of
                    Just True -> [True, True]
                    Just _ -> [True, False]
                    _ -> [False, False]
                where
                maybeFormTruth = L.decide value
            (value, _formWithRanges) = valueAndForm 
            valueAndForm =
                evalForm 
                    currentDeg (maxSize args) ix minIntegrationStepSize ppb 
                    formRaw
                where
                minIntegrationStepSize = 2^^(- (minIntegrExp args))
                ix = fromInteger $ toInteger $ effort args

            currentDeg =  
                case maybeCurrdeg of Just currentDeg2 -> currentDeg2; _ -> startDeg
   
            newtruevol = truevol + (boxVolume ppb)    

            (L.TVMUndecided undecidedSimplerForm undecidedMeasure _) = value
            undecidedMeasureImproved = 
                case maybePrevMeasure of
                    Nothing -> True
                    Just prevUndecidedMeasure ->
                        prevUndecidedMeasure / undecidedMeasure > improvementRatioThreshold

    
            {- functions related to splitting the box -}
    
            (splitSuccess, splitVar, (boxL,boxR))
                = L.split thinVarsMaybeNonIntVars maybeVar ppb value
            maybeVar =
                Nothing
--                case maybeSplitGuessingLimit of
--                    Nothing -> Nothing
--                    _ -> Just $ advanceVar thinVarsMaybeNonIntVars prevSplitVar
--                where
--                advanceVar forbiddenVars var
--                    | allVarsForbidden = var
--                    | newVar `elem` forbiddenVars = advanceVar forbiddenVars newVar
--                    | otherwise = newVar
--                    where
--                    allVarsForbidden = length forbiddenVars == dim
--                    newVar = (var + 1) `mod` dim
--            maybeSplitGuessingLimit = case splitGuessing args of -1 -> Nothing; n -> Just n
    
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
                
            (thinVars, thickVars) = boxThinThickVars ppb
    

            {- processing of a split box -}
    
            bisectAndRecur maybeSimplerForm newBoxes isSimpleSplit splitVar2 =
                case (order args) of
                    DFS -> bisectAndRecurDFS
--                    DFSthenBFS -> bisectAndRecurDFS
                    BFS -> bisectAndRecurBFS
--                    BFSFalsifyOnly -> bisectAndRecurBFS
                where
                bisectAndRecurBFS =
                    pavingLoop
                        (max (depth+1) maxDepthReached) 
                        (queueTail Q.>< (Q.fromList $ map prepareBox newBoxes)) 
                        newMaxQLength 
                        (computedboxes+1) 
                        newproblemvol 
                        truevol 
                        Nothing Nothing
                bisectAndRecurDFS =
                    pavingLoop
                        (max (depth+1) maxDepthReached) 
                        ((Q.fromList $ map prepareBox newBoxes) Q.>< queueTail) 
                        newMaxQLength 
                        (computedboxes+1) newproblemvol truevol 
                        Nothing Nothing
                newMaxQLength = max newQLength maxQLengthReached
                newQLength = qlength - 1 + newBoxesLength
                newBoxesLength = length newBoxes
                prepareBox ppb2 =
                    BoxToDo
                    {
                        boxToDo_depth = depth+1,
                        boxToDo_queueSize = newQLength,
                        boxToDo_startDeg = newstartDeg,
                        boxToDo_form = maybeSimplerForm,
                        boxToDo_prevSplitVar = splitVar2,
                        boxToDo_ppb = ppb2
                    }
                newstartDeg =
                    (originalStartDeg + currentDeg) `div` 2

                newproblemvol
                    | isSimpleSplit = problemvol
                    | otherwise
                        =
                        case newBoxesLength of
                            2 -> problemvol -- no dropping of boxes - a clean split
                            _ -> problemvol - (boxVolume ppb) + (sum $ map boxVolume newBoxes)


            {- functions related to reporting of progress: -}
    
            reportBoxInitSplit =
                reportProgress False "Not reached minimum depth, splitting." False True (Just SM.Nothing)
            reportBoxStart =
                reportProgress False "Examining another box." True False Nothing
            reportBoxProved =
                reportProgress True "Formula proved on this box." False True (Just (SM.Just True))
            reportBoxDisproved =
                reportProgress True "Formula DISPROVED on this box." False False (Just (SM.Just False))
            reportBoxSplit =
                reportProgress True "Formula undecided on this box, splitting." False True (Just SM.Nothing)
            reportRaiseDegree =
                reportProgress True ("Raising polynomial degree to " ++ show (currentDeg + 1) ++ ".") False False Nothing

            reportProgress takeCurrentBoxIntoAccount message reportBox reportState maybeBoxResult  =
                do
                currtime <- getCPUTime 
                atomically $ mapM_ (writeToChan currtime) outChannels
                where
                writeToChan currtime outChannel = 
                    writeTBQueue outChannel $
                        Left PaverProgress
                        {
                            paverProgress_message = message,
                            paverProgress_durationInPicosecs = currtime - inittime,
                            paverProgress_maybeState = maybeState, 
                            paverProgress_maybeCurrentBoxToDo = maybeBoxToDo,
                            paverProgress_maybeNewBoxDone = maybeNewBoxDone  
                        }
                maybeState 
                    | reportState = SM.Just $ pavingState takeCurrentBoxIntoAccount
                    | otherwise = SM.Nothing
                maybeBoxToDo
                    | reportBox = SM.Just boxToDo
                    | otherwise = SM.Nothing
                maybeNewBoxDone =
                    case maybeBoxResult of
                        Just boxResult -> SM.Just (ppb SP.:!: boxResult)
                        _ -> SM.Nothing
        
            pavingState takeCurrentBoxIntoAccount =
                PavingState
                {
                    pavingState_trueFraction = provedFraction takeCurrentBoxIntoAccount,
                    pavingState_computedBoxes = computedboxesMaybePlusOne,
                    pavingState_maxQLengthReached = maxQLengthReached,
                    pavingState_maxDepthReached = maxDepthReached
                }
                where
                computedboxesMaybePlusOne
                    | takeCurrentBoxIntoAccount = computedboxes + 1
                    | otherwise = computedboxes

            {- functions related to terminating and composing result: -}
    
            stopPaverProved =
                stopPaver False (Right True)
            stopPaverDisproved =
                stopPaver False (Right False)
            stopPaverGiveUp takeCurrentBoxIntoAccount message  =
                stopPaver takeCurrentBoxIntoAccount (Left message)
            stopPaver takeCurrentBoxIntoAccount formTruthOrMessage =
                do
                currtime <- getCPUTime 
                atomically $ mapM_ (writeToChan currtime) outChannels
                where
                writeToChan currtime outChannel = 
                    writeTBQueue outChannel $
                        Right PaverResult
                        {
                            paverResult_formTruthOrMessage = formTruthOrMessage,
                            paverResult_lastPPB = ppb,
                            paverResult_durationInPicosecs = currtime - inittime,
                            paverResult_state = pavingState takeCurrentBoxIntoAccount
                        }
                -- end of the imperative program tryToDecideFormOnBoxByPaving, usually end of thread

            provedFraction takeCurrentBoxIntoAccount
                | problemvol `RA.equalReals` 0 == Just True = 1
                | otherwise = ((oldornewtruevol takeCurrentBoxIntoAccount) / problemvol)
            oldornewtruevol takeCurrentBoxIntoAccount
                | takeCurrentBoxIntoAccount =
                    if decidedAndTrue then newtruevol else truevol
                | otherwise = truevol
                
        