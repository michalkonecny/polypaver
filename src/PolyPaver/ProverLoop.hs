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
import PolyPaver.PPBox
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

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan

import System.CPUTime

data PaverResult =
    PaverResult
    {
        paverResult_formTruthOrMessage :: Either String Bool,
        paverResult_lastPPB :: PPBox Double, 
        paverResult_durationInPicosecs :: Integer,
        paverResult_state :: PavingState Double
    }
--    deriving (Data,Typeable)
    
data PaverProgress =
    PaverProgress
    {
        paverProgress_message :: String,
        paverProgress_durationInPicosecs :: Integer,
        paverProgress_maybeState :: Maybe (PavingState Double),
        paverProgress_maybeCurrentBoxToDo :: Maybe (BoxToDo Double),
        paverProgress_maybeNewBoxDone :: Maybe (PPBox Double, Maybe Bool, Form (Maybe (IRA BM)))
    }
    
data PavingState b =
    PavingState
    {
        pavingState_trueFraction :: IRA b,
        pavingState_computedBoxes :: Int,
        pavingState_maxQLengthReached :: Int,
        pavingState_maxDepthReached :: Int 
    }
    
data BoxToDo b =
    BoxToDo
    {
        boxToDo_depth :: Int,
        boxToDo_skewAncestors :: [PPBox b],
        boxToDo_startDeg :: Int,
        boxToDo_form :: Form (),
        boxToDo_prevSplitVar :: Int,
        boxToDo_ppb :: PPBox b    
    }

{-|
   Attempt to decide @form@ over @box@ by evaluating @form@ over various sub-boxes of @box@.
   These sub-boxes are obtain by adaptive bisection and (optionally) skewing in the direction
   of hyperplanes that approximate the surfaces given by the expressions in the formula.
   
   The program sends several 'PaverProgress' records on the @out@ channel for each box it evaluates.
   It also sends one 'PaverResult' record when it is finished with paving.
-}
tryToDecideFormOnBoxByPaving :: 
    TChan (Either PaverProgress PaverResult) {-^ @out@ -} ->
    Args {-^ @args@ - A record with various parameters -} -> 
    Form  () {-^ @form@ - A logical formula -} ->
    PPBox Double {-^ @box@ - A rectangle (possibly skewed) in R^n -} -> 
    IO ()
tryToDecideFormOnBoxByPaving
    outChannel
    args
    originalForm 
    initppb@(_, initbox, varIsInts, _varNames)
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
    
    pave inittime = -- continue with inittime available in the scope
        pavingLoop 
        -- pavingLoop processes first box in a given queue and calls itself with an updated queue
            -- the following values change from box to box: 
            (0 :: Int) -- maxDepthReached
            (Q.singleton firstBoxToDo) -- initial queue with one box only
            (1 :: Int) -- greatest computed queue size
            (0 :: Int) -- number of computed boxes
            (ppVolume initppb) -- initial volume
            0 -- volume of proved boxes
            Nothing Nothing
        where
        firstBoxToDo =
            BoxToDo 
            {
                boxToDo_depth = 0,
                boxToDo_skewAncestors = [],
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
                    BFSFalsifyOnly -> 
                        stopPaverGiveUp False "Problem proved true while trying to falsify it." 
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
                    bisectAndRecur formRaw [boxLNoHP, boxRNoHP] True splitVarNoHP
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
                    bisectAndRecur undecidedMaybeSimplerForm [boxL, boxR] False splitVar
    

            BoxToDo depth skewAncestors startDeg formRaw prevSplitVar ppb@(_skewed, box, _, _) = boxToDo 
                -- beware: "formRaw" above has ranges left over in it from a parent box - do not show them
            boxToDo = Q.index queue 0
            queueTail = Q.drop 1 queue


            {- definitions related to evaluating the formula over the box -}
    
            (decided, decidedAndTrue) = 
                case maybeFormTruth of
                    Just True -> (True, True)
                    Just _ -> (True, False)
                    _ -> (False, False)
                where
                maybeFormTruth = L.decide value
            (value, formWithRanges) =
                evalForm 
                    currentDeg (maxSize args) ix minIntegrationStepSize ppb 
                    formRaw
                where
                minIntegrationStepSize = 2^^(- (minIntegrExp args))
                ix = fromInteger $ toInteger $ effort args

            currentDeg =  
                case maybeCurrdeg of Just currentDeg2 -> currentDeg2; _ -> startDeg
   
            newtruevol = truevol + (ppVolume ppb)    

            undecidedMaybeSimplerForm
                =
                case maybeHP of
                    Nothing -> undecidedSimplerForm
                    _ -> originalForm
                        -- when skewing, the new boxes are not sub-boxes of box and thus we cannot
                        -- rely on the simplification of form performed while evaluating it over box

            (L.TVMUndecided undecidedSimplerForm undecidedMeasure _ _) = value
            undecidedMeasureImproved = 
                case maybePrevMeasure of
                    Nothing -> True
                    Just prevUndecidedMeasure ->
                        prevUndecidedMeasure / undecidedMeasure > improvementRatioThreshold

    
            {- functions related to splitting the box -}
    
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
    

            {- processing of a split box -}
    
            bisectAndRecur maybeSimplerForm newBoxes isSimpleSplit splitVar2 =
                case (order args) of
                    DFS -> bisectAndRecurDFS
                    DFSthenBFS -> bisectAndRecurDFS
                    BFS -> bisectAndRecurBFS
                    BFSFalsifyOnly -> bisectAndRecurBFS
                where
                bisectAndRecurBFS =
                    pavingLoop
                        (max (depth+1) maxDepthReached) 
                        (queueTail Q.>< (Q.fromList $ map prepareBox newBoxes2)) 
                        newMaxQLength 
                        (computedboxes+1) 
                        newproblemvol 
                        truevol 
                        Nothing Nothing
                bisectAndRecurDFS =
                    pavingLoop
                        (max (depth+1) maxDepthReached) 
                        ((Q.fromList $ map prepareBox newBoxes2) Q.>< queueTail) 
                        newMaxQLength 
                        (computedboxes+1) newproblemvol truevol 
                        Nothing Nothing
                newMaxQLength = max newQLength maxQLengthReached
                newQLength = qlength - 1 + newBoxes2length
                newBoxes2length = length newBoxes2
                newBoxes2
                    = filter intersectsAllSkewAncestors newBoxes
                    where
                    intersectsAllSkewAncestors ppb2
                        = and $ map couldIntersect skewAncestors
                        where
                        couldIntersect ancestor
                            = ppIntersect ancestor ppb2 /= Just False
                prepareBox ppb2 =
                    BoxToDo
                    {
                        boxToDo_depth = depth+1,
                        boxToDo_skewAncestors = newSkewAncestors,
                        boxToDo_startDeg = newstartDeg,
                        boxToDo_form = maybeSimplerForm,
                        boxToDo_prevSplitVar = splitVar2,
                        boxToDo_ppb = ppb2
                    }
                newstartDeg =
                    (originalStartDeg + currentDeg) `div` 2

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


            {- functions related to reporting of progress: -}
    
            reportBoxInitSplit =
                reportProgress False "Not reached minimum depth, splitting." False True (Just Nothing)
            reportBoxStart =
                reportProgress False "Evaluating over a new box." True True Nothing
            reportBoxProved =
                reportProgress True "Formula proved on this box." False True (Just (Just True))
            reportBoxSplit =
                reportProgress True "Formula undecided on this box, splitting." False True (Just Nothing)
            reportRaiseDegree =
                reportProgress True ("Raising polynomial degree to " ++ show (currentDeg + 1) ++ ".") False False Nothing

            reportProgress takeCurrentBoxIntoAccount message reportBox reportState maybeBoxResult  =
                do
                currtime <- getCPUTime 
                atomically $ writeTChan outChannel $
                    Left PaverProgress
                    {
                        paverProgress_message = message,
                        paverProgress_durationInPicosecs = currtime - inittime,
                        paverProgress_maybeState = maybeState, 
                        paverProgress_maybeCurrentBoxToDo = maybeBoxToDo,
                        paverProgress_maybeNewBoxDone = maybeNewBoxDone  
                    }
                where
                maybeState 
                    | reportState = Just $ pavingState takeCurrentBoxIntoAccount
                    | otherwise = Nothing
                maybeBoxToDo
                    | reportBox = Just $ boxToDo { boxToDo_form = formRaw }
                    | otherwise = Nothing
                maybeNewBoxDone =
                    case maybeBoxResult of
                        Just boxResult -> Just (ppb, boxResult, formWithRanges)
                        _ -> Nothing
        
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
                atomically $ writeTChan outChannel $
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
                
        