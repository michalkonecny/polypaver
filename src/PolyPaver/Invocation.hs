{-# LANGUAGE DeriveDataTypeable #-}

{-|
    Module      :  PolyPaver.Invocation
    Description :  user interface for solving PolyPaver problems 
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    User interface for solving PolyPaver problems.
-}
module PolyPaver.Invocation
(
    defaultMain,
    batchMain,
    getTightnessValues,
    Problem(..),
    reportCmdLine
)
where

import PolyPaver.Args
import PolyPaver.APBox
import PolyPaver.Form
import PolyPaver.ProverLoop
import PolyPaver.Vars

import qualified PolyPaver.Plot as Plot

--import Numeric.ER.BasicTypes.DomainBox.IntMap
--import Numeric.ER.Real.DefaultRepr
import Numeric.ER.Real.Base
import Numeric.ER.Real.Base.MachineDouble
import qualified Numeric.ER.Real.Approx as RA

--import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import Data.List (intercalate)

import qualified Data.Strict.Maybe as SM
import qualified Data.Strict.Tuple as SP

import Data.Typeable
--import Data.Data

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
import Control.Concurrent (forkIO)

import System.Environment (getArgs, getProgName)
import System.Console.CmdArgs (cmdArgs)
--import System.CPUTime
import System.IO

data Problem = Problem
    {
        problem_box :: 
            [(Int,
              (Rational,Rational),
               Bool)], -- is the variable restricted to integers?
        problem_form :: Form ()
    }
    deriving (Show,Read,Typeable)


getTightnessValues :: Args -> [Integer]
getTightnessValues args =
    parse $ tightnessValues args
    where
    parse ('2' : '^' : s) = map (2^) $ parse2 s
    parse s = parse2 s
    parse2 :: String -> [Integer]
    parse2 s =
        case reads s of
            [(t,"")] -> [t] -- a single number
            [(tL,'.' : '.' : rest)] -> -- a range?
                case reads rest of
                  [(tR,"")] -> 
                    if tL <= tR then [tL..tR] else [tR,(tR-1)..tL]
                  _ -> 
                    case reads ("[" ++ s ++ "]") of -- try whether it is a comma-separated list
                        [(ts, "")] -> ts
                        _ -> parseError  
            _ -> parseError
        where
        parseError = error $ "Failed to parse argument of i: " ++ s

defaultMain :: Problem -> IO PaverResult
defaultMain problem = 
    do
    reportCmdLine
    argsPre <- cmdArgs paverDefaultArgs
    let args = setDefaults argsPre
    case checkArgs args of
        [] -> runPaverReportingProgress problem args
        msgs -> 
            do
            mapM_ putStrLn msgs
            error "The above errors have been identified in the command-line arguments."

batchMain :: 
    (Args -> [String] -> IO [(String, Problem)]) -> 
    IO ()
batchMain problemFactory =
    do
    reportCmdLine
    argsPre <- cmdArgs paverDefaultArgs
    let args = setDefaults argsPre
    case checkArgs args of
        [] -> 
            do
            let problemIdOpt = problemId args
            problems <- problemFactory args problemIdOpt
            results <- mapM (runProblem args) problems
            putStr "\n>>>>>>>>>>> SUMMARY <<<<<<<<<<<"
            _ <- mapM printSummaryLine $ zip problems results
            putStrLn ""
            return ()
        msgs -> 
            do
            mapM_ putStrLn msgs
            error "The above errors have been identified in the command-line arguments."
    where
    printSummaryLine ((name, _problem), result) =
        putStr $ "\n" ++ name ++ ": " ++ showPaverResultOneLine result
    runProblem args (name, problem)
        =
        do
        putStrLn banner
        putStrLn $ "*** applying PolyPaver on conjecture " ++ name
        putStrLn banner
        putStrLn $ show (problem_form problem)
        putStrLn banner
        putStrLn $ showForm 5000 const (problem_form problem)
        putStrLn banner
        runPaverReportingProgress problem args
    banner = replicate 100 '*'
    

reportCmdLine :: IO ()
reportCmdLine
    =
    do
    rawargs <- getArgs
    progName <- getProgName
    putStrLn $ "command line: " ++ progName ++ " " ++ intercalate " " rawargs
    
    
runPaverReportingProgress :: 
    Problem -> 
    Args -> 
    IO PaverResult
runPaverReportingProgress problem args =
    do
    initMachineDouble -- set FPU to round upwards
    hSetBuffering stdout NoBuffering -- print progress in real time, not in batches
    progressChannel <- atomically $ newTBQueue 10
    progressChannelsPlot <- case shouldPlot of
        True -> 
            do
            progressChannelPlot <- atomically $ newTBQueue 10
            _ <- forkIO (startPlotter progressChannelPlot)
            return [progressChannelPlot]
        False ->
            return []
    _ <- forkIO $ paverOnThisProblem $ progressChannel : progressChannelsPlot
    printProgress progressChannel
    where
    paverOnThisProblem progressChannels =
        tryToDecideFormOnBoxByPaving
            progressChannels
            args
            form -- the formula that needs deciding
            initbox
    form = sortHypothesesBySize $ problem_form problem
    varNames = getFormVarNames form
    initbox = boxFromIntervals varIsInts varNames boxBounds 
    varIsInts = IMap.fromList $ map (\(var,_,ii) -> (var, ii)) boxBoundsIsInts
    boxBounds = map (\(var,bounds,_) -> (var,bounds)) boxBoundsIsInts
    boxBoundsIsInts = problem_box problem
    
    printProgress progressChannel =
        monitorLoop progressChannel printReport
        where
        printReport maybePrevState progressOrResult =
            do
            putStr $ format maybePrevState progressOrResult
        format 
            | quiet args = showProgressOrResult 0
            | verbose args = showProgressOrResult 2
            | otherwise = showProgressOrResult 1
            

    shouldPlot = dim == 2 && w > 0 && h > 0
    dim = length boxBounds
    w = plotWidth args
    h = plotHeight args
    startPlotter progressChannel =
        do
        stateTV <- Plot.initPlot initbox w h
        _ <- monitorLoop progressChannel (plotBox stateTV)
        return ()
        where
        plotBox stateTV _ (Left progress) =
            case paverProgress_maybeNewBoxDone progress of
                SM.Just (ppb SP.:!: maybeTruth) ->
                    do
                    Plot.addBox stateTV colour ppb
                    return ()
                    where
                    colour =
                        case maybeTruth of
                            SM.Just False -> red
                            SM.Just True -> green
                            SM.Nothing -> yellow
                _ -> return () 
        plotBox _ _ _ = return ()
        green = (0.1,0.6,0.1,0.4)
        red = (0.6,0.1,0.1,1)
        yellow = (0.6,0.6,0.1,0.05)

monitorLoop :: 
    TBQueue (Either PaverProgress result) -> 
    (Maybe (PavingState Double) -> Either PaverProgress result -> IO ()) -> 
    IO result
monitorLoop progressChannel handleNextReport =
    aux Nothing
    where
    aux maybePrevState =
        do
        progressOrResult <- atomically $ readTBQueue progressChannel
        handleNextReport maybePrevState progressOrResult
        case progressOrResult of
            Left _ -> aux $ updatedMaybePrevState progressOrResult
            Right result -> return result
        where
        updatedMaybePrevState progressOrResult =
            case progressOrResult of
                (Left progress) ->
                    case paverProgress_maybeState progress of
                        SM.Just state -> Just state
                        _ -> maybePrevState
                _ -> maybePrevState




showProgressOrResult :: 
    Int ->
    Maybe (PavingState Double) -> 
    Either PaverProgress PaverResult -> 
    String
showProgressOrResult _ _ (Right result) = 
    showPaverResult result
showProgressOrResult verbosity maybePrevState (Left progress) 
    | verbosity == 0 = ""
    | verbosity == 1 = showPaverProgressMini maybePrevState progress
    | otherwise = showPaverProgress progress

showPaverResult :: PaverResult -> String
showPaverResult result =
    banner ++
    outcomeS ++
    " in " ++ showDuration durationInPicoseconds ++ "." ++
    stateS
    where
    banner = take 100 $ "\n^^^^ time = " ++ showDuration durationInPicoseconds ++ repeat '^'
    outcomeS = 
        case outcome of
            Right True -> "\nConjecture proved TRUE" 
            Right False -> "\nConjecture shown FALSE on " ++ showBox lastPPB
            Left message -> "\nGave up on deciding conjecture: " ++ message 
    outcome = paverResult_formTruthOrMessage result
    durationInPicoseconds = paverResult_durationInPicosecs result
    state = paverResult_state result
    lastPPB = paverResult_lastPPB result
    stateS = showState state

showPaverProgress :: PaverProgress -> String
showPaverProgress progress =
    banner ++
    messageS ++
    boxS ++
    stateS
    where
    banner = take 100 $ "\n**** time = " ++ showDuration durationInPicoseconds ++ repeat '*'
    durationInPicoseconds = paverProgress_durationInPicosecs progress
    messageS = "\n" ++ paverProgress_message progress
    boxS = 
        case paverProgress_maybeCurrentBoxToDo progress of
            SM.Just currentBoxToDo -> showB currentBoxToDo
            _ -> ""
    showB currentBoxToDo =
        "\n" ++ showBox ppb
        ++ "(depth=" ++ show depth ++ ", queue=" ++ show queueSize ++ ")"            
        where
        depth = boxToDo_depth currentBoxToDo
        queueSize = boxToDo_queueSize currentBoxToDo
        ppb = boxToDo_ppb currentBoxToDo
    stateS = 
        case paverProgress_maybeState progress of
            SM.Just state -> showState state
            _ -> ""

showPaverProgressMini ::
    Maybe (PavingState Double) -> 
    PaverProgress -> 
    String
showPaverProgressMini maybePrevState progress =
    case paverProgress_maybeState progress of
        SM.Nothing -> ""
        SM.Just state ->
            case maybePrevState of
                (Just prevState) | differentTrueFraction prevState ->
                    miniReport
                _ -> ""
            where
            differentTrueFraction prevState =
                prevTrueFractionR < trueFractionR
                where
                (_,prevTrueFractionR) = RA.bounds prevTrueFraction
                (_,trueFractionR) = RA.bounds trueFraction
                prevTrueFraction = pavingState_trueFraction prevState
            trueFraction = pavingState_trueFraction state
            miniReport =
                "\n" ++ showDuration durationInPicoseconds ++ "> proved faction = " ++ show trueFraction
            durationInPicoseconds = paverProgress_durationInPicosecs progress

showState :: 
    ERRealBase b =>
    PavingState b -> [Char]
showState state =
    "\nProved fraction: " ++ show provedFraction ++ 
    "\nComputed boxes: " ++ show computedboxes ++ 
    "\nGreatest queue size: " ++ show maxQLengthReached ++  
    "\nGreatest depth: " ++ show maxDepthReached
    where
    computedboxes = pavingState_computedBoxes state
    maxQLengthReached = pavingState_maxQLengthReached state
    maxDepthReached = pavingState_maxDepthReached state
    provedFraction = pavingState_trueFraction state

showDuration :: Integer -> String
showDuration durationInPicoseconds =
    show durationInSeconds ++ "." ++ millisS ++ "s (" ++ show days ++ "d, " ++ show hours ++ "h, " ++ show mins ++ "min, " ++ show secs ++ "s)" 
    where
    millisS 
        | length s == 1 = "0" ++ s
        | otherwise = s
        where
        s = show millis
    durationInSeconds = durationInPicoseconds `div` 1000000000000
    millis = (durationInPicoseconds `mod` 1000000000000) `div` 10000000000
    (minsAll, secs) = quotRem durationInSeconds 60
    (hoursAll, mins) = quotRem minsAll 60
    (days, hours) = quotRem hoursAll 24

showPaverResultOneLine :: PaverResult -> String
showPaverResultOneLine result =
    outcomeS ++
    " in " ++ showDuration durationInPicoseconds ++ " with" ++
    " (size = " ++ show computedboxes ++ 
    ", queue = " ++ show maxQLengthReached ++  
    ", depth = " ++ show maxDepthReached ++ ")"
    where
    outcomeS = 
        case outcome of
            Right True -> "Conjecture proved TRUE" 
            Right False -> "Conjecture shown FALSE"
            Left message -> "Gave up on deciding conjecture: " ++ message 
    outcome = paverResult_formTruthOrMessage result
    durationInPicoseconds = paverResult_durationInPicosecs result
    state = paverResult_state result
    computedboxes = pavingState_computedBoxes state
    maxQLengthReached = pavingState_maxQLengthReached state
    maxDepthReached = pavingState_maxDepthReached state 

    