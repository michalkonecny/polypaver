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
    module PolyPaver.Form
)
where

import PolyPaver.Args
import PolyPaver.PPBox
import PolyPaver.Form
import PolyPaver.ProverLoop
import PolyPaver.Vars

import Numeric.ER.BasicTypes.DomainBox.IntMap
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.Real.Base.MachineDouble
import qualified Numeric.ER.Real.Approx as RA

import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import Data.List (intercalate)

import qualified Data.Sequence as Q
import Data.Typeable
import Data.Data

import System.Environment (getArgs, getProgName)
import System.Console.CmdArgs (cmdArgs)
import System.CPUTime
import System.IO

data Problem = Problem
    {
        box :: 
            [(Int,
              (Rational,Rational),
               Bool)], -- is the variable restricted to integers?
        conjecture :: Form
    }
    deriving (Show,Read,Typeable)


getTightnessValues :: IO [Integer]
getTightnessValues =
    do
    argsPre <- cmdArgs paverDefaultArgs
    let args = setDefaults argsPre
    return $ parse $ tightnessValues args
    where
    parse ('2' : '^' : s) = map (2^) $ parse2 s
    parse s = parse2 s
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

defaultMain problem = 
    do
    reportCmdLine
    argsPre <- cmdArgs paverDefaultArgs
    let args = setDefaults argsPre
    case checkArgs args of
        [] -> runPaver problem args
        msgs -> 
            do
            mapM_ putStrLn msgs
            error $ "The above errors have been identified in the command-line arguments."

batchMain problemFactory =
    do
    reportCmdLine
    argsPre <- cmdArgs paverDefaultArgs
    let args = setDefaults argsPre
    case checkArgs args of
        [] -> 
            do
            let problemIdOpt = problemId args
            problems <- problemFactory problemIdOpt
            results <- mapM (runProblem args) problems
            putStrLn ">>>>>>>>>>> SUMMARY <<<<<<<<<<<"
            mapM printSummaryLine $ zip problems results
        msgs -> 
            do
            mapM_ putStrLn msgs
            error $ "The above errors have been identified in the command-line arguments."
    where
    printSummaryLine ((name, _problem), result) =
        putStrLn $ name ++ ": " ++ show result
    runProblem args (name, problem)
        =
        do
        putStrLn banner
        putStrLn $ "*** applying PolyPaver on conjecture " ++ name
        putStrLn banner
        runPaver problem args
    banner = replicate 100 '*'
    

reportCmdLine
    =
    do
    rawargs <- getArgs
    progName <- getProgName
    putStrLn $ "command line: " ++ progName ++ " " ++ (intercalate " " rawargs)
    
    
runPaver problem args =
    do
    initMachineDouble -- set FPU to round upwards
    hSetBuffering stdout LineBuffering -- print progress in real time, not in batches
    solveAndReportOnConsole args
        conj -- formula to be decided
        initbox
    where
    conj = conjecture problem
    varNames = getFormVarNames conj
    initbox = ppBoxFromIntervals varIsInts varNames boxBounds 
    varIsInts = IMap.fromList $ map (\(var,_,ii) -> (var, ii)) boxBoundsIsInts
    boxBounds = map (\(var,bounds,_) -> (var,bounds)) boxBoundsIsInts
    boxBoundsIsInts = box problem
    quietOpt = quiet args
    verboseOpt = verbose args
        
