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
import Data.Maybe (catMaybes)

import qualified Data.Sequence as Q
import System.Environment (getArgs, getProgName)
import System.Console.CmdArgs
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

data PolyPaver = PolyPaver 
    {problemId :: [String]
    ,tightnessValues :: String
    ,startDegree :: Int
    ,degree :: Int
    ,maxSize :: Int
    ,effort :: Int
    ,minIntegrExp :: Int
    ,order :: Order
    ,splitIntFirst :: Bool
    ,minDepth :: Int
    ,maxDepth :: Int
    ,maxQueueLength :: Int
    ,time :: Int
----    ,epsrelbits :: Int
----    ,epsabsbits :: Int
    ,boxSkewing :: Bool
    ,splitGuessing :: Int
    ,quiet :: Bool
    ,verbose :: Bool
    ,plotWidth :: Int
    ,plotHieght :: Int
    }
    deriving (Show,Data,Typeable)

paver =
    PolyPaver 
    {problemId = [] &= args &= typ "PROBLEM_ID" 
    ,tightnessValues = "1" &= name "i" 
        &= groupname "Problem parameters"
        &= help "value(s) of T to try (if the formula has an unbound variable T) (eg \"2^0..10\" or \"1..10\" or \"1,10,100\") (default = 1)" 
    ,startDegree = -1 &= name "s" &= help "first polynomial degree to try on each box (default = degree)"
        &= groupname "Box solving effort"
    ,degree = 0 &= name "d" &= help "maximum polynomial degree (default = 0)" 
    ,maxSize = 100 &= name "z" &= help "maximum polynomial term size (default = 100)"
    ,effort = 10 &= help "for approximating point-wise sqrt and exp (default = 10)" 
    ,minIntegrExp = 0 &= name "I" &= help "n to compute approximate integration step using 2^(-n)" 
    ,order = DFSthenBFS 
        &= groupname "Box subdivision strategy"
        &= help "sub-problem processing order, bfs for breadth-first or dfs for depth-first, (default = DFSthenBFS)"
    ,splitIntFirst = False &= name "f" 
        &= help "whether to split integer valued domains until they are exact before splitting the continuous domains"
    ,minDepth = 0 &= help "minimum bisection depth (default = 0)"
    ,maxDepth = 1000 &= name "b" &= help "maximum bisection depth (default = 1000)"
    ,maxQueueLength = -1 &= name "u" 
        &= help ("maximum queue size (default = " 
                    ++ show maxQueueLengthDefaultDFS ++ " for depth-first and "
                    ++ show maxQueueLengthDefaultBFS ++ " for breadth-first order)")
    ,time = 7*24*3600 &= help "timeout in seconds (default = 7*24*3600 ie 1 week)"    
    ,boxSkewing = False &= name "k" &= help "allow parallelepiped boxes, by default only coaxial rectangles" 
        &= groupname "Experimental"
    ,splitGuessing = -1 &= name "g" &= opt (20 :: Int) &= help "try guessing the best box splitting direction but do not allow a box in which a pair of box edge lengths exceeds a given ratio (default 20)"
--    ,epsrelbits = 23 &= name "r" &= help "n to compute machine epsilon using 2^-n (default = 24)" &= groupname "Floating point rounding interpretation in conjectures"
--    ,epsabsbits = 126 &= name "a" &= help "n to compute denormalised epsilon using 2^-n (default = 126)"
    ,quiet = False &= help "suppress all output except the final result (default off)" 
        &= groupname "Verbosity"
    ,verbose = False &= help "output extra details while paving (default off)"
    ,plotWidth = 0 &= name "w" &= help "plot width for 2D problems, 0 mean no plotting (default)" 
        &= groupname "Plotting"
    ,plotHieght = 0 &= name "h" &= help "plot height for 2D problems, 0 mean no plotting (default)"
    } 
    &= help (unlines 
                ["Tries to decide numerical conjectures (problems) using polynomial enclosures.",
                 "[PROBLEM_ID] specifies one or more conjectures as follows:                   ",
                 "  <name>.vc [<conclusion number>]: like a VC in SPARK .siv                   ",
                 "  <name>.siv [<vc name> [<conclusion number>]]: SPARK-generated VCs          ",
                 "  <name>.form: using internal syntax (machine generated)                     ", 
                 "  <name>.hs [<identifier name>]: Haskell constant of type Problem            "])
    &= summary "PolyPaver 0.2 (c) 2011, 2013 Jan Duracz and Michal Konecny (Aston University)"
    &= name "polypaver"

setDefaults :: PolyPaver -> PolyPaver
setDefaults = setMaxQLength . setStartDegree
    where
    setMaxQLength args =
        case maxQueueLength args == -1 of
            False -> args -- maxQueueLength is explicitly set, do no change
            True -> 
                case order args of
                    DFS -> args { maxQueueLength = maxQueueLengthDefaultDFS }
                    BFS -> args { maxQueueLength = maxQueueLengthDefaultBFS }
                    DFSthenBFS -> args { maxQueueLength = maxQueueLengthDefaultDFS }
                    BFSFalsifyOnly -> args { maxQueueLength = maxQueueLengthDefaultBFS }
    setStartDegree args 
        | startDegree args == -1 = args { startDegree = degree args }
        | otherwise = args 

maxQueueLengthDefaultDFS = 50
maxQueueLengthDefaultBFS = 5000

checkArgs args =
    catMaybes $ [checkSplitGuessing, checkSkewing]
    where
    checkSkewing
        | boxSkewing args && startDegree args == 0 =
            Just $
                 "Box skewing is not compatible with polynomial degree 0."
                 ++ "\n  Raise starting polynomial degree to a positive value."
        | otherwise = Nothing
    checkSplitGuessing 
        | splitGuessing args /= -1 && startDegree args == 0 =
            Just $
                 "Guessing split direction is not compatible with polynomial degree 0."
                 ++ "\n  Raise starting polynomial degree to a positive value."
        | otherwise = Nothing
        

getTightnessValues :: IO [Integer]
getTightnessValues =
    do
    argsPre <- cmdArgs paver
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
    argsPre <- cmdArgs paver
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
    argsPre <- cmdArgs paver
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
    initMachineDouble -- round upwards
    hSetBuffering stdout LineBuffering -- print progress in real time, not in batches
    solveAndReportOnConsole
        plotSizesOpt
        plotStepDelayMs
        ordr -- sub-problem processing order
        report -- level of verbosity
--        epsrelbitsOpt
--        epsabsbitsOpt 
        boxSkewingOpt
        splitGuessingOpt
        splitIntFirstOpt
        startdeg -- maximum polynomial degree for the first attempt
        maxdeg -- maximum polynomial degree to try
        improvementRatioThreshold -- when to try raising degree/effort and when to give up and split
        maxsize -- maximum number of terms in a polynomial
        mindepth -- minimum bisection depth
        maxdepth -- maximum bisection depth
        maxQLength -- maximum queue length
        ix -- effort index for AERN
        minIntegrationStepSize -- approximate step to use in piecewise numerical integration
        maxtime -- timeout
        conj -- formula to be decided, defined in IntegralTest
        initbox
    where
    maxdeg = degree args
    startdeg = case startDegree args of s | s == -1 -> maxdeg; s -> s
    minIntegrationStepSize = 2^^(- (minIntegrExp args))
    improvementRatioThreshold = 1.2
    maxsize = maxSize args 
    maxtime = toInteger $ time args
    ix = fromInteger $ toInteger $ effort args
    mindepth = minDepth args 
    maxdepth = maxDepth args 
    maxQLength = maxQueueLength args 
    conj = conjecture problem
    varNames = getFormVarNames conj
    initbox = ppBoxFromIntervals varIsInts varNames boxBounds 
    varIsInts = IMap.fromList $ map (\(var,_,ii) -> (var, ii)) boxBoundsIsInts
    boxBounds = map (\(var,bounds,_) -> (var,bounds)) boxBoundsIsInts
    boxBoundsIsInts = box problem
    ordr = order args 
    quietOpt = quiet args
    verboseOpt = verbose args
    report = if quietOpt then ReportNONE else if verboseOpt then ReportALL else ReportNORMAL
--    epsrelbitsOpt = epsrelbits args 
--    epsabsbitsOpt = epsabsbits args 
    splitGuessingOpt = case splitGuessing args of -1 -> Nothing; n -> Just n
    splitIntFirstOpt = splitIntFirst args
    boxSkewingOpt = boxSkewing args
    plotSizesOpt = (plotWidth args, plotHieght args)
    plotStepDelayMs = 0
        
