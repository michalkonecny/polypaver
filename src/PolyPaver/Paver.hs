{-# LANGUAGE DeriveDataTypeable #-}

{-|
    Module      :  PolyPaver.Paver
    Description :  user interface for solving PolyPaver problems 
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    User interface for solving PolyPaver problems.
-}
module PolyPaver.Paver
(
    defaultMain,Problem(..),module PolyPaver.Form,readBox
)
where

import PolyPaver.PPBox
import PolyPaver.Form
import PolyPaver.Solver

import Numeric.ER.BasicTypes.DomainBox.IntMap
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.Real.Base.MachineDouble
import qualified Numeric.ER.Real.Approx as RA

import qualified Data.IntMap as IMap

import qualified Data.Sequence as Q
import System.Console.CmdArgs
import System.CPUTime
import System.IO

data Problem = Problem
    {box :: [(Int,(Rational,Rational))]
    ,theorem :: Form}
    deriving (Show,Read)

data Paver = Paver 
    {degree :: Int
    ,startDegree :: Int
    ,maxSize :: Int
    ,minDepth :: Int
    ,maxDepth :: Int
    ,effort :: Int
    ,time :: Int
    ,order :: Order
    ,report :: Report
    ,fptype :: FPType
    ,boxSkewing :: Bool
    ,splitGuessing :: Bool
    ,plotWidth :: Int
    ,plotHieght :: Int
    }
    deriving (Show,Data,Typeable)

paver = Paver 
    {degree = 0 &= help "maximum polynomial degree (default = 0)"
    ,startDegree = 0 &= help "first polynomial degree to try on each box (default = 0)"
    ,maxSize = 100 &= name "z" &= help "maximum polynomial term size (default = 100)"
    ,order = B &= help "sub-problem processing order, b for breadth-first (default) or d for depth-first"
    ,minDepth = 0 &= help "minimum bisection depth (default = 0)"
    ,maxDepth = 10 &= name "b" &= help "maximum bisection depth (default = 10)"
    ,effort = 10 &= help "approximation effort parameter (default = 10)" 
    ,time = 3600 &= help "timeout in seconds (default = 3600)"
    ,boxSkewing = False &= name "k" &= help "allow parallelepiped boxes, by default only coaxial rectangles"
    ,splitGuessing = False &= name "g" &= help "try guessing the best direction of splitting, by default tend towards square boxes"
    ,fptype = B32near &= help "type of binary floating point number and rounding mode, b32 or b32near (default) for 32-bit and b64 or b64near for 64-bit"
    ,report = VOL &= help "progress reporting, v for proved volume fraction (default)"
    ,plotWidth = 0 &= name "w" &= help "plot width for 2D problems, 0 mean no plotting (default)"
    ,plotHieght = 0 &= name "h" &= help "plot height for 2D problems, 0 mean no plotting (default)"
    } &=
    help "Decides theorems using polynomial interval arithmetic" &=
    summary "PolyPaver 0.1 (c) 2011 Jan Duracz, Michal Konecny"

defaultMain problem = 
    do
    args <- cmdArgs paver
    initMachineDouble -- round upwards
    hSetBuffering stdout LineBuffering -- print progress in real time, not in batches
    let maxdeg = degree args
        startdeg = startDegree args
        improvementRatioThreshold = 1.2
        maxsize = maxSize args 
        maxtime = toInteger $ time args
        ix = fromInteger $ toInteger $ effort args
        mindepth = minDepth args 
        maxdepth = maxDepth args 
        initbox = readBox $ box problem 
        thm = theorem problem
        ordr = order args 
        repor = report args
        fpt = fptype args
        splitGuessingOpt = splitGuessing args
        boxSkewingOpt = boxSkewing args
        plotSizesOpt = (plotWidth args, plotHieght args)
        plotStepDelayMs = 0
        in do
    loop
        plotSizesOpt
        plotStepDelayMs
        ordr -- sub-problem processing order
        repor -- 
        fpt -- 
        boxSkewingOpt
        splitGuessingOpt
        startdeg
        maxdeg -- maximum bound degree
        improvementRatioThreshold -- when to try raising degree/effort and when to give up and split
        maxsize
        mindepth -- minimum bisection depth
        maxdepth -- maximum bisection depth
        0 -- maxdepth
        ix
        maxtime -- 24 hour timeout
        23 -- mantissa bit size (read precisionS)
        thm -- to be proved, defined in IntegralTest
--        intvarids -- variable IDs of integer variables, defined in IntegralTest
        initbox

readBox  :: [(Int,(Rational,Rational))] -> PPBox BM
readBox intervals = 
    IMap.fromList $ map readInterval $ intervals
    where
    readInterval (i,(l,r)) =
        (i, (const,  IMap.insert i slope zeroCoeffs))
        where
        slope = (rRA - lRA) / 2
        const = (rRA + lRA) / 2 
        lRA = fromRational l
        rRA = fromRational r
    vars = map fst intervals
    zeroCoeffs = IMap.fromList $ zip vars $ repeat 0
    