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
    ,ivars :: [Int]
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
    ,fptype :: FPType}
    deriving (Show,Data,Typeable)

paver = Paver 
    {degree = 0 &= help "maximum polynomial degree (default = 0)"
    ,startDegree = 0 &= help "first polynomial degree to try on each box (default = 0)"
    ,maxSize = 100 &= help "maximum polynomial term size (default = 100)" &= name "z"
    ,minDepth = 0 &= help "minimum bisection depth (default = 0)"
    ,maxDepth = 10 &= help "maximum bisection depth (default = 10)" &= name "b"
    ,effort = 10 &= help "approximation effort parameter (default = 10)" 
    ,time = 3600 &= help "timeout in seconds (default = 3600)"
    ,order = B &= help "sub-problem processing order, b for breadth-first (default) or d for depth-first"
    ,report = VOL &= help "progress reporting, v for proved volume fraction (default)"
    ,fptype = B32 &= help "type of binary floating point number, b32 for 32-bit (default) and b64 for 64-bit"
    } &=
    help "Decides theorems using polynomial interval arithmetic" &=
    summary "PolyPaver 0.1 (c) 2011 Jan Duracz, Michal Konecny"

defaultMain problem = 
    do
    args <- cmdArgs paver
    inittime <- getCPUTime
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
        intvarids = ivars problem
        thm = theorem problem
        ordr = order args 
        repor = report args
        fpt = fptype args
        in do
    loop
        ordr -- sub-problem processing order
        repor -- 
        fpt -- 
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
        intvarids -- variable IDs of integer variables, defined in IntegralTest
        (Q.singleton (0,startdeg,initbox)) -- enqueue (initial depth, initial startdegree, initial box)
        1 -- queue length
        inittime -- inittime
        0 -- prevtime
        1 -- number computed boxes
        (ppVolume initbox) -- initial volume
        0 -- volume of proved boxes

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
    