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

import PolyPaver.Form
import PolyPaver.Solver

import Numeric.ER.BasicTypes.DomainBox.IntMap
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.Real.Base.MachineDouble
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import qualified Numeric.ER.Real.Approx as RA

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
    ,minDepth = 0 &= help "minimum bisection depth (default = 0)"
    ,maxDepth = 10 &= help "maximum bisection depth (default = 10)"
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
        improvementRatioThreshold = 1.5
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
        startdeg -- first degree bound to try
        maxdeg -- maximum bound degree
        improvementRatioThreshold -- when to try raising degree/effort and when to give up and split
        mindepth -- minimum bisection depth
        maxdepth -- maximum bisection depth
        0 -- maxdepth
        ix
        maxtime -- 24 hour timeout
        23 -- mantissa bit size (read precisionS)
        thm -- to be proved, defined in IntegralTest
        intvarids -- variable IDs of integer variables, defined in IntegralTest
        (Q.singleton (0,initbox)) -- enqueue (initial depth, initial box)
        1 -- queue length
        inittime -- inittime
        0 -- prevtime
        1 -- number computed boxes
        (volume initbox) -- initial volume
        0 -- volume of proved boxes

--    solver
--        maxdeg -- maximum bound degree
--        maxtime -- 24 hour timeout
--        bisections -- maximum bisection depth
--        0 -- domain width parameter
--        0 -- midpoint
--        ix
--        23 -- mantissa bit size (read precisionS)
--        thm -- to be proved, defined in IntegralTest
--        intvarids -- variable IDs of integer variables, defined in IntegralTest
--        (Q.singleton (0,initbox)) -- enqueue (initial depth, initial box)
--        qlength -- queue length
--        inittime -- inittime
--        0 -- prevtime
--        maxdepth -- maxdepth
--        1 -- number computed boxes
--        (volume initbox) -- initial volume
--        0 -- volume of proved boxes

readBox  :: [(Int,(Rational,Rational))] -> Box (IRA BM)
readBox = 
    DBox.fromList . 
    map (\(i,(l,r)) -> (i,fromRational l RA.\/ fromRational r))
