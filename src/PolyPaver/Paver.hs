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

data Problem = Problem
    {box :: [(Int,(Rational,Rational))]
    ,ivars :: [Int]
    ,theorem :: Form}
    deriving (Show,Read)

data Paver = Paver 
    {degree :: Int
    ,bisect :: Int
    ,effort :: Int
    ,time :: Int
    ,order :: Order}
    deriving (Show,Data,Typeable)

paver = Paver 
    {degree = 0 &= help "maximum polynomial degree (default = 0)"
    ,bisect = 10 &= help "maximum bisection depth (default = 10)"
    ,effort = 10 &= help "approximation effort parameter (default = 10)" 
    ,time = 3600 &= help "timeout in seconds (default = 3600)"
    ,order = B &= help "sub-problem processing order, b for breadth-first (default) or d for depth-first"} &=
    help "Decides theorems using polynomial interval arithmetic" &=
    summary "PolyPaver 0.1 (c) 2011 Jan Duracz, Michal Konecny"

defaultMain problem = 
    do
    args <- cmdArgs paver
    inittime <- getCPUTime
    initMachineDouble -- round upwards
    let maxdeg = degree args
        maxtime = toInteger $ time args
        ix = fromInteger $ toInteger $ effort args
        bisections = bisect args 
        initbox = readBox $ box problem 
        intvarids = ivars problem
        thm = theorem problem
        ordr = order args 
        in do
    loop
        ordr -- sub-problem processing order
        maxdeg -- maximum bound degree
        bisections -- maximum bisection depth
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
