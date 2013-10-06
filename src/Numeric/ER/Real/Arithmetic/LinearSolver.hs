{-|
    Module      :  Numeric.ER.Real.Arithmetic.LinearSolver
    Description :  arbitrary precision piece-wise something function enclosures
    Copyright   :  (c) 2008 Jan Duracz, Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    A simple validated solver for systems of linear equations with
    interval coefficients.  It uses a naive splitting approach and is
    therefore very slow.
-}
module Numeric.ER.Real.Arithmetic.LinearSolver 
(
    linearSolver
)
where

import qualified Numeric.ER.Real.Approx as RA 
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainBoxMappable, DomainIntBox)
import Numeric.ER.BasicTypes

import Data.List
import Data.Maybe
--import qualified Data.Map as Map

-- the following is code for unit testing 
{-

import Numeric.ER.Real.DefaultRepr 

eq1 :: (Box IRA, IRA)
eq1 = (mkBox [1,2,1], 2)
eq2 = (mkBox [2,4,2], 4)
eq3 = (mkBox [2,4,4], 5)
eqs = [eq1,eq2,eq3]

box = mkBox $ replicate 3 $ (-1)RA.\/1  
x1 = (-13/16)RA.\/(-3/4) :: IRA
x2 = (5/16)RA.\/(3/8) :: IRA
tol = 2^^(-20) :: IRA

mkBox :: [IRA] -> Box IRA
mkBox iras = Map.fromList $ zip [1..] iras
-}

linearSolver ::
    (RA.ERIntApprox ira, 
     DomainIntBox box varid ira,
     DomainBoxMappable box box varid ira ira) =>
    [(box, ira)] 
        {-^ the equations; 
            each equation has coefficients of linear terms 
              + constant term -} ->
    box {-^ the domain of the variables -} ->
    ira {-^ an upper bound on the size of an acceptable solution box -} ->
    Maybe box 
        {-^ 
            A box containing at least one solution within the domain; 
            Nothing if there is no solution. 
        -}
linearSolver eqns domBox tolerance =
    linearSolver' eqns [domBox] tolerance
    
linearSolver' eqns [] tolerance = 
    Nothing
linearSolver' eqns (b:bs) tolerance
    | not $ evalEqns b eqns = -- no solutions in the box
        linearSolver' eqns bs tolerance
    | belowTolerance = 
        Just b
    | otherwise = 
        linearSolver' eqns (splitBox b ++ bs) tolerance
    where
    belowTolerance =
        and $ map (\d -> width d `RA.ltSingletons` tolerance) $ DBox.elems b

evalEqns box eqns =
    and $ map (evalEqn box) eqns
            
{-|
    returns true iff there exists a solution to the equation in the box
-}
evalEqn box (expr,cons) = 
    cons `RA.refines` (evalExpr expr box)
    where
    evalExpr expr box = sum $ DBox.elems $ DBox.intersectionWith (*) expr box

{-|
    returns the list of (two) boxes resulting from splitting the widest edge 
    of the box in half
-}
splitBox box =
    [DBox.insert k (iLg RA.\/ iMg) box, 
     DBox.insert k (iMg RA.\/ iRg) box]
    where
    iMg = (iLg+iRg)/2
    iLg = incrementGranularity iL
    iRg = incrementGranularity iR
    (iL,iR) = RA.bounds i
    i = DBox.lookup "ER: LinearSolver: splitBox: " k box
    k = widestVar box
    incrementGranularity x =
        RA.setMinGranularityOuter (RA.getGranularity x + 1) x

widestVar box =
    fst $ DBox.bestSplit box

width i =
    snd $ RA.bounds (iR-iL)
    where
    (iL,iR) = RA.bounds i

