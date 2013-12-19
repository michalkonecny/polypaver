{-# LANGUAGE FlexibleInstances #-}

{-|
    Module      :  PolyPaver.APBox
    Description :  axis-parallel boxes for paving the domain  
    Copyright   :  (c) Michal Konecny, Jan Duracz  
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Axis-parallel boxes for paving domains.
-}
module PolyPaver.APBox 
(
    APBox(..),
    showBox,
    centerRadiusFromEndpoints,
    boxFromEndpoints,
    boxFromIntervals,
    boxVolume,
    shrinkIntervalToIntegerBounds,
    centerRadiusToInterval,
    boxEqual,
    boxThinThickVars,
    boxCentre,
    boxCorners
)
where

import PolyPaver.Vars

import qualified Numeric.ER.Real.Approx as RA
--import Numeric.ER.Real.Approx.Interval (ERInterval(..))
import qualified Numeric.ER.Real.Base as B
import Numeric.ER.Real.DefaultRepr
--import Numeric.ER.RnToRm.DefaultRepr

--import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import Data.List (intercalate)

import Numeric.ER.Misc
_ = unsafePrint

data APBox b = 
    APBox
    !(IMap.IntMap (IRA b, IRA b)) 
        -- for each variable, affine map from [-1,1] to its domain, ie (center, radius)
    !(IMap.IntMap Bool) -- whether the variable is restricted to integers
    !(IMap.IntMap String) -- human-friendly variable names

showBox ::
    B.ERRealBase b =>
    (APBox b) -> String
showBox (APBox box _ varNames) =
        "APBox{ "
        ++ (intercalate ", " $ map showVarInterval vars)
        ++ " }"
    where
    (vars, _) = unzip $ IMap.toAscList box
    showVarInterval var =
        case IMap.lookup var box of
            Just (center, radius) ->
                showVar varNames var ++ " in " ++ show ((center - radius) RA.\/ (center + radius))
            _ -> error "ppShow: showVarInterval failed"

boxFromEndpoints ::
    (B.ERRealBase b) =>
    IMap.IntMap Bool ->
    IMap.IntMap String ->
    [(Int, (RA b, RA b))] ->
    APBox b
boxFromEndpoints varIsInts varNames intervals = 
    APBox (IMap.fromList $ map readInterval $ intervals) varIsInts varNames 
    where
    readInterval (i,(lRA, rRA)) =
        (i, (constant,  slope))
        where
        (constant, slope) = centerRadiusFromEndpoints (lRA, rRA)

centerRadiusFromEndpoints :: 
    Fractional t => (t, t) -> (t, t)
centerRadiusFromEndpoints (lRA,rRA) =
    (constant, slope)
    where
    slope = (rRA - lRA) / 2
    constant = (rRA + lRA) / 2

boxFromIntervals ::
    (B.ERRealBase b) =>
    IMap.IntMap Bool ->
    IMap.IntMap String ->
    [(Int, (Rational, Rational))] ->
    APBox b
boxFromIntervals varIsInts varNames intervals =
    boxFromEndpoints varIsInts varNames ras
    where
    ras = map getRA intervals
    getRA (i, (l,r)) = (i, (lRA, rRA))
        where
        lRA = fromRational l
        rRA = fromRational r

boxVolume :: (B.ERRealBase b) => APBox b -> IRA b
boxVolume (APBox box varIsInts _) =
    product $ checkLength $ IMap.elems $ IMap.intersectionWith getSize varIsInts box
    where
    getSize isIntVar (center, radius)
        | isIntVar = countIntegers $ centerRadiusToInterval (center, radius) 
        | otherwise = 2 * radius
    checkLength list
        | length list == IMap.size box = list
        | otherwise = error "boxVolume: mismatch in parameters"
    countIntegers interval =
        fromInteger $ 1 + rFloor - lCeil
        where
        (lCeil, rFloor) = shrinkIntervalToIntegerBounds interval 

shrinkIntervalToIntegerBounds :: 
     RA.ERIntApprox t =>
     t -> (Integer, Integer)
shrinkIntervalToIntegerBounds interval =
--    unsafePrint
--    (
--        "shrinkIntervalToIntegerBounds: "
--        ++ "\n interval = " ++ show interval
--        ++ "\n lCeilEI = " ++ show lCeilEI
--        ++ "\n rFloorEI = " ++ show rFloorEI
--    ) $
    (lCeil, rFloor)
    where
    lCeil =  toInteger lCeilEI
    rFloor = toInteger rFloorEI
    (_, lCeilEI) = RA.integerBounds l
    (rFloorEI, _) = RA.integerBounds r
    (l,r) = RA.bounds interval
        
--boxToIntervals :: 
--    B.ERRealBase b =>
--    APBox b -> 
--    [IRA b]
--boxToIntervals (box, _, _) =
--    map centerRadiusToInterval $ IMap.elems box
    
centerRadiusToInterval :: 
    B.ERRealBase b => 
    (IRA b, IRA b) -> IRA b
centerRadiusToInterval (center, slope) =
    (center - slope) RA.\/ (center + slope)
    
boxEqual :: 
    (B.ERRealBase b) => 
    APBox b -> APBox b -> Bool
boxEqual (APBox box1 _ _) (APBox box2 _ _) = 
    sizeCheck && (and $ IMap.elems varComparisons) 
    where
    sizeCheck = 
        comparisonsSize == IMap.size box1 && comparisonsSize == IMap.size box2 
        where
        comparisonsSize = IMap.size varComparisons 
    varComparisons = IMap.intersectionWith centerRadiusEqual box1 box2
    centerRadiusEqual (c1, r1) (c2, r2) =
        (c1 `eq` c2) && (r1 `eq` r2) 
    a `eq` b = 
        (a `RA.equalReals` b) == Just True
            
boxThinThickVars :: 
    B.ERRealBase b => 
    APBox b -> ([Int], [Int])
boxThinThickVars (APBox box _ _) =
    (thinVars, thickVars)
    where
    thickVars = IMap.keys thickbox
    thinVars = IMap.keys thinbox
    thickbox = IMap.filter (not . radiusIsZero) box -- thick projection of box
    thinbox = IMap.filter (radiusIsZero) box -- thick projection of box
    radiusIsZero (_, r) = r `RA.equalReals` 0 == Just True 
        
boxCentre :: 
    APBox b -> 
    [IRA b]
boxCentre (APBox box _ _) =
    fst $ unzip $ snd $ unzip $ IMap.toAscList box

boxCorners ::
    B.ERRealBase b =>
    APBox b -> 
    [[IRA b]]
boxCorners (APBox box _ _)
    =
    map getCorner signCombinations
    where
    (vars, centresRadii) = unzip $ IMap.toAscList box
    (centre, radii) = unzip centresRadii
    signCombinations
        = map (map snd) $ allPairsCombinations $ zip vars $ repeat (-1,1)
    getCorner signs =
        zipWith (*) centre $ zipWith (*) radii signs  
