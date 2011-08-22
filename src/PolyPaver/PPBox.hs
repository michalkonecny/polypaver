{-# LANGUAGE FlexibleInstances #-}

{-|
    Module      :  PolyPaver.Logic
    Description :  parallelepiped boxes for paving the domain  
    Copyright   :  (c) Michal Konecny, Jan Duracz  
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Parallelepiped boxes for paving domains.
-}
module PolyPaver.PPBox where

import Numeric.ER.Misc
import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Real.Approx.Interval (ERInterval(..))
import qualified Numeric.ER.Real.Base as B
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.RnToRm.DefaultRepr

import qualified Data.IntMap as IMap

type PPBox b = IMap.IntMap (Affine b)
type Affine b = (IRA b, Coeffs b)
type Coeffs b = IMap.IntMap (IRA b)

type BoxDirection b = Affine b

ppVolume :: (B.ERRealBase b) => PPBox b -> IRA b
ppVolume box
    = abs $ determinant $ 
        IMap.elems $ IMap.unionsWith (++) $ 
            map (IMap.map (:[]) . snd) $ IMap.elems box
    
determinant :: (B.ERRealBase b) => [[IRA b]] -> IRA b
determinant matrix =
    case matrix of
        [[a]] -> a
        [[a,b],[c,d]] -> a * d - c * b
        firstCol : rest ->
            sum $ zipWith (*) firstCol $
                map (getMinorDet rest) [0..length firstCol - 1]
    where
    getMinorDet cols i 
        | even i = det
        | otherwise = - det
        where
        det = determinant $ map (removeElem i) cols
    removeElem i col =
        (take i col) ++ (drop (i + 1) col)
    

ppEqual :: (B.ERRealBase b) => PPBox b -> PPBox b -> Bool
ppEqual box1 box2 = and $ IMap.elems $ IMap.intersectionWith ppAffineEqual box1 box2
    
ppAffineEqual :: (B.ERRealBase b) => Affine b -> Affine b -> Bool
ppAffineEqual (c1, coeffs1) (c2, coeffs2)
    =
    (c1 `eq` c2) &&
    (and $ map snd $ IMap.toList $
        IMap.intersectionWith eq coeffs1 coeffs2)
    where
    c1 `eq` c2 = 
        case c1 `RA.equalReals` c2 of
            Just res -> res
            _ -> False
            
ppCoeffsZero :: 
    (B.ERRealBase b) => Coeffs b -> Bool
ppCoeffsZero coeffs = 
    and $ map isZero $ IMap.elems coeffs
    where
    isZero coeff = 
        case RA.equalReals coeff 0 of 
            Just True -> True
            _ -> False

            