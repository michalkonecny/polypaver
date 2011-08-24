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
module PolyPaver.PPBox 
(
    PPBox,
    BoxHyperPlane,
    ppShow,
    ppVolume,
    ppCentre,
    ppCorners,
    ppEqual,
    ppCoeffsZero,
    ppIsectInterior,
    ppPointInInterior,
    ppSkewAlongHyperPlane
)
where

import Numeric.ER.Misc
import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Real.Approx.Interval (ERInterval(..))
import qualified Numeric.ER.Real.Base as B
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.RnToRm.DefaultRepr

import qualified Data.IntMap as IMap
import Data.List (intercalate)

type PPBox b = IMap.IntMap (Affine b)
type Affine b = (IRA b, Coeffs b)
type Coeffs b = IMap.IntMap (IRA b)

type BoxHyperPlane b = Affine b

ppShow box 
    =
    "PP{ corner0=" ++ show corner0 ++ "; "
    ++ (intercalate ", " $ map showVarCorner vars)
    ++ "}"
    where
    (vars, affines) = unzip $ IMap.toAscList box
    (centre, coeffsList) = unzip affines
    corner0 = getCorner centre coeffsList (replicate (length vars) (-1))
    showVarCorner var =
        "corner" ++ show (var + 1) ++ "=" ++ show (getVarCorner var)
    getVarCorner var =
        getCorner centre coeffsList signs
        where
        signs = 
            replicate var (-1)
            ++ 
            [1]
            ++
            (replicate (length vars - var - 1) (-1))

ppCentre box =
    fst $ unzip $ snd $ unzip $ IMap.toAscList box

ppCorners box
    =
    map (getCorner centre coeffsList) signCombinations
    where
    (vars, affines) = unzip $ IMap.toAscList box
    (centre, coeffsList) = unzip affines
    signCombinations
        = map (map snd) $ allPairsCombinations $ zip vars $ repeat (-1,1)

getCorner ::
    (B.ERRealBase b) =>
    [IRA b] -> [IMap.IntMap (IRA b)] -> [IRA b] -> [IRA b]
getCorner centre coeffsList signs =
    zipWith (+) centre $
        map (sumWithSigns signs) coeffsList
    where
    sumWithSigns sings coeffs
        =
        sum $ zipWith (*) signs $ map snd $ IMap.toAscList coeffs
    getCoord pt coeffs =
        case IMap.lookup pt coeffs of Just cf -> cf

ppEvalBox box ptUnitCoords
    =
    getCorner centre coeffsList ptUnitCoords
    where
    (vars, affines) = unzip $ IMap.toAscList box
    (centre, coeffsList) = unzip affines
    

ppInvertBox box
    =
    IMap.fromAscList $ map (\(v,(c,cfList)) -> (v,(c, IMap.fromAscList cfList))) $
    case map (\(v,(c,cfs)) -> (v,(c, IMap.toAscList cfs))) $ IMap.toList box of
        [(0, (o1, [(0, cf11)]))] -> -- one variable
            [(0, (-o1/cf11, [(0, 1/cf11)]))]
        [(0, (o1, [(0, cf11),(1, cf12)])),
         (1, (o2, [(0, cf21),(1, cf22)]))] -> -- two variables
            [(0, (io1, [(0, icf11),(1, icf12)])),
             (1, (io2, [(0, icf21),(1, icf22)]))]
            where
            det = cf11 * cf22 - cf12 * cf21
            io1 = (o2 * cf12 - o1 * cf22) / det
            io2 = (o1 * cf21 - o2 * cf11) / det
            icf11 = cf22 / det
            icf12 = - cf12 / det
            icf21 = - cf21 / det
            icf22 = cf11 / det
        _ ->
            error "box inversion currently supported only for dimensions 1 and 2"

ppVolume :: (B.ERRealBase b) => PPBox b -> IRA b
ppVolume box
    = abs $ determinant $ 
        IMap.elems $ IMap.unionsWith (++) $ 
            map (IMap.map (:[]) . snd) $ IMap.elems box
    
determinant :: (B.ERRealBase b) => [[IRA b]] -> IRA b
determinant matrix 
    =
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
    (B.ERRealBase b) => 
    Coeffs b -> Bool
ppCoeffsZero coeffs 
    = 
    and $ map isZero $ IMap.elems coeffs
    where
    isZero coeff = 
        case RA.equalReals coeff 0 of 
            Just True -> True
            _ -> False

ppIsectInterior ::
    (B.ERRealBase b) => 
    PPBox b -> PPBox b -> Maybe Bool
ppIsectInterior box1 box2
    | dim > 2 = Nothing -- box inversion currently supported only for dim <= 2
    | otherwise 
        =
        do
        trues1 <- sequence $ filter (/= (Just False)) $ map ptIsInside2 corners1
        trues2 <- sequence $ filter (/= (Just False)) $ map ptIsInside1 corners2
        case trues1 ++ trues2 of
            [] -> Just False -- all corners outside the opposite box
            _ -> Just True -- at least one point inside
    where
    dim = IMap.size box1
    corners1 = ppCorners box1
    corners2 = ppCorners box2
    ptIsInside1 pt =
        ppPointInInterior box1 pt
    ptIsInside2 pt =
        ppPointInInterior box2 pt
            
ppPointInInterior box pt
    =
--    unsafePrint
--    (
--        "ppPointInInterior"
--        ++ "\n box = " ++ ppShow box
--        ++ "\n pt = " ++ show pt
--        ++ "\n invBox = " ++ ppShow invBox
--        ++ "\n invPt = " ++ show invPt
--        ++ "\n mapM insideUnitInterval invPt = " ++ show (mapM insideUnitInterval invPt)
--    ) $
    do
    coordsInside <- mapM insideUnitInterval invPt
    Just $ and coordsInside -- all coords inside [-1,1]
    where
    invPt = ppEvalBox invBox pt
    invBox = ppInvertBox box
    insideUnitInterval coord
        = (RA.bounds coord) `strictlyInsideBounds` (-1,1)
            
strictlyInsideBounds (aL,aR) (bL,bR)
    | aR <= bL  = Just False
    | aL >= bR = Just False
    | bL < aL && aR < bR = Just True
    | otherwise = Nothing
            
ppSkewAlongHyperPlane ::
    (B.ERRealBase b) => 
    PPBox b -> BoxHyperPlane b -> (IRA b, Maybe Int, PPBox b)
ppSkewAlongHyperPlane prebox hp@(hp_const, hp_coeffs)
    | 0 `RA.refines` skewVar_stretch = 
--        unsafePrint
--        (
--            "ppSkewAlongHyperPlane: hyperplane parallel to box"
--            ++"\n maybeSkewVar = " ++ show maybeSkewVar 
--        ) $
        (1/0, maybeSkewVar, prebox) -- zero skewing - hyperplane parallel to a side of prebox 
    | otherwise 
        =
--        unsafePrint
--        (
--            "ppSkewAlongHyperPlane: "
--            ++"\n prebox = " ++ ppShow prebox 
--            ++"\n box = " ++ ppShow box
--            ++"\n" 
--        ) $
        (isecPtDistance, maybeSkewVar, box)
    where
    isecPtDistance = abs $ hp_const / largest_hp_coeff
    box = 
        -- apply skewing to each affine transformation:
        IMap.map skewAffine prebox
        where
        skewAffine (af_const, af_coeffs)
            -- skewing doew not change the constant, only the coeffs:
            = (af_const, new_af_coeffs)
            where
            new_af_coeffs 
                =
                IMap.insert skewVar stretched_af_coeff_skewVar $
                    IMap.union new_af_coeffs_noSkewVar af_coeffs_outside_hp
            -- each variable, except the skew variable, feature in the sum as before
            -- plus its contribution to the new interpretation of skewVar:
            new_af_coeffs_noSkewVar
                = IMap.intersectionWith skew af_coeffs hp_coeffs_noSkewVar
                where
                skew af_coeff hp_coeff = 
                    af_coeff - (af_coeff_skewVar * hp_coeff / hp_coeff_skewVar)
            -- the interpretation of skewVar makes the new box stretch to still cover the original box:
            stretched_af_coeff_skewVar
                = af_coeff_skewVar * (1 + skewVar_stretch)
            af_coeff_skewVar 
                = case IMap.lookup skewVar af_coeffs of Just cf -> cf
            af_coeffs_outside_hp
                = IMap.difference af_coeffs hp_coeffs
    hp_coeff_skewVar
        = case IMap.lookup skewVar hp_coeffs of Just cf -> cf 
    hp_coeffs_noSkewVar 
        = IMap.delete skewVar hp_coeffs
    skewVar_stretch
        = sum $ map absScale $ IMap.elems hp_coeffs_noSkewVar
        where
        absScale hp_coeff
            = abs (hp_coeff / hp_coeff_skewVar)
    Just skewVar = maybeSkewVar 
    (largest_hp_coeff, maybeSkewVar) 
        = foldl max (0, Nothing) $ map flipAbs $ IMap.toList hp_coeffs
        where
        flipAbs (var, cf) = (abs cf, Just var)
     
    
    
