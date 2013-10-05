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
    showAffine,
    ppBoxFromIntervals,
    ppBoxFromRAs,
    constSlopeFromRA,
    ppVolume,
    shrinkIntervalToIntegerBounds,
    affineUnivariateToInterval,
    ppCentre,
    ppCorners,
    ppEqual,
    ppCoeffsZero,
    ppIntersect,
    ppPointInside,
    ppSkewAlongHyperPlane
)
where

import PolyPaver.Vars

import Numeric.ER.Misc
import qualified Numeric.ER.Real.Approx as RA
--import Numeric.ER.Real.Approx.Interval (ERInterval(..))
import qualified Numeric.ER.Real.Base as B
import Numeric.ER.Real.DefaultRepr
--import Numeric.ER.RnToRm.DefaultRepr

import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import Data.List (intercalate, sort)

type PPBox b = 
    (Bool, -- True if skewed, False if axis-aligned
     IMap.IntMap (Affine b), 
        -- affine maps from the skewed variables in [-1,1] 
        --   to individual original coordinates 
     IMap.IntMap Bool, -- whether the variable is restricted to integers
     IMap.IntMap String -- human-friendly variable names
    )
type Affine b = (IRA b, Coeffs b)
type Coeffs b = Map.Map Int (IRA b)

type BoxHyperPlane b = Affine b

ppShow ::
    B.ERRealBase b =>
    (PPBox b) -> String
ppShow (skewed, box, _, varNames)
    | skewed =
        "PP{ corner0=" ++ show corner0 ++ "; "
        ++ (intercalate ", " $ map showVarCorner vars)
        ++ "}"
    | otherwise =
        "Box{ "
        ++ (intercalate ", " $ map showVarInterval vars)
        ++ " }"
    where
    (vars, affines) = unzip $ IMap.toAscList box
    (centre, coeffsList) = unzip affines
--    isInterval 
--        = 
--        and $ map isVarProj $ IMap.toAscList box
--        where
--        isVarProj (var, (_, coeffs))
--            =
--            and $ map isZero $ Map.elems $ Map.delete var coeffs
--        isZero cf = cf `RA.equalReals` 0 == Just True
    corner0 = getCorner centre coeffsList (replicate (length vars) (-1))
    showVarInterval var =
        case IMap.lookup var box of
            Just (constant, coeffs) ->
                case Map.lookup var coeffs of
                    Nothing -> showVar varNames var ++ " is thin"
                    Just cf -> showVar varNames var ++ " in " ++ show ((constant - cf) RA.\/ (constant + cf))
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

showAffine ::
    (B.ERRealBase b) =>
    (Affine b) -> [Char]
showAffine (c, coeffs)
    = show c ++ " + " ++ (intercalate " + " $ map showVarCoeff $ Map.toAscList coeffs)
    where
    showVarCoeff (var, cf)
        = "x" ++ show var ++ "*" ++ show cf

ppBoxFromRAs ::
    (B.ERRealBase b) =>
    IMap.IntMap Bool ->
    IMap.IntMap String ->
    [(Int, (RA b, RA b))] ->
    PPBox b
ppBoxFromRAs varIsInts varNames intervals = 
    (False, IMap.fromList $ map readInterval $ intervals, varIsInts, varNames) 
    where
    readInterval (i,(lRA, rRA)) =
        (i, (constant,  Map.insert i slope zeroCoeffs))
        where
        (constant, slope) = constSlopeFromRA (lRA, rRA)
    vars = map fst intervals
    zeroCoeffs = Map.fromList $ zip vars $ repeat 0

constSlopeFromRA :: 
    Fractional t => (t, t) -> (t, t)
constSlopeFromRA (lRA,rRA) =
    (constant, slope)
    where
    slope = (rRA - lRA) / 2
    constant = (rRA + lRA) / 2

ppBoxFromIntervals ::
    (B.ERRealBase b) =>
    IMap.IntMap Bool ->
    IMap.IntMap String ->
    [(Int, (Rational, Rational))] ->
    PPBox b
ppBoxFromIntervals varIsInts varNames intervals =
    ppBoxFromRAs varIsInts varNames ras
    where
    ras = map getRA intervals
    getRA (i, (l,r)) = (i, (lRA, rRA))
        where
        lRA = fromRational l
        rRA = fromRational r

ppCentre :: 
    IMap.IntMap (Affine b) -> 
    [IRA b]
ppCentre box =
    fst $ unzip $ snd $ unzip $ IMap.toAscList box

ppCorners ::
    B.ERRealBase b =>
    IMap.IntMap (Affine b) -> 
    [[IRA b]]
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
    [IRA b] -> [Map.Map Int (IRA b)] -> [IRA b] -> [IRA b]
getCorner centre coeffsList signs =
    zipWith (+) centre $
        map sumWithSigns coeffsList
    where
    sumWithSigns coeffs
        =
        sum $ zipWith (*) signs $ map snd $ Map.toAscList coeffs

ppEvalBox ::
    B.ERRealBase b =>
    IMap.IntMap (Affine b) -> [IRA b] -> [IRA b]
ppEvalBox box ptUnitCoords
    =
    getCorner centre coeffsList ptUnitCoords
    where
    (_vars, affines) = unzip $ IMap.toAscList box
    (centre, coeffsList) = unzip affines
    

ppInvertBox :: 
    (B.ERRealBase b) =>
    IMap.IntMap (Affine b) -> IMap.IntMap (Affine b)
ppInvertBox box
    =
    IMap.fromAscList $ map (\(v,(c,cfList)) -> (v,(c, Map.fromAscList cfList))) $
    case map (\(v,(c,cfs)) -> (v,(c, Map.toAscList cfs))) $ IMap.toList box of
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
ppVolume (skewed, box, varIsInts, _)
    | skewed = volume box
    | otherwise =
--        unsafePrint
--        (
--            "ppVolume: axis-aligned:"
--            ++ "\n boxContOnly = " ++ show (boxContOnly) 
--            ++ "\n volume boxContOnly = " ++ show (volume boxContOnly) 
--            ++ "\n boxIntIntervals = " ++ show boxIntIntervals
--            ++ "\n map countIntegers boxIntIntervals = " ++ show (map countIntegers boxIntIntervals) 
--        ) 
        intCombinations * (volume boxContOnly)
    where
    volume b =
        abs $ determinant $ 
            Map.elems $ Map.unionsWith (++) $ 
                map (Map.map ((:[]) . (2 *)) . snd) $ 
                    IMap.elems b
    boxContOnly = 
        IMap.map (\(c,cf) -> (c, Map.filterWithKey isNotInt cf)) boxContOnlyPre
        where
        isNotInt var _ =
            IMap.lookup var varIsInts /= Just True
    (boxIntOnly, boxContOnlyPre) =
        IMap.partitionWithKey isInt box
        where
        isInt var _ = 
            IMap.lookup var varIsInts == Just True 
    intCombinations =
        product $ map countIntegers boxIntIntervals
    countIntegers interval =
        fromInteger $ 1 + rFloor - lCeil
        where
        (lCeil, rFloor) = shrinkIntervalToIntegerBounds interval 
    boxIntIntervals =
        ppNonSkewedToIntervals boxIntOnly

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
        
ppNonSkewedToIntervals :: 
    B.ERRealBase b =>
    IMap.IntMap (Affine b) -> 
    [IRA b]
ppNonSkewedToIntervals box =
    map affineUnivariateToInterval $ IMap.toList box 
    
affineUnivariateToInterval :: 
    (B.ERRealBase b) =>
    (Int, Affine b) -> IRA b
affineUnivariateToInterval (var, (constant, slopesMap)) =
    constant + slope * ((-1) RA.\/ 1)
    where
    Just slope = Map.lookup var slopesMap
    
determinant :: (B.ERRealBase b) => [[IRA b]] -> IRA b
determinant matrix 
    =
    case matrix of
        [] -> 1
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
ppEqual (_, box1, _, _) (_, box2, _, _) = 
    and $ IMap.elems $ IMap.intersectionWith ppAffineEqual box1 box2
    
ppAffineEqual :: (B.ERRealBase b) => Affine b -> Affine b -> Bool
ppAffineEqual (c1, coeffs1) (c2, coeffs2)
    =
    (c1 `eq` c2) &&
    (and $ map snd $ Map.toList $
        Map.intersectionWith eq coeffs1 coeffs2)
    where
    a `eq` b = 
        (a `RA.equalReals` b) == Just True
            
ppCoeffsZero :: 
    (B.ERRealBase b) => 
    Maybe [Int] ->
    Coeffs b ->
    Bool
ppCoeffsZero maybeRelevantVars coeffs 
    = 
    and $ map isZeroOrIrrelevant $ Map.toList coeffs
    where
    isZeroOrIrrelevant (v, coeff) =
        irrelevant v 
        ||
        (RA.equalReals coeff 0 == Just True)
    irrelevant v =
        case maybeRelevantVars of
            Nothing -> False
            Just relevantVars -> not $ v `elem` relevantVars 

ppIntersect ::
    (B.ERRealBase b) => 
    PPBox b -> PPBox b -> Maybe Bool
ppIntersect (_, box1, _, _) (_, box2, _, _)
    | dim > 2 = Nothing -- box inversion currently supported only for dim <= 2
    | otherwise 
        =
        case reverse $ sort $ trues1 ++ trues2 of
            [] -> Just False -- no corner is inside the opposite box
            (Nothing : _) -> Nothing -- no corner definitely inside but some undecided
            _ -> Just True -- at least one corner inside the other box
    where
    trues1 = filter (/= (Just False)) $ map ptIsInside2 corners1
    trues2 = filter (/= (Just False)) $ map ptIsInside1 corners2
    dim = IMap.size box1
    corners1 = ppCorners box1
    corners2 = ppCorners box2
    ptIsInside1 pt =
        ppPointInside box1 pt
    ptIsInside2 pt =
        ppPointInside box2 pt
            
ppPointInside ::
    (B.ERRealBase b) =>
    IMap.IntMap (Affine b) -> 
    [IRA b] -> 
    Maybe Bool
ppPointInside box pt
    =
--    unsafePrint
--    (
--        "ppPointInside"
--        ++ "\n box = " ++ ppShow box
--        ++ "\n pt = " ++ show pt
--        ++ "\n invBox = " ++ ppShow invBox
--        ++ "\n invPt = " ++ show invPt
--        ++ "\n mapM insideUnitInterval invPt = " ++ show (mapM insideUnitInterval invPt)
--    ) $
    
    case reverse $ sort falses of
        [] -> Just True -- all true
        (Nothing : _) -> Nothing -- all either true or undecided
        _ -> Just False -- some false is there
    where
    falses = filter (/= (Just True)) $ map insideUnitInterval invPt
    invPt = ppEvalBox invBox pt
    invBox = ppInvertBox box
    insideUnitInterval coord
        = (RA.bounds coord) `insideBounds` (-1,1)
            
insideBounds :: Ord a => (a, a) -> (a, a) -> Maybe Bool
insideBounds (aL,aR) (bL,bR)
    | aR < bL  = Just False
    | aL > bR = Just False
    | bL <= aL && aR <= bR = Just True
    | otherwise = Nothing
            
ppSkewAlongHyperPlane ::
    (B.ERRealBase b) => 
    PPBox b -> 
    BoxHyperPlane b -> 
    (IRA b, Maybe Int, PPBox b)
ppSkewAlongHyperPlane prebox@(_skewed, preAffine, varIsInts, varNames) _hp@(hp_const, hp_coeffs)
    | 0 `RA.refines` skewVar_stretch = 
--        unsafePrint
--        (
--            "ppSkewAlongHyperPlane: hyperplane parallel to box"
--            ++"\n hp = " ++ show hp 
--            ++"\n maybeSkewVar = " ++ show maybeSkewVar 
--        ) $
        (1/0, maybeSkewVar, prebox) -- zero skewing - hyperplane parallel to a side of prebox 
    | otherwise 
        =
--        unsafePrint
--        (
--            "ppSkewAlongHyperPlane: "
--            ++"\n prebox = " ++ ppShow prebox 
--            ++"\n hp = " ++ show hp 
--            ++"\n resbox = " ++ ppShow resbox
--            ++"\n" 
--        ) $
        (isecPtDistance, maybeSkewVar, resbox)
    where
    isecPtDistance = abs $ hp_const / largest_hp_coeff
    resbox = (True, affine, varIsInts, varNames)
    affine = 
        -- apply skewing to each affine transformation:
        IMap.map skewAffine preAffine
        where
        skewAffine (af_const, af_coeffs)
            -- skewing doew not change the constant, only the coeffs:
            = (af_const, new_af_coeffs)
            where
            new_af_coeffs 
                =
                Map.insert skewVar stretched_af_coeff_skewVar $
                    Map.union new_af_coeffs_noSkewVar af_coeffs_outside_hp
            -- each variable, except the skew variable, feature in the sum as before
            -- plus its contribution to the new interpretation of skewVar:
            new_af_coeffs_noSkewVar
                = Map.intersectionWith skew af_coeffs hp_coeffs_noSkewVar
                where
                skew af_coeff hp_coeff = 
                    af_coeff - (af_coeff_skewVar * hp_coeff / hp_coeff_skewVar)
            -- the interpretation of skewVar makes the new box stretch to still cover the original box:
            stretched_af_coeff_skewVar
                = af_coeff_skewVar * (1 + skewVar_stretch)
            af_coeff_skewVar 
                = case Map.lookup skewVar af_coeffs of Just cf -> cf
            af_coeffs_outside_hp
                = Map.difference af_coeffs hp_coeffs
    hp_coeff_skewVar
        = case Map.lookup skewVar hp_coeffs of Just cf -> cf 
    hp_coeffs_noSkewVar 
        = Map.delete skewVar hp_coeffs
    skewVar_stretch = 
--        unsafePrint
--        (
--            "ppSkewAlongHyperPlane: skewVar_stretch:"
--            ++"\n hp = " ++ show hp 
--        ) $
        sum $ map absScale $ Map.elems hp_coeffs_noSkewVar
        where
        absScale hp_coeff
            = abs (hp_coeff / hp_coeff_skewVar)
    Just skewVar = maybeSkewVar 
    (largest_hp_coeff, maybeSkewVar) 
        = foldl max (0, Nothing) $ map flipAbs $ Map.toList hp_coeffs
        where
        flipAbs (var, cf) = (abs cf, Just var)
     
    
    
