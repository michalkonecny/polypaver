{-|
    Module      :  Numeric.ER.Real.Arithmetic.Elementary
    Description :  some elementary functions
    Copyright   :  (c) Michal Konecny, Amin Farjudian, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Some important elementary functions for real approximations
    and their maximal extensions for interval approximations.
-}
module Numeric.ER.Real.Arithmetic.Elementary
(   
    -- * specialised exponentiation
    erSqr_R,
    erSqr_IR,
    erPow_R,
    erPow_IR,
    erSqrt_R,
    erSqrt_IR,
    erSqrt_IR_Inner,
    erRoot_R,
    erRoot_IR,
    erRoot_IR_Inner,
    -- * exponentiation and logarithm 
    erExp_R,
    erExp_IR,
    erExp_IR_Inner,
    erLog_R,
    erLog_IR,
    erLog_IR_Inner,
    -- * trigonometrics
    erSine_R,
    erSine_IR,
    erSine_IR_Inner,
    erCosine_R,
    erCosine_IR,
    erCosine_IR_Inner,
    erATan_R,
    erATan_IR,
    erATan_IR_Inner,
    erPi_R
)
where

import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.BasicTypes
import qualified Numeric.ER.BasicTypes.ExtendedInteger as EI

import Numeric.ER.Real.Arithmetic.Taylor
-- import Numeric.ER.Real.Arithmetic.Newton

import Numeric.ER.Misc

{-
    sqr
-}

erSqr_IR ::
    (RA.ERIntApprox ira, Ord ira) =>
    EffortIndex -> 
    ira -> ira
erSqr_IR =
    RA.maxExtensionR2R
        sqrExtrema
        erSqr_R
    where
    sqrExtrema ix x 
        | 0 `RA.refines` x = [0]
        | otherwise = [] 

erSqr_R ::
    (RA.ERIntApprox ira, Ord ira) =>
    EffortIndex -> 
    ira -> ira
erSqr_R ix a =
    max 0 $ a' * a'
    where
    a' = RA.setMinGranularityOuter gran a
    gran = effIx2gran ix
    
{-
    integer exponentiation x ^ p
-}

erPow_IR ::
    (RA.ERIntApprox ira, Ord ira) =>
    EffortIndex -> 
    Integer ->
    ira -> ira
erPow_IR ix n x = 
    RA.maxExtensionR2R
        powExtrema
        (\ ix x -> erPow_R ix n x)
        ix x
    where
    powExtrema ix x 
        | even n && 0 `RA.refines` x = [0]
        | otherwise = [] 


erPow_R ::
    (RA.ERIntApprox ira, Ord ira) =>
    EffortIndex ->
    Integer ->
    ira -> ira
erPow_R ix p a
    | p < 0 =
        1 / erPow_R ix (-p) a
    | p == 0 = 
        1
    | even p =
        erPow_R ix (div p 2) (erSqr_R ix a')
    | otherwise =
        a' * (erPow_R ix (div (p - 1) 2) (erSqr_R ix a'))
    where
    a' = RA.setMinGranularityOuter gran a
    gran = effIx2gran ix

{-
    sqrt
-}

erSqrt_R ::
    (RA.ERIntApprox ira) => 
    EffortIndex -> ira -> ira
erSqrt_R = erSqrtNewton_R  
    
erSqrt_IR ::
    (RA.ERIntApprox ira) => 
    EffortIndex -> ira -> ira
erSqrt_IR =
    RA.maxExtensionR2R 
        sqrtExtrema
        (\ ix x -> erSqrt_R ix x)

erSqrt_IR_Inner ::
    (RA.ERIntApprox ira) => 
    EffortIndex -> ira -> ira
erSqrt_IR_Inner =
    RA.maxExtensionInnerR2R 
        sqrtExtrema
        (\ ix x -> erSqrt_R ix x)

sqrtExtrema ix x = fst $ sqrtExtremaAndDirections ix x
        
sqrtExtremaAndDirections ix x =
    case RA.compareReals 0 x of
        Just LT -> ([], (Just True, Just True))
        Just GT -> ([], (Nothing, Nothing))
        _ -> ([0], (Nothing, Just True))
        


erSqrtContFr_R ::
    (RA.ERIntApprox ira) => 
    EffortIndex -> ira -> ira
erSqrtContFr_R ix a
    | aR == 0 = 0
    | aL == RA.plusInfinity = RA.plusInfinity
    | aR `RA.ltSingletons` 0 = RA.topApprox
    | otherwise =
        contFrIter (ix + 3) $
            RA.setMinGranularityOuter gran $ 0 RA.\/ aR -- assuming aR >= 0 
    where
    gran = effIx2gran ix
    (aL, aR) = RA.bounds a
    aM1 = a - 1
    
    contFrIter i x_i
        | i == 0 =
            x_i
        | otherwise =
            1 + (aM1 / (x_iPlus1 + 1))
        where
        x_iPlus1 = contFrIter (i - 1) x_i
            
erSqrtNewton_R ::
    (RA.ERIntApprox ira) => 
    EffortIndex -> ira -> ira
erSqrtNewton_R ix a
    | aR == 0 = 0
    | aL == RA.plusInfinity = RA.plusInfinity
    | aR `RA.ltSingletons` 0 = RA.topApprox
    | otherwise =
        x_i RA.\/ (a/x_i)
    where
    gran = effIx2gran ix
    (aL, aR) = RA.bounds a
    aM1 = a - 1
    
    x_i = 
        newtonIter ((ix `div` 10) + 5) $
                RA.setMinGranularityOuter gran aR -- assuming aR >= 0 
    newtonIter i x_i
        | i == 0 = x_i
        | otherwise =
                snd $ RA.bounds $
                    (x_iMinus1 + a / (x_iMinus1)) / 2
        where
        x_iMinus1 = newtonIter (i - 1) x_i

{-
    pth root x ^ (1/p)
-}

erRoot_R ::
    (RA.ERIntApprox ira, Ord ira) => 
    EffortIndex -> Integer -> ira -> ira
erRoot_R = erRootNewton_R    
    
erRoot_IR ::
    (RA.ERIntApprox ira, Ord ira) => 
    EffortIndex -> Integer -> ira -> ira
erRoot_IR ix p =
    RA.maxExtensionR2R 
        (rootExtrema p)
        (\ ix x -> erRoot_R ix p x) $
            ix

erRoot_IR_Inner ::
    (RA.ERIntApprox ira, Ord ira) => 
    EffortIndex -> Integer -> ira -> ira
erRoot_IR_Inner ix p =
    RA.maxExtensionInnerR2R 
        (rootExtrema p)
        (\ ix x -> erRoot_R ix p x) $
            ix
rootExtrema p ix x = fst $ rootExtremaAndDirections p ix x

rootExtremaAndDirections p ix x
    | odd p = ([], (Just True, Just True))
    | otherwise =
        case RA.compareReals 0 x of
            Just LT -> ([], (Just True, Just True))
            Just GT -> ([], (Nothing, Nothing))
            _ -> ([0], (Nothing, Just True))

erRootNewton_R ::
    (RA.ERIntApprox ira, Ord ira) => 
    EffortIndex -> Integer -> ira -> ira
erRootNewton_R ix p a
    | aR == 0 = 0
    | aL == RA.plusInfinity = RA.plusInfinity
    | aR < 0 && even p = RA.topApprox
    | aR < 0 = - erRootNewton_R ix p (-a)
    | p > 0 =
        x_i RA.\/ (a/x_i_pow_p_minus_1)
    | otherwise =   
        1 / (erRootNewton_R ix (-p) a) -- TODO: check extremes
    where
    gran = effIx2gran ix
    (aL, aR) = RA.bounds a
    aM1 = a - 1
    pIRA = fromInteger p
    pIRA_minus_1 = pIRA - 1
    
    (x_i, x_i_pow_p_minus_1) = 
        newtonIter (ix + 5) $
                RA.setMinGranularityOuter gran $ max 0 aR
 
    newtonIter i x_0
        | i == 0 = 
            (x_0, x_0_pow_p_minus_1)
        | otherwise =
            (x_i, x_i_pow_p_minus_1)
            
        where
        (x_iMinus1, x_iMinus1_pow_p_minus_1) = 
            newtonIter (i - 1) x_0
        x_i =
                snd $ RA.bounds $
                    (pIRA_minus_1 * x_iMinus1 + a / x_iMinus1_pow_p_minus_1) / pIRA
        x_i_pow_p_minus_1 =
                erPow_R ix (p - 1) x_i
        x_0_pow_p_minus_1 =
                erPow_R ix (p - 1) x_0

{-
    e^x and log
-}

erExp_R :: 
    (RA.ERIntApprox ira, Ord ira) => 
    EffortIndex -> ira -> ira
    
erExp_R ix x 
    | RA.isBounded x =
--        unsafePrintReturn
--        (
--            "erExp_R: "
--            ++ "\n x = " ++ show x
--            ++ "\n xNear0 = " ++ show xNear0
--            ++ "\n n = " ++ show n
--            ++ "\n erExp_Tay_Opt_R ix xNear0 = " ++ (show $ erExp_Tay_Opt_R ix xNear0)
--            ++ "\n result = "
--        ) $
        erPow_IR ix n $ 
        erExp_Tay_Opt_R ix xNear0
    | x `RA.refines` (-RA.plusInfinity) = 0
    | (-RA.plusInfinity) `RA.refines` x =
        0 RA.\/ (erExp_R ix (snd $ RA.bounds x))
    | otherwise = RA.bottomApprox
    where
    (xNear0, n) = scaleNear0 (x,1)
    scaleNear0 (xPrev, nPrev) =
        case xPrev `RA.refines` ((-1) RA.\/ 1) of
            True -> (xPrev, nPrev)
            False -> scaleNear0 (xNext, nNext)
        where
        xNext = xPrev / 2
        nNext = 2 * nPrev

erExp_IR :: 
    (RA.ERIntApprox ira, Ord ira) => 
    EffortIndex -> ira -> ira
    
erExp_IR =
    RA.maxExtensionR2R
        noExtrema
        erExp_R

erExp_IR_Inner :: 
    (RA.ERIntApprox ira, Ord ira) => 
    EffortIndex -> ira -> ira
erExp_IR_Inner =
    RA.maxExtensionInnerR2R
        noExtrema
        erExp_R

noExtrema ix x = []

{- Log using Newton -}

erLog_R :: 
    (RA.ERIntApprox ira, Ord ira) => 
    EffortIndex -> ira -> ira
    
erLog_R =
    logDivSeries_R 
--    erLog_IR -- intervals are more efficient for log than singletons 

erLog_IR ::
    (RA.ERIntApprox ira, Ord ira) => 
    EffortIndex -> ira -> ira
    
erLog_IR =
    RA.maxExtensionR2R
        logExtrema
        (\ ix x -> logDivSeries_R ix x)
        
erLog_IR_Inner ::
    (RA.ERIntApprox ira, Ord ira) => 
    EffortIndex -> ira -> ira
    
erLog_IR_Inner =
    RA.maxExtensionInnerR2R
        logExtrema
        (\ ix x -> logDivSeries_R ix x)
        
logExtrema ix x = fst $ logExtremaAndDirections ix x
        
logExtremaAndDirections ix x =
    case RA.compareReals 0 x of
        Just LT -> ([], (Just True, Just True))
        Just GT -> ([], (Nothing, Nothing))
        _ -> ([-RA.plusInfinity], (Nothing, Just True))
        
{-| log using a fast converging series, designed to be used with singletons -}
logDivSeries_R ::
    (RA.ERIntApprox ira, Ord ira) => EffortIndex -> ira -> ira 
logDivSeries_R ix x 
    | posx `RA.refines` 0 = -RA.plusInfinity
    | 0 `RA.refines` posx = RA.bottomApprox
    | posx `RA.refines` (RA.plusInfinity) = RA.plusInfinity
    | otherwise =
        case RA.compareReals posx 1 of
            Just LT ->
--                unsafePrint 
--                (
--                    "logDivSeries_R: recursion via recip" 
--                ) $
                negate $
                    (logDivSeries_R ix posxRecipL) 
                    RA.\/ 
                    (logDivSeries_R ix posxRecipR)
            _ ->
--                unsafePrint 
--                (
--                    "logDivSeries_R: using series"
--                    ++ "\n posx = " ++ show posx 
--                    ++ "\n xCeiling = " ++ show xCeiling 
--                    ++ "\n intLogUp 2 $ xCeiling = " ++ show (intLogUp 2 $ xCeiling)
--                    ++ "\n nearLogx = " ++ show nearLogx 
--                    ++ "\n remNearLogx = " ++ show remNearLogx 
--                    ++ "\n t = " ++ show t 
--                ) $
                nearLogx + 2 * t * (series ix (RA.setMinGranularityOuter gran 1))
    where
    gran = effIx2gran ix
    posx = (RA.setMinGranularityOuter gran x) RA./\ (0 RA.\/ (RA.plusInfinity))
    (posxRecipL, posxRecipR) = RA.bounds $ recip posx
    nearLogx =
        0.69314718055994530941 * (fromInteger $ intLogUp 2 $ xCeiling)
    remNearLogx =
        posx / (erExp_R ix nearLogx) -- should be very close to 1
    xCeiling = 
        snd $ RA.integerBounds posx
    t = 
        ((remNearLogx - 1) / (remNearLogx + 1)) -- the range of this expression is [-1,1] 
            RA./\ ((-1) RA.\/ 1) -- correction of wrapping 
    tsquare = abs $ t * t -- the range is [0,1]
    series termsCount currentDenominator 
        | termsCount > 0 =
            (recip currentDenominator) + tsquare * (series (termsCount - 1) (currentDenominator + 2))
        | otherwise =
            (recip currentDenominator)
            * (1 RA.\/ (recip $ 1 - tsquare)) -- [1,1/(1-t^2)] is a valid error bound
        
--{- log using Newton -}
--    
--logNewton_RA
--    :: (RA.ERIntApprox ira)
--    => EffortIndex
--    -> ra -- must not be below 1
--    -> ra
--    
--logNewton_RA i x = 
--    case compareReals posx 1 of
--        Just LT ->
--            - (logNewton_RA i (recip posx))
--        _ ->    
--            erNewton_FullArgs 
--                ( \ i y -> (erExp_RA i y) - posx, erExp_RA) 
--                (RA.setMinGranularityOuter gran nearLogx) 
--                (RA.setMinGranularityOuter gran 1) 
--                (fromInteger $ toInteger i)
--                i
--    where
--    gran = effIx2gran i
--    posx = 
--        RA.setMinGranularityOuter gran x /\ (ira2ra $ 0 RA.\/ (RA.plusInfinity))
--    nearLogx =                    
--        0.69314718055994530941 * (fromInteger $ intLog 2 $ xCeiling)
--    xCeiling 
--        | RA.isEmpty posx = 1 -- choice of constant irrelevant
--        | otherwise =
--            snd $ RA.iraIntegerBounds $ ra2ira posx


{-
    sin(x) and cos(x)
-}

erSine_R ::
    (RA.ERIntApprox ira) => 
    EffortIndex -> ira -> ira

erSine_R ix x =
    case (RA.isBounded x) of
        True | xNear0 `RA.refines` plusMinusPiHalf ->
            erSine_Tay_Opt_R ix xNear0
        True | (xNear0 - piHalf) `RA.refines` plusMinusPiHalf ->
            erCosine_Tay_Opt_R ix (xNear0 - piHalf)
        True | (xNear0 + piHalf) `RA.refines` plusMinusPiHalf ->
            negate $ erCosine_Tay_Opt_R ix (xNear0 + piHalf)
        True | (xNear0 - pi) `RA.refines` plusMinusPiHalf ->
            negate $ erSine_Tay_Opt_R ix (xNear0 - pi)
        _ ->
            (-1) RA.\/ 1
    where
    xNear0 = x - k * 2 * pi -- should be in [-pi,3*pi/2]
    k = fromInteger $ toInteger kEI
    (kEI,_) = RA.integerBounds $ 0.5 + (x / (2*pi))

    plusMinusPiHalf = (- piHalf) RA.\/ piHalf
    piHalf = pi / 2
    pi = erPi_R ix
    

erCosine_R :: 
    (RA.ERIntApprox ira) => 
    EffortIndex -> ira -> ira
     
erCosine_R ix x =
    case (RA.isBounded x) of
        True | xNear0 `RA.refines` plusMinusPiHalf ->
            erCosine_Tay_Opt_R ix xNear0
        True | (xNear0 - piHalf) `RA.refines` plusMinusPiHalf ->
            negate $ erSine_Tay_Opt_R ix (xNear0 - piHalf)
        True | (xNear0 + piHalf) `RA.refines` plusMinusPiHalf ->
            erSine_Tay_Opt_R ix (xNear0 + piHalf)
        True | (xNear0 - pi) `RA.refines` plusMinusPiHalf ->
            negate $ erCosine_Tay_Opt_R ix (xNear0 - pi)
        _ ->
            (-1) RA.\/ 1
    where
    xNear0 = x - k * 2 * pi -- should be in [-pi,3*pi/2]
    k = fromInteger $ toInteger kEI
    (kEI,_) = RA.integerBounds $ 0.5 + (x / (2*pi))

    plusMinusPiHalf = (- piHalf) RA.\/ piHalf
    piHalf = pi / 2
    pi = erPi_R ix


{- Sine using generic Taylor (see Taylor for an optimised version) -}

erSine_Tay_R :: 
    (RA.ERIntApprox ira) =>
    EffortIndex -> ira -> ira

erSine_Tay_R ix x
    | (RA.plusInfinity) `RA.refines` x || (-RA.plusInfinity) `RA.refines` x = 
        (-1) RA.\/ 1  
    | otherwise =
        erTaylor_R ix sine_coefSeq sine_error 0 x

sine_coefSeq :: 
    (RA.ERIntApprox ira) => 
    Int -> ira

sine_coefSeq n
  | n `mod` 4 == 0 = 0
  | n `mod` 4 == 1 = 1
  | n `mod` 4 == 2 = 0
  | n `mod` 4 == 3 = -1
  
sine_error n = (-1) RA.\/ 1  

{- maximal extensions -}

erSine_IR ::
    (RA.ERIntApprox ira) =>
    EffortIndex -> ira -> ira 
    
erSine_IR = 
    RA.maxExtensionR2R sineExtremes erSine_R
    
erCosine_IR ::
    (RA.ERIntApprox ira) =>
    EffortIndex -> ira -> ira 
    
erCosine_IR = 
    RA.maxExtensionR2R cosineExtremes erCosine_R
        
erSine_IR_Inner ::
    (RA.ERIntApprox ira) =>
    EffortIndex -> ira -> ira 
    
erSine_IR_Inner = 
    RA.maxExtensionInnerR2R sineExtremes erSine_R
    
erCosine_IR_Inner ::
    (RA.ERIntApprox ira) =>
    EffortIndex -> ira -> ira 
    
erCosine_IR_Inner = 
    RA.maxExtensionInnerR2R cosineExtremes erCosine_R
        
sineExtremes ix x = fst $ sineExtremesAndDirections ix x
cosineExtremes ix x = fst $ cosineExtremesAndDirections ix x
        
sineExtremesAndDirections ix x 
    | RA.isBounded x =
        alternatingExtremes 1 (-1) ix scaledX
    | otherwise = ([-1,1], (Nothing, Nothing))
    where
    scaledX = (x / (erPi_R ix)) - 0.5
    
cosineExtremesAndDirections ix x
    | RA.isBounded x =
        alternatingExtremes 1 (-1) ix scaledX
    | otherwise = ([-1,1], (Nothing, Nothing))
    where
    scaledX = (x / (erPi_R ix))
    
alternatingExtremes extrHigh extrLow ix scaledX
    | extremesCount == 1 && even minExtremeN = 
        ([extrHigh], (Just True, Just False)) -- increasing, decreasing
    | extremesCount == 1 =
        ([extrLow], (Just False, Just True)) -- decreasing, increasing
    | extremesCount >= 2 = 
        ([extrHigh,extrLow], (Just $ even minExtremeN, Just $ odd maxExtremeN))  
    | otherwise = 
        ([], (Just isIncreasing, Just isIncreasing))
    where
    extremesCount = 1 + maxExtremeN - minExtremeN
    isIncreasing = even maxExtremeN
    (xFloor, xCeiling) = RA.integerBounds scaledX
    minExtremeN = 
        case RA.compareReals (fromInteger $ toInteger xFloor) scaledX of
            Just LT -> (xFloor + 1)
            _ -> xFloor
    maxExtremeN =
        case RA.compareReals scaledX (fromInteger $ toInteger xCeiling) of
            Just LT -> xCeiling - 1
            _ -> xCeiling
        

{-
    tan(x), atan(x) and pi
-}

erATan_R :: 
    (RA.ERIntApprox ira) => 
    EffortIndex -> ira -> ira
    
erATan_R = atanEuler_R

erATan_IR ::
    (RA.ERIntApprox ira) =>
    EffortIndex -> ira -> ira 
    
erATan_IR =
    RA.maxExtensionR2R noExtrema erATan_R

erATan_IR_Inner ::
    (RA.ERIntApprox ira) =>
    EffortIndex -> ira -> ira 

erATan_IR_Inner =
    RA.maxExtensionInnerR2R noExtrema erATan_R

{- atan using Euler's series: 
    (x / (1 + x^2)) * (1 + t*2*1/(2*1 + 1)*(1 + t*2*2/(2*2 + 1)*(1 + ... (1 + t*2*n/(2*n+1)*(1 + ...)))))
    where
    t = x^2/(1 + x^2)
    
    where the tail  (1 + t*2*n/(2*n+1)*(1 + ...)) is inside the interval:
    [1, 1 + x^2]
-}

atanEuler_R ::
    (RA.ERIntApprox ira) => 
    EffortIndex -> ira -> ira

atanEuler_R ix x 
    | x `RA.refines` RA.plusInfinity = RA.plusInfinity  
    | x `RA.refines` (- RA.plusInfinity) = - RA.plusInfinity  
    | not $ RA.isBounded x = RA.bottomApprox 
    | x `RA.refines` ((-1.5) RA.\/ 1.5) =
        (x / xSquarePlus1) * (series ix (RA.setMinGranularityOuter gran 2))
    | otherwise = -- too far from 0, needs atan(x) = 2*atan(x/(1+sqrt(1+x^2)))
        2 * (atanEuler_R ix $ x / (1 + sqrtXQuarePlus1))
    where
    gran = effIx2gran ix
    series termsCount coeffBase 
        | termsCount > 0 =
            1 + xSquareOverXSquarePlus1 * coeff * (series (termsCount - 1) (coeffBase + 2))
        | otherwise =
            1 + xSquare * (0 RA.\/ 1)
        where
        coeff = coeffBase / (coeffBase + 1)
    xSquare = abs $ x * x
    xSquarePlus1 = xSquare + 1
    xSquareOverXSquarePlus1 = xSquare / xSquarePlus1
    sqrtXQuarePlus1 =
        iterateIx 10 EI.MinusInfinity
        where
        iterateIx ix prevPrec 
            | prevPrec == currentPrec = result
            | otherwise =
                iterateIx (ix * 2) currentPrec
            where 
            result = erSqrt_R ix xSquarePlus1
            currentPrec = RA.getPrecision result 
    
--{- atan using Newton -}
--
--atanNewton_RA :: 
--    (RA.ERIntApprox ira) => 
--    EffortIndex -> ra -> ra
--    
--atanNewton_RA i x = 
--    erNewton_FullArgs 
--        ( \ i y -> (erTan_RA i y) - x, erTanDeriv_RA) 
--        (RA.setMinGranularityOuter (effIx2gran i) (x))
--        (RA.setMinGranularityOuter (effIx2gran i) 1) 
--        (fromInteger $ toInteger i)
--        i

{- tan -}

erTan_R :: 
    (RA.ERIntApprox ira) => 
    EffortIndex -> ira -> ira
    
erTan_R ix x =
    (erSine_R ix x) / (erCosine_R ix x)

erTanDeriv_R ix x = 
    recip $ abs $ cosx * cosx
    where
    cosx = erCosine_R ix x


{- pi -}

{-|
    pi using Bellard's formula
    
    Convergence properties:
    
    * shrinking sequence
     
    * rate at least 2^(-i).
    
-}
erPi_R :: 
    (RA.ERIntApprox ira) => 
    EffortIndex -> ira
erPi_R = piBellard_R

-- | pi using atan 
piAtan_R ::
    (RA.ERIntApprox ira) => 
    EffortIndex -> ira
piAtan_R ix =
    (*) 4 $ atanEuler_R ix 1

{-|
    pi using Bellard's formula
    (see <http://en.wikipedia.org/wiki/Computing_π>)
    
    Convergence properties:
    
    * shrinking sequence
     
    * rate at least 2^(-i).
    
-}
piBellard_R ::
    (RA.ERIntApprox ira) => 
    EffortIndex -> ira
piBellard_R ix =
    r1over64 * (sum $ reverse $ bellardTerms 0 (10 + (ix `div` 10)) (1,z,z))
    {- 
      sum from the smallest to the largest 
      (got this trick from Martin Escardo who said he got it from Andrej Bauer)
      
      the rounding error dominates the truncation error to such
      a degree that the truncation error can be safely left out
      
      each bellard term contributes 10 binary digits that the following terms
      do not influence
    -} 
    where
    gran = max 0 (effIx2gran ix) + 10
    r1over64 = (RA.setMinGranularityOuter gran 1) / 64
    r1over1024 = (RA.setMinGranularityOuter gran 1) / 1024
    z = RA.setMinGranularityOuter gran 0
    bellardTerms n nMax (mult, r4n, r10n)
        | n >= nMax = []
        | otherwise =
             termN : rest
        where
        rest = 
            bellardTerms (n + 1) nMax (- mult * r1over1024, r4n + 4, r10n + 10)
        termN = 
            mult * bellardSum
        bellardSum =
            -- sum from the smallest to the largest
            (recip $ r10n + 9)
            - (recip $ r4n + 3)
            - 4 * ((recip $ r10n + 7) + (recip $ r10n + 5))
            - (64 / (r10n + 3))
            - (32 / (r4n + 1))
            + (256 / (r10n + 1)) 
    
    