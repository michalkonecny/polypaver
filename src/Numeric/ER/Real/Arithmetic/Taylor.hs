{-|
    Module      :  Numeric.ER.Real.Arithmetic.Taylor
    Description :  implementation of Taylor series
    Copyright   :  (c) Amin Farjudian, Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Taylor series related functions.
-}
module Numeric.ER.Real.Arithmetic.Taylor where

import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.BasicTypes.ExtendedInteger as EI
import Numeric.ER.BasicTypes
import Numeric.ER.Misc


erTaylor_R
    :: (RA.ERIntApprox ira)
    => EffortIndex
    -> (Int -> ira) -- ^ coefficients of the Taylor series
    -> (Int -> ira) -- ^ function to estimate the n'th derivative between a and x
    -> ira -- ^ centre of the Taylor Expansion
    -> ira 
    -> ira
erTaylor_R ix coefSeq derivBounds a x =
    erTaylor_R_FullArgs coefSeq derivBounds n a gran x
    where
    n = fromInteger ix
    gran = fromInteger $ toInteger $ ix

erTaylor_R_FullArgs
    :: (RA.ERIntApprox ira)
    => (Int -> ira)  -- ^ coefficients of the Taylor series
    -> (Int -> ira) -- ^ function to estimate the n'th derivative between a and x
    -> Int -- ^ use this many elements of the series (+ account for error appropriately)
    -> ira -- ^ centre of the Taylor Expansion
    -> Granularity -- ^ make all constants have this granularity, thus influencing rounding errors
    -> ira 
    -> ira
erTaylor_R_FullArgs coefSeq derivBounds n a gran x = 
    rec_apTaylor (RA.setMinGranularityOuter gran 0) 0
    where
    rec_apTaylor i j
        | n > j = (coefSeq(j)) + 
                        ((x - a)/(i+1)) * (rec_apTaylor (i+1) (j+1))
        | n == j = derivBounds n
        | otherwise = 
            error "Numeric.ER.Real.Arithmetic.Taylor.hs: erTaylor_RA_FullArgs: The index n cannot be negative"

{-|
    A Taylor series for exponentiation.    
-}
erExp_Tay_Opt_R
    :: (RA.ERIntApprox ira)
    => EffortIndex
    -> ira
    -> ira
erExp_Tay_Opt_R ix x 
    | x `RA.refines` (-RA.plusInfinity) = 
--        unsafePrintReturn
--        (
--            "erExp_Tay_Opt_R (x `RA.refines` (-RA.plusInfinity)): "
--            ++ "\n x = " ++ show x
--            ++ "\n ix = " ++ show ix
--            ++ "\n result = "
--        ) $
        0 -- -infty is not handled well by the Taylor formula
    | otherwise = 
--        unsafePrintReturn
--        (
--            "erExp_Tay_Opt_R: "
--            ++ "\n x = " ++ show x
--            ++ "\n ix = " ++ show ix
--            ++ "\n result = "
--        ) $
        1 + (te ix x (RA.setMinGranularityOuter gran 1))
    where
    gran = effIx2gran ix
    te steps x i
        | steps > 0 =
            (x/i) * (1 + (te (steps - 1) x (i + 1)))
        | steps == 0 = 
            errorBound
            where
            errorBound = 
                (x/i) * ithDerivBound
            ithDerivBound 
                | xCeiling == EI.MinusInfinity = -- certainly -infty:
                    0
                | xCeiling < 0 = -- certainly negative:
                    pow26xFloor RA.\/ 1
                | xFloor > 0 = -- certainly positive:
                    1 RA.\/ pow28xCeiling
                | otherwise = -- could contain 0:
                    pow26xFloor RA.\/ pow28xCeiling
                where
                (xFloor, xCeiling) = RA.integerBounds x
                pow26xFloor 
                    | xFloor == EI.MinusInfinity =
                        0
                    | otherwise = 
                        ((RA.setMinGranularityOuter gran 26)/10) ^^ xFloor 
                            -- lower estimate of e^x
                pow28xCeiling 
                    | xCeiling == EI.PlusInfinity =
                        (RA.plusInfinity)
                    | otherwise = 
                        ((RA.setMinGranularityOuter gran 28)/10) ^^ xCeiling 
                            -- upper estimate of e^x

{-
 The sine and cosine are implemented in almost exactly the same way 
-}

{-|
    A Taylor series for sine.    
-}
erSine_Tay_Opt_R
    :: (RA.ERIntApprox ira)
    => EffortIndex
    -> ira
    -> ira
erSine_Tay_Opt_R ix x = 
    taylor_seg ix x (RA.setMinGranularityOuter gran 1)
    where
    gran = effIx2gran ix
    taylor_seg i x n -- 'i' for iterator
        | i > 0  = x - (x*x)/((n+1)*(n+2)) * (taylor_seg (i-2) x (n+2))
        | otherwise = errorRegion
        where 
        errorRegion = (- x) RA.\/ x
                    
        
{-|
    A Taylor series for cosine.    
-}
erCosine_Tay_Opt_R 
    :: (RA.ERIntApprox ira) 
    => EffortIndex 
    -> ira
    -> ira
erCosine_Tay_Opt_R ix x = taylor_seg ix x (RA.setMinGranularityOuter gran 1)
    where
    gran = effIx2gran ix
    taylor_seg i x n -- 'i' for iterator
        | i > 0  = 1 - ((x*x)/(n*(n+1))) * (taylor_seg (i-2) x (n+2))
        | otherwise = errorRegion
        where 
        errorRegion = (-1) RA.\/ (1)

   
                
{-| Natural Logarithm: The following is a code for obtaining natural
     logarithm using taylor series. Note that it only works for 
     x in [ 1, 2]. For other values, a scaling by factors of e^q is
     best to do, i.e. if x is not in [1,2], then find some ratioal number                
     q where exp(q) * x is in [1,2]. Then you have:
     log ( exp(q) * m)  = q + log(m)
-}

{-| Coefficients of the taylor series around a=1 -}
--logTayCoefs
--    :: (RA.ERIntApprox ira)
--    =>    Int -- up to how many terms of the Taylor series is desired
--    -> Int
--    -> ra
--    
--logTayCoefs n i 
----    | i < 0 = error "ERTaylor.logTayCoefs: Negative n for the n-th term of Taylor series for logarithm"
--    | i == 0 = 0
--    | i == n = fromInteger $ toInteger $ amFact(n-1)    
--    | otherwise = fromInteger $ toInteger $ ((-1)^(i-1) * amFact(i-1))
--    where
--        amFact (m) = product [1..m]
        
--logTay_RA
--    :: (RA.ERIntApprox ira)
--    => EffortIndex
--    -> ra
--    -> ra
--    
--logTay_RA i = erTaylor_RA_FullArgs (logTayCoefs $fromInteger $ toInteger $ i) 
--                (100000) (setMinGranularity (effIx2gran i) 1) (effIx2gran i)
--    
--    
--logTay 
--    :: (RA.ERIntApprox ira)    
--    => (ConvergRealSeq ra)
--    -> (ConvergRealSeq ra)
--logTay = convertFuncRA2Seq logTay_RA    
                    
