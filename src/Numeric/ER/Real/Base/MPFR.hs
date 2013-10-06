{-# LANGUAGE CPP #-}
-- #define USE_MPFR
{-|
    Module      :  Numeric.ER.Real.Base.MPFR
    Description :  enabling MPFR dyadics as interval endpoints
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable (requires fenv.h)

    Make Ales Bizjak's Haskell interface to MPFR an instance of 
    'B.ERRealBase'.
     
    If compiled without USE_MPFR, this module is empty.
-}
module Numeric.ER.Real.Base.MPFR
(
#ifdef USE_MPFR
    MPFR
#endif
)
where

import qualified Numeric.ER.Real.Base as B
import qualified Numeric.ER.BasicTypes.ExtendedInteger as EI
import Numeric.ER.Misc

import Data.Binary

#ifdef USE_MPFR
-- need hmpfr >= 0.3
import qualified Data.Number.MPFR as M
import Data.Number.MPFR.Instances.Up

type MPFR = M.MPFR

instance Binary M.MPFR
    where
    get = error "Data.Number.Dyadic: Binary not implemented yet"
    put = error "Data.Number.Dyadic: Binary not implemented yet"


instance B.ERRealBase M.MPFR
    where
    typeName _ = "MPFR float"
    defaultGranularity _ = 30
    getApproxBinaryLog d 
        | d < 0 =
            error $ "ER.Real.Base.MPFR: getApproxBinaryLog: negative argument " ++ show d 
        | d == 0 = EI.MinusInfinity 
        | d >= 1 =
            fromInteger $ intLogUp 2 $ ceiling d
        | d < 1 =
            negate $ fromInteger $ intLogUp 2 $ ceiling $ recip d
    getGranularity = mPrec2gran . M.getPrec
    setMinGranularity g x 
        | g > xGran = B.setGranularity g x
        | otherwise = x
        where
        xGran = B.getGranularity x  
    setGranularity g = M.set M.Up (gran2mPrec g)
    getMaxRounding _ = 
        error "ER.Real.Base.MPFR: getMaxRounding undefined"
    isERNaN = M.isNaN
    erNaN = 0/0
    isPlusInfinity x = M.isInfinite x && x > 0
    plusInfinity = 1/0
    fromIntegerUp = fromInteger
    fromDouble = M.fromDouble M.Up 53
    toDouble = M.toDouble M.Up
    fromFloat = B.fromDouble . fromRational . toRational  
    toFloat = fromRational . toRational . B.toDouble
    showDiGrCmp numDigits showGran _showComponents f = 
        M.toStringExp (int2word numDigits) f
        ++ (if showGran then "{g=" ++ granS ++ "}" else "")
        where
        granS = show $ M.getPrec f
#endif
    
mPrec2gran = fromInteger . toInteger
gran2mPrec = fromInteger . toInteger
int2word = fromInteger . toInteger
