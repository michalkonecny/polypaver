{-# LANGUAGE ForeignFunctionInterface #-}
{-|
    Module      :  Numeric.ER.Real.Base.MachineDouble
    Description :  enabling Double's as interval endpoints
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable (requires fenv.h)

    Make 'Double' an instance of 'B.ERRealBase' as much as possible.    
-}
module Numeric.ER.Real.Base.MachineDouble
(
    initMachineDouble
)
where

import qualified Numeric.ER.Real.Base as B
import qualified Numeric.ER.BasicTypes.ExtendedInteger as EI
import Numeric.ER.Misc

import Foreign.C

{- 
    The following section is taken from Oleg Kiselyov's email
    http://www.haskell.org/pipermail/haskell/2005-October/016574.html
-}

type FP_RND_T = CInt  -- fenv.h

eFE_TONEAREST = 0
eFE_DOWNWARD = 0x400
eFE_UPWARD   = 0x800
eFE_TOWARDZERO = 0xc00

foreign import ccall "fenv.h fegetround" fegetround 
  :: IO FP_RND_T

foreign import ccall "fenv.h fesetround" fesetround
  :: FP_RND_T -> IO FP_RND_T
{- end of Oleg's code -}

{-|
    Set machine floating point unit to the upwards-directed rounding
    mode.  
    
    This procedure has to be executed before using 'Double' 
    as a basis for interval and polynomial arithmetic defined in this package.
-}
initMachineDouble :: IO ()
initMachineDouble =
    do
    currentRndMode <- fegetround
    case currentRndMode == eFE_UPWARD of
        True -> 
            putStrLn "already rounding upwards" 
        False ->
            do
            fesetround eFE_UPWARD
            putStrLn "switching to upwards rounding" 

instance B.ERRealBase Double
    where
    typeName _ = "double"
    initialiseBaseArithmetic x = 
		do
		putStr $ "Base arithmetic:" ++ B.typeName x ++ "; "
		initMachineDouble
    defaultGranularity _ = 53
    getApproxBinaryLog d 
        | d < 0 =
            error $ "ER.Real.Base.MachineDouble: getApproxBinaryLog: negative argument " ++ show d 
        | d == 0 = EI.MinusInfinity 
        | d >= 1 =
            fromInteger $ intLogUp 2 $ ceiling d
        | d < 1 =
            negate $ fromInteger $ intLogUp 2 $ ceiling $ recip d
        | otherwise = 
            error $ "ER.Real.Base.MachineDouble: getApproxBinaryLog: illegal argument " ++ show d 
    getGranularity _ = 53
    setMinGranularity _ = id
    setGranularity _ = id
    getMaxRounding _ = 0
    isERNaN f = isNaN f
    erNaN = 0/0
    isPlusInfinity f = isInfinite f && f > 0
    plusInfinity = 1/0
    fromIntegerUp i
        | i <= floor nearest = nearest
        | otherwise = nearestIncreased
        where
        nearestCeil = ceiling nearest
        nearest = fromInteger i
        nearestIncreased = encodeFloat (s+1) e
        (s,e) = decodeFloat nearest
    fromDouble = fromRational . toRational
    toDouble = fromRational . toRational
    fromFloat = fromRational . toRational
    toFloat = fromRational . toRational
    showDiGrCmp _numDigits _showGran _showComponents f = show f
    

