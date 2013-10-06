{-|
    Module      :  Numeric.ER.Real.Base
    Description :  class abstracting floats
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Abstraction over various fixed and floating point types as well
    as rational numbers.
    
    This module should be included qualified as is often given the local
    synonym B.
-}
module Numeric.ER.Real.Base
(
    module Numeric.ER.BasicTypes,
    ERRealBase(..)
)
where

import Numeric.ER.BasicTypes
import qualified Numeric.ER.BasicTypes.ExtendedInteger as EI

import Data.Typeable

{-|
    This class is an abstraction of a subset of real numbers
    with *upwards rounded* operations. 
-}
class (RealFrac rb, Ord rb, Show rb) => ERRealBase rb 
    where
    typeName :: rb -> String
    initialiseBaseArithmetic :: rb -> IO ()
    initialiseBaseArithmetic x = 
        do
        putStrLn $ "Base arithmetic: " ++ typeName x
    defaultGranularity :: rb -> Granularity
    getApproxBinaryLog :: rb -> EI.ExtendedInteger
    getGranularity :: rb -> Granularity
    setMinGranularity :: Granularity -> rb -> rb
    setGranularity :: Granularity -> rb -> rb
    {-|
        if @a@ is rounded to @ao@ then @|a-ao| <= getBaseMaxRounding ao@
    -}
    getMaxRounding :: rb -> rb
    isERNaN :: rb -> Bool
    erNaN :: rb
    isPlusInfinity :: rb -> Bool
    isMinusInfinity :: rb -> Bool
    isMinusInfinity = isPlusInfinity . negate
    plusInfinity :: rb
    minusInfinity :: rb
    minusInfinity = - plusInfinity
    fromIntegerUp :: Integer -> rb
    fromIntegerDown :: Integer -> rb
    fromIntegerDown i = negate $ fromIntegerUp $ - i
    fromDouble :: Double -> rb
    toDouble :: rb -> Double
    fromFloat :: Float -> rb
    toFloat :: rb -> Float
    showDiGrCmp :: 
        Int {- ^ number of decimal digits to show -} ->
        Bool {-^ whether to show granularity -} ->
        Bool {-^ whether to show internal structure -} ->
        rb -> String
