{-|
    Module      :  Numeric.ER.BasicTypes.ExtendedInteger
    Description :  integer with infinities 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    An arbitrary sized integer type with additional +infinity and -infinity.
    
    To be imported qualified, usually with prefix EI. 
-}
module Numeric.ER.BasicTypes.ExtendedInteger 
(
    ExtendedInteger(..),
    isInfinite, binaryLog, take
)
where

import Prelude hiding (isInfinite, take)
import qualified Prelude

data ExtendedInteger
    = MinusInfinity | Finite Integer | PlusInfinity
    deriving (Eq)

isInfinite :: ExtendedInteger -> Bool
isInfinite MinusInfinity = True
isInfinite PlusInfinity = True
isInfinite _ = False

{-|
    the smallest integer i for which 2^i <=  abs n
-}
binaryLog :: ExtendedInteger -> ExtendedInteger
binaryLog PlusInfinity = PlusInfinity
binaryLog MinusInfinity = PlusInfinity
binaryLog (Finite n) 
    | n < 0 = binaryLog (Finite (- n))
    | n == 0 = MinusInfinity
    | otherwise = -- (n > 0)
        -- how to do this fast?
        intBinaryLog n

intBinaryLog n 
    | n > 1 = 1 + (intBinaryLog (n `div` 2))
    | n == 1 = 0

instance Show ExtendedInteger where
    show MinusInfinity = "-InfInt"
    show PlusInfinity = "+InfInt"
    show (Finite i) = show i

take :: ExtendedInteger -> [a] -> [a]
take MinusInfinity _ = error "takeEI called with MinusInfinity"
take PlusInfinity list = list
take (Finite n) list = Prelude.take (fromInteger n) list

instance Ord ExtendedInteger where
    compare MinusInfinity MinusInfinity = EQ
    compare MinusInfinity _ = LT
    compare _ MinusInfinity = GT
    compare PlusInfinity PlusInfinity = EQ
    compare PlusInfinity _ = GT
    compare _ PlusInfinity = LT
    compare (Finite i1) (Finite i2) =
        compare i1 i2

instance Num ExtendedInteger where
    fromInteger i = Finite i
    {- abs -}
    abs MinusInfinity = PlusInfinity
    abs PlusInfinity = PlusInfinity
    abs (Finite i) = Finite $ abs i
    {- signum -}
    signum ei
        | ei < 0 = -1
        | ei > 0 = 1
        | otherwise = 0
    {- negate -}
    negate (Finite i) = Finite (-i)
    negate MinusInfinity = PlusInfinity
    negate PlusInfinity = MinusInfinity
    {- addition -}
    PlusInfinity + MinusInfinity = 
        error "cannot add PlusInfinity and MinusInfinity"
    MinusInfinity + PlusInfinity = 
        error "cannot add PlusInfinity and MinusInfinity"
    PlusInfinity + ei = PlusInfinity
    ei + PlusInfinity = PlusInfinity
    MinusInfinity + ei = MinusInfinity
    ei + MinusInfinity = MinusInfinity
    (Finite i1) + (Finite i2) = Finite $ i1 + i2
    {- multiplication -}
    ei1 * ei2 | ei1 > ei2 = ei2 * ei1
    MinusInfinity * ei 
        | ei < 0 = PlusInfinity
        | ei > 0 = MinusInfinity
        | otherwise = error "cannot multiply MinusInfinity and 0"
    ei * PlusInfinity
        | ei < 0 = MinusInfinity
        | ei > 0 = PlusInfinity
        | otherwise = error "cannot multiply PlusInfinity and 0"
    (Finite i1) * (Finite i2) = Finite $ i1 * i2

instance Enum ExtendedInteger where
    toEnum i = Finite $ toInteger i
    fromEnum (Finite i) = fromInteger i
    fromEnum _ = error "infinite integers cannot be enumerated"

instance Real ExtendedInteger where
    toRational (Finite i) = toRational i
    toRational _ = error "infinite integers cannot be converted to rational"
    
instance Integral ExtendedInteger where
    quotRem (Finite i) (Finite m) = 
        (Finite a, Finite b)
        where
        (a,b) = quotRem i m
    quotRem _ _ = error "cannot make a quotient involving an infinite integer"
    toInteger (Finite i) = i
    toInteger _ = error "infinite integers cannot be converted to Integer"
        