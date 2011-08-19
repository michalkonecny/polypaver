{-# LANGUAGE FlexibleInstances #-}

{-|
    Module      :  PolyPaver.Logic
    Description :  interface to decisions based on arithmetic  
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    Loosely coupled interface to decisions based on arithmetic.
-}
module PolyPaver.Logic where

import Numeric.ER.Misc
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.RnToRm.DefaultRepr

class TruthValue tv where
    not :: tv -> tv
    (&&) :: tv -> tv -> tv
    (||) :: tv -> tv -> tv
    (~>) :: tv -> tv -> tv
    fromBool :: Box (IRA BM) -> Bool -> tv
    leq :: FAPDOI BM -> FAPDOI BM -> tv
    includes :: FAPDOI BM -> FAPDOI BM -> tv
    split :: [Int] -> Box (IRA BM) -> tv -> (Int,(Box (IRA BM), Box (IRA BM)))
    decide :: Int -> tv -> Maybe Bool
    bot :: tv

instance TruthValue (Maybe Bool) where
    not (Just x) = Just (Prelude.not x)
    not Nothing = Nothing
    (Just True) && (Just True) = Just True
    (Just False) && _ = Just False
    _ && (Just False) = Just False
    _ && _ = Nothing
    (Just False) || (Just False) = Just False
    (Just True) || _ = Just True
    _ || (Just True) = Just True
    _ || _ = Nothing
    (Just True) ~> (Just False) = Just False 
    (Just False) ~> _ = Just True
    _ ~> (Just True) = Just True 
    _ ~> _ = Nothing
    fromBool _ = Just
    leq = RA.leqReals
    includes = RA.includes
    split thinvarids box tv =
        (var,DBox.split box var Nothing)
        where
        (var,_) = DBox.bestSplit splittablesubbox
        splittablesubbox =
            foldr DBox.delete box thinvarids
    decide _ tv = tv
    bot = Nothing

    
data TVM =
    TVMDecided Bool | TVMUndecided { tvmDistanceFromDecision :: Double }

instance TruthValue TVM where
    not (TVMDecided x) = TVMDecided (Prelude.not x)
    not tv = tv
    -- and:
    (TVMDecided False) && _ = TVMDecided False
    (TVMDecided True) && tv = tv
    _ && (TVMDecided False) = TVMDecided False
    tv && (TVMDecided True) = tv
    (TVMUndecided m1) && (TVMUndecided m2) = TVMUndecided (max m1 m2)
    -- or: 
    (TVMDecided True) || _ = TVMDecided True
    (TVMDecided False) || tv = tv
    _ || (TVMDecided True) = TVMDecided True
    tv || (TVMDecided False) = tv
    (TVMUndecided m1) || (TVMUndecided m2) = TVMUndecided (max m1 m2)
    -- implication:
    (TVMDecided False) ~> _ = TVMDecided True
    (TVMDecided True) ~> tv = tv
    _ ~> (TVMDecided True) = TVMDecided True
    tv ~> (TVMDecided False) = tv
    (TVMUndecided m1) ~> (TVMUndecided m2) = TVMUndecided (max m1 m2)

    fromBool _ = TVMDecided
    a `leq` b = 
        case a `RA.leqReals` b of
            Just result -> TVMDecided result
            Nothing -> TVMUndecided measure
        where
        measure = snd $ RA.doubleBounds $ a - b
    a `includes` b = 
        case a `RA.includes` b of
            Just result -> TVMDecided result
            Nothing -> TVMUndecided 1 -- TODO
    split thinvarids box tv =
        (var,DBox.split box var Nothing)
        where
        (var,_) = DBox.bestSplit splittablesubbox
        splittablesubbox =
            foldr DBox.delete box thinvarids
    decide _ (TVMDecided result) = Just result
    decide _ (TVMUndecided _) = Nothing
    bot = TVMUndecided (1/0) -- infinite badness...
    