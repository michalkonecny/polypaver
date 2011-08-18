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
