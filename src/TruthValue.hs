{-# LANGUAGE FlexibleInstances #-}
module TruthValue where

import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.Real.DefaultRepr
import Numeric.ER.RnToRm.DefaultRepr
import qualified Numeric.ER.RnToRm.Approx as FA
import qualified Numeric.ER.RnToRm.Approx.DomTransl as DT
import Data.List
import qualified Prelude
import Prelude hiding ((&&), not,(||))
import Data.Maybe
import Data.Array
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Bits
import Data.Int

import Numeric.ER.RnToRm.UnitDom.Approx.IntervalOI

import Numeric.ER.Misc

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

data LocalGolbal a b 
    = LG a b
    deriving (Eq, Show, Read)

instance (TruthValue tva, TruthValue tvb) => TruthValue (LocalGolbal tva tvb) where
    not (LG a b) = LG (not a) (not b)
    LG a b && LG a' b' = LG (a && a') (b && b')
    LG a b || LG a' b' = LG (a || a') (b || b')
    LG a b ~> LG a' b' = LG (a ~> a') (b ~> b')
    fromBool box bool = LG (fromBool box bool) (fromBool box bool)
    leq f g = LG (leq f g) (leq f g)
    includes f g = LG (includes f g) (includes f g)
    split thinvarids box (LG tvlocal _) = split thinvarids box tvlocal
    decide i (LG _ tvgolbal) = decide i tvgolbal
    bot = LG bot bot

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

type Corners = Integer

{-
    The integer pair (t,f) codes that the ith corner evaluates to true if the ith
    bit is set in t and that the ith corner evaluates to false if the ith bit is 
    set in f. 
    
    Warning: no checks are performed to verify that the same bit is not set in
    both t and f.
-}
instance TruthValue ((Corners,Corners)) where
    not (a,b) = (b,a)
    (a,b) && (c,d) = (a.&.c,b.|.d)
    (a,b) || (c,d) = (a.|.c,b.&.d)
    (a,b) ~> (c,d) = (b.|.c,a.&.d)
    fromBool box b -- OBSERVE should only be called with nonempty box!
        | b = (tt,0)
        | otherwise = (0,tt)
        where
        tt = foldl' setBit 0 [0..2^dim-1]
        dim = length $ DBox.keys box
    leq = leqAtCorners
    includes = includesAtCorners
    split = splitWithCorners
    decide = decideWithCorners
    bot = (0,0)

type Corner = Int

leqAtCorners :: FAPDOI BM -> FAPDOI BM -> (Corners,Corners)
leqAtCorners left right =
    foldr 
        (\c (t,f) -> 
            case leqAtCorner c of 
                Just True -> (setBit t c,f)
                Just False -> (t,setBit f c)
                Nothing -> (t,f)) 
        (0,0) 
        corners 
    where
    corners = [0..2^dim-1]
    dim = length varids
    leqAtCorner :: Corner -> Maybe Bool
    leqAtCorner corner =
        (leftAtCorner corner) `RA.leqReals` (rightAtCorner corner)
    leftAtCorner corner =
        left `atCorner` corner
    rightAtCorner corner =
        right `atCorner` corner
    atCorner encl corner =
        head $ FA.evalAA (cornerToPoint corner) $ DT.erfnUnitApprox encl
--        head $ FA.eval (cornerToPoint corner) $ DT.erfnUnitApprox encl
    cornerToPoint :: Corner -> Box (IRA BM)
    cornerToPoint corner =
        DBox.fromList $ map (\i -> (i, if testBit corner i then 1 else -1)) varids
    varids = DBox.keys dom
    dom = DBox.union domL domR
    domL = FA.dom left
    domR = FA.dom right

includesAtCorners :: FAPDOI BM -> FAPDOI BM -> (Corners,Corners)
includesAtCorners left right =
    foldr 
        (\c (t,f) -> 
            case includesAtCorner c of 
                Just True -> (setBit t c,f)
                Just False -> (t,setBit f c)
                Nothing -> (t,f)) 
        (0,0) 
        corners 
    where
    corners = [0..2^dim-1]
    dim = length varids
    includesAtCorner :: Corner -> Maybe Bool
    includesAtCorner corner =
        (leftAtCorner corner) `RA.includes` (rightAtCorner corner)
    leftAtCorner corner =
        left `atCorner` corner
    rightAtCorner corner =
        right `atCorner` corner
    atCorner encl corner =
        head $ FA.evalAA (cornerToPoint corner) $ DT.erfnUnitApprox encl
    cornerToPoint :: Corner -> Box (IRA BM)
    cornerToPoint corner =
        DBox.fromList $ map (\i -> (i, if testBit corner i then 1 else -1)) varids
    varids = DBox.keys dom
    dom = DBox.union domL domR
    domL = FA.dom left
    domR = FA.dom right
{-
    Performs bestSplit on the variable(s) that have the minimum number of true corners,
    where this number is the max of the number of true corners between its two faces.
-}
splitWithCorners :: [Int] -> Box (IRA BM) -> (Corners, Corners) -> (Int,(Box (IRA BM), Box (IRA BM)))
splitWithCorners thinvarids box (t,f) =
    (varid,DBox.split box varid Nothing)
    where
--    (var,pt) = DBox.bestSplit box -- same splitting as Maybe Bool
    (varid,_) = DBox.bestSplit splittablesubbox
    splittablesubbox =
        IntMap.filterWithKey -- do split min entropy vars
            (\key val -> elem key minentropyvarids) $
            foldr DBox.delete box thinvarids -- only split nonthin domains
    {-
        varids with minimal average entropy
    -} 
    minentropyvarids =
        minvarids varidentropies initialminentropy [initialvarid]
        where
        minvarids [] _ l = l
        minvarids (h:t) minvalue varids
            | value < minvalue = 
                minvarids t value [varid]
            | value == minvalue =
                minvarids t minvalue (varid:varids)
            | otherwise =
                minvarids t minvalue varids 
            where
            (varid,value) = h
        (initialvarid,initialminentropy):varidentropies = entropies
        entropies = 
            map 
                (\(varid,faces) -> (varid,averageentropy faces)) $
                assocs rankedFaceArray
        averageentropy (zeroface,oneface) =
            0.5*(zerofaceentropy+onefaceentropy)
            where
            zerofaceentropy =
                entropy $ cornertrueinfaceprob zeroface
            onefaceentropy =
                entropy $ cornertrueinfaceprob oneface
            entropy p
                | 1 > p Prelude.&& p > 0 =
                    -p*(log p)-(1-p)*log(1-p)
                | otherwise =
                    0

    {-
        The probability that a corner is true in the face (t,f), given that undecided
        corners have equal probability of being true or false.
        
        Note: the assumption implies that Pr(corner is False) == 1-Pr(corner is True),
        this observation is used in faceEntropies.
    -}
    cornertrueinfaceprob (t,f) =
        (t-f)/2^dim + 1/2    
    {-
        varid -> zero-one-face pair ((t0,f0),(t1,f1)) where t0 counts the number of
        true corners in the zero-face of the variable, f0 counts the number of false
        corners in the zero-face of the variable etc.. 
        
        Explanation: each variable i maps to the two faces of a box determined by setting 
        the ith bit of a corner to zero or one, e.g. in a 2-dim box with corners 00,10,01 
        and 11 the zero face of variable 0 contains corners 00 and 01 while the one face 
        of variable 0 contains corners 10 and 11. 
    -}
    rankedFaceArray =
        accumArray 
            (\
                ((t0,f0),(t1,f1)) 
                ((t0',f0'),(t1',f1')) 
                -> 
                ((t0+t0',f0+f0'),(t1+t1',f1+f1')))
            ((0,0),(0,0)) 
            (0,dim-1) $
            concatMap assignTrueCornerToFaces truecorners ++
            concatMap assignFalseCornerToFaces falsecorners
    assignTrueCornerToFaces corner =
        map 
            (\varid -> 
                if testBit corner varid then 
                    (varid,((0,0),(1,0))) -- the true corner belongs to the one-face of varid
                else 
                    (varid,((1,0),(0,0)))) -- the true corner belongs to the zero-face of varid 
            varids
    truecorners = filter (testBit t) [0..2^dim-1]
    assignFalseCornerToFaces corner =
        map 
            (\varid -> 
                if testBit corner varid then 
                    (varid,((0,0),(0,1))) -- the false corner belongs to the one-face of varid
                else 
                    (varid,((0,1),(0,0)))) -- the false corner belongs to the zero-face of varid 
            varids 
    falsecorners = filter (testBit f) [0..2^dim-1]
    dim = length varids
    varids = DBox.keys box

{-
    
-}
decideWithCorners dim tv
    | and $ map (testBit f) cornerids = -- All corners are false
        Just False
    | and $ map (testBit t) cornerids = -- All corners are true
        Just True
    | otherwise =
        Nothing
    where
    cornerids = [0..2^dim-1]
    (t,f) = tv
