{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-|
    Module      :  Numeric.ER.BasicTypes.DomainBox.IntMap
    Description :  implementation of DomainBox based on Data.IntMap   
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    A simple implementation of the 'VariableID' and 'DomainBox' classes.
-}
module Numeric.ER.BasicTypes.DomainBox.IntMap 
(
    VarID, Box
)
where

import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainBoxMappable, DomainIntBox)

import Numeric.ER.Misc

import qualified Data.IntMap as IMap
import qualified Data.Set as Set

type VarID = Int
type Box ira = IMap.IntMap ira

instance VariableID VarID
    where
    newVarID prevVars 
        | Set.null prevVars = 0
        | otherwise =
            1 + (Set.findMax prevVars)
    showVar v
        | v == 0 = "x"
        | otherwise = "x" ++ show v

instance (Show val) => (DomainBox (Box val) VarID val)
    where
    noinfo = IMap.empty
    isNoinfo = IMap.null
    size = IMap.size
    unary r = IMap.singleton defaultVar r
    singleton = IMap.singleton
    toList = IMap.toList
    fromList = IMap.fromList
    toAscList = IMap.toAscList
    fromAscList = IMap.fromAscList
--    toMap = id
--    fromMap = id
    compare compareVals b1 b2 =
        compareListsWith comparePairs (IMap.toList b1) (IMap.toList b2)
        where
        comparePairs (k1,v1) (k2,v2) =
            compareComposeMany
                [
                    compare k1 k2,
                    compareVals v1 v2
                ]
             
    adjust = IMap.adjust
    insert = IMap.insert
    insertWith = IMap.insertWith
    delete = IMap.delete
    member = IMap.member 
    notMember = IMap.notMember
    union = IMap.union 
    unionWith = IMap.unionWith 
    elems = IMap.elems
    keys = IMap.keys
    filter = IMap.filter
    fold = IMap.fold
    foldWithKey = IMap.foldWithKey
    zipWith f b1 b2 = 
        applyF (IMap.toAscList b1) (IMap.toAscList b2)
        where
        applyF [] _ = []
        applyF _ [] = []
        applyF bl1@((k1,v1):rest1) bl2@((k2,v2):rest2) 
            | k1 == k2 = 
                (k1, f v1 v2) : (applyF rest1 rest2)
            | k1 < k2 = applyF rest1 bl2
            | otherwise = applyF bl1 rest2 
    zipWithDefault defaultValue f b1 b2 = 
        applyF (IMap.toAscList b1) (IMap.toAscList b2)
        where
        applyF [] [] = []
        applyF bl1@((k1,v1):rest1) [] =
            (k1, f v1 defaultValue) : (applyF rest1 [])
        applyF [] bl2@((k2,v2):rest2) =
            (k2, f defaultValue v2) : (applyF [] rest2)
        applyF bl1@((k1,v1):rest1) bl2@((k2,v2):rest2) 
            | k1 == k2 = 
                (k1, f v1 v2) : (applyF rest1 rest2)
            | k1 < k2 = 
                (k1, f v1 defaultValue) : (applyF rest1 bl2)
            | otherwise =  
                (k2, f defaultValue v2) : (applyF bl1 rest2)
    zipWithDefaultSecond defaultValue f b1 b2 = 
        applyF (IMap.toAscList b1) (IMap.toAscList b2)
        where
        applyF [] _ = []
        applyF bl1@((k1,v1):rest1) [] =
            (k1, f v1 defaultValue) : (applyF rest1 [])
        applyF bl1@((k1,v1):rest1) bl2@((k2,v2):rest2) 
            | k1 == k2 = 
                (k1, f v1 v2) : (applyF rest1 rest2)
            | k1 < k2 = 
                (k1, f v1 defaultValue) : (applyF rest1 bl2)
            | otherwise =  
                applyF bl1 rest2
    findWithDefault = IMap.findWithDefault
    lookup locspec var dom =
        IMap.findWithDefault err var dom
        where
        err =
            error $
                locspec ++ "DomainBox.IntMap lookup: domain box " ++ show dom 
                ++ " ignores variable " ++ show var

instance (Show val1, Show val2) => 
    (DomainBoxMappable (Box val1) (Box val2) VarID val1 val2)
    where
    map = IMap.map
    mapWithKey = IMap.mapWithKey
    intersectionWith = IMap.intersectionWith
    difference = IMap.difference

instance (RA.ERIntApprox ira) => DomainIntBox (Box ira) VarID ira
    where
    compatible dom1 dom2 =
        foldl (&&) True $ map snd $
            DBox.zipWith RA.equalIntervals dom1 dom2
    unify locspec dom1 dom2
        | DBox.compatible dom1 dom2 =
            IMap.union dom1 dom2
        | otherwise =
            error $
                locspec ++ "incompatible domains " ++ show dom1 ++ " and " ++ show dom2
    bestSplit domB =
        (var, (varDom, pt))
        where
        pt = 
            RA.defaultBisectPt varDom
        (_, (varDom, var)) = 
            foldl findWidestVar (0, err) $ IMap.toList domB
        err =
            error $ "DomainBox: bestSplit: failed to find a split for " ++ show domB 
        findWidestVar (prevWidth, prevRes) (v, d)
            | currWidth `RA.leqSingletons` prevWidth = (prevWidth, prevRes)
            | otherwise = (currWidth, (d, v))
            where
            currWidth = snd $ RA.bounds $ domHI - domLO
            (domLO, domHI) = RA.bounds d
    split domB var maybePt = 
        (IMap.insert var varDomL domB, 
         IMap.insert var varDomR domB)
        where
        varDomL = varDomLO RA.\/ pt
        varDomR = pt RA.\/ varDomHI
        pt = 
            case maybePt of
                Nothing -> varDomMid
                Just pt | pt `RA.refines` varDom -> pt
                Just pt -> 
                    error $  
                        "ER.DomainBox.IntMap: split given an invalid split point " 
                        ++ show pt ++ " for the domain box " ++ show domB 
                        ++ " and split variable " ++ show var 
        (varDomLO, varDomMid, varDomHI, _) = RA.exactMiddle varDom
        varDom = DBox.lookup "DomainBox.IntMap: split: " var domB
    classifyPosition dom sdom =    
        (away, touch, intersect, inside)
            where
            (away, touch, inside, intersect) =
                foldl addDimension (True, True, True, False) awayTouchInsides
            addDimension 
                    (prevAway, prevTouch, prevInside, prevIntersect) 
                    (thisAway, thisTouch, thisInside, thisIntersect) =
                (prevAway && thisAway, 
                 (prevTouch || prevAway) && (thisTouch || thisAway) && (prevTouch || thisTouch),
                 prevInside && thisInside,
                 prevIntersect || thisIntersect)
            awayTouchInsides =
                map snd $
                    DBox.zipWith classifyRA dom sdom
            classifyRA d sd =
                (outsideNoTouch, outsideTouch, inside,
                 not (outsideNoTouch || outsideTouch || inside))
                 where
                 outsideNoTouch = sdR < dL || dR < sdL
                 outsideTouch = sdR == dL || dR == sdL
                 inside = sdL =< dL && dR =< sdR
                 (==) = RA.eqSingletons
                 (<) = RA.ltSingletons
                 (=<) = RA.leqSingletons
                 (dL, dR) = RA.bounds d 
                 (sdL, sdR) = RA.bounds sd 
        

    