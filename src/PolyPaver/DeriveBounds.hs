{-|
    Module      :  PolyPaver.DeriveBounds
    Description :  work out bounds for variables using hypotheses 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

   Work out bounds for variables using hypotheses.
-}
module PolyPaver.DeriveBounds
(
    getBox
)
where

import PolyPaver.Form
import PolyPaver.Vars
import PolyPaver.Eval
import PolyPaver.PPBox
import PolyPaver.Logic (TVM(..))

import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Base as B

import Numeric.ER.Misc

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IMap

getBox :: 
    Form -> 
    Either String [(Int, (Rational, Rational))]
getBox form 
    | allGood = 
--        unsafePrint
--        (
--            "PolyPaver.DeriveBounds: getBox:"
--            ++ "\n 4 scans:"
--            ++ "\n" ++ (unlines $ map show $ take 4 boxSeq)
--        ) $
        Right $ map removeJust $ varRanges
    | otherwise = 
        Left errorMessage
    where
    allGood = and $ map isGood varRanges
    varRanges = IMap.toList box
    isGood (v, (Just _,Just _)) = True
    isGood _ = False
    removeJust (v, (Just l, Just r)) = (v, (l,r))
    errorMessage =
        unlines $ map reportBadVar $ filter (not . isGood) varRanges
        where
        reportBadVar (v, _) =
            "*** failed to derive a bound for variable " ++ showVar varNames v ++ " in formula " ++ showForm form 
    varSet = getFormFreeVars form
    varNames = getFormVarNames form
    initBox
        = IMap.fromAscList $ zip (Set.toAscList varSet) (repeat (Nothing, Nothing))
    box = findRepeat initBox $ tail $ boxSeq
    boxSeq = iterate (scanHypotheses form) initBox
    
scanHypotheses (Implies h c) =
    scanHypotheses c . scanHypothesis h 
scanHypotheses _ = id

scanHypothesis (And h1 h2) intervals = 
    (scanHypothesis h1 . scanHypothesis h2) intervals
scanHypothesis (Or h1 h2) intervals = 
    IMap.unionWith mergeWorse box1 box2
    where
    box1 = scanHypothesis h1 intervals 
    box2 = scanHypothesis h2 intervals
    mergeWorse (l1,r1) (l2,r2) = (minM l1 l2, maxM r1 r2)
    minM (Just a) (Just b) = Just $ min a b
    minM _ _ = Nothing
    maxM (Just a) (Just b) = Just $ max a b
    maxM _ _ = Nothing
scanHypothesis (Eq (Var v _) t) intervals = 
    IMap.insertWith updateUpper v val $
    IMap.insertWith updateLower v val intervals
    where
    val = evalT intervals t
scanHypothesis (Eq t (Var v _)) intervals = 
    IMap.insertWith updateUpper v val $
    IMap.insertWith updateLower v val intervals
    where
    val = evalT intervals t
scanHypothesis (Le (Var v _) t) intervals = 
    IMap.insertWith updateUpper v (evalT intervals t) intervals
scanHypothesis (Le t (Var v _)) intervals = 
    IMap.insertWith updateLower v (evalT intervals t) intervals
scanHypothesis (Leq (Var v _) t) intervals = 
    IMap.insertWith updateUpper v (evalT intervals t) intervals
scanHypothesis (Leq t (Var v _)) intervals = 
    IMap.insertWith updateLower v (evalT intervals t) intervals
scanHypothesis (Ge (Var v _) t) intervals = 
    IMap.insertWith updateLower v (evalT intervals t) intervals
scanHypothesis (Ge t (Var v _)) intervals = 
    IMap.insertWith updateUpper v (evalT intervals t) intervals
scanHypothesis (Geq (Var v _) t) intervals = 
    IMap.insertWith updateLower v (evalT intervals t) intervals
scanHypothesis (Geq t (Var v _)) intervals = 
    IMap.insertWith updateUpper v (evalT intervals t) intervals
scanHypothesis _ intervals = intervals
    
evalT ::
    (IMap.IntMap (Maybe Rational, Maybe Rational)) ->
    Term ->
    (Maybe Rational, Maybe Rational)
evalT intervals term 
    | termVarsBounded = (ifBoundedDown l, ifBoundedUp r)
    | otherwise = (Nothing, Nothing)
    where
    termVarsBounded =
        and $ map varBounded $ Set.toList termVars
        where
        varBounded v =
            case IMap.lookup v intervals of
                Just (Just l, Just r) -> True
                _ -> False
        
    termVars = getTermFreeVars term
    
    ((l,r),_) 
        = 
        RA.oiBounds $
        evalTerm (TVMDecided True) 1 100 0 10 box (5,32) term
        where
        box = 
            ppBoxFromIntervals (getTermVarNames term) $ 
                map removeJust $ filter varInTerm $ IMap.toList intervals
            where
            varInTerm (v, _) = Set.member v termVars
            removeJust (v, (Just l, Just r)) = (v, (l, r)) 
                -- v is bounded thanks to the termVarsBounded guard
            
    ifBoundedUp v
        | isInfinite d = Nothing 
        | otherwise = Just $ toRational d
        where
        (_,d) = RA.doubleBounds v
    
    ifBoundedDown v
        | isInfinite d = Nothing 
        | otherwise = Just $ toRational d
        where
        (d,_) = RA.doubleBounds v
    
    
updateUpper (_,Just u2) (l, Just u1) = (l, Just $ min u1 u2)
updateUpper (_,Just u2) (l, Nothing) = (l, Just $ u2)
updateUpper (_,Nothing) (l, Just u1) = (l, Just $ u1)
updateUpper (_,Nothing) (l, Nothing) = (l, Nothing)

updateLower (Just l2,_) (Just l1,u) = (Just $ max l1 l2, u)
updateLower (Just l2,_) (Nothing,u) = (Just $ l2, u)
updateLower (Nothing,_) (Just l1,u) = (Just $ l1, u)
updateLower (Nothing,_) (Nothing,u) = (Nothing, u)

    
findRepeat :: (Eq a) => a -> [a] -> a
findRepeat prev (next:rest)
    | prev == next = prev
    | otherwise = findRepeat next rest
findRepeat prev [] = prev

    