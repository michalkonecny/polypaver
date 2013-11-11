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
--import PolyPaver.Logic (TVM(..))

import qualified Numeric.ER.Real.Approx as RA
--import qualified Numeric.ER.Real.Base as B
--import Numeric.ER.Real.DefaultRepr

--import Numeric.ER.Misc

import qualified Data.Set as Set
--import qualified Data.Map as Map
import qualified Data.IntMap as IMap

import Data.Hashable

getBox :: 
    (HasDefaultValue l, Eq l, Hashable l) =>
    Form l ->
    Either String [(Int, (Rational, Rational), Bool)]
getBox form 
    | allGood = 
        Right $ map removeJustAddIsInt $ varRanges
    | otherwise = 
        Left errorMessage
    where
    allGood = and $ map isGood varRanges
    varRanges = IMap.toList box
    isGood (_v, (Just _,Just _)) = True
    isGood _ = False
    removeJustAddIsInt (v, (Just l, Just r)) = (v, (l,r), isInt)
        where
        isInt = v `Set.member` intVars
    removeJustAddIsInt _ = error "DeriveBounds: getBox: removeJustAddIsInt failed"
    intVars = getIntVarsFromHyps form
    errorMessage =
        unlines $ map reportBadVar $ filter (not . isGood) varRanges
        where
        reportBadVar (v, _) =
            "*** failed to derive a bound for variable " ++ showVar varNames v ++ " in formula " ++ showForm 10000 const form 
    varSet = getFormFreeVars form
    varNames = getFormVarNames form
    initBox
        = IMap.fromAscList $ zip (Set.toAscList varSet) (repeat (Nothing, Nothing))
    box = 
        findRepeat initBox $ tail $ 
--            unsafePrint
--            (
--                "PolyPaver.DeriveBounds: getBox:"
--                ++ "\n first <=4 scans:"
--                ++ "\n" ++ (unlines $ map show $ take 4 boxSeq)
--            ) $
            boxSeq
    boxSeq = 
        iterate (scanHypotheses form) initBox
    
getIntVarsFromHyps :: Form l -> Set.Set Int
getIntVarsFromHyps (Implies h c) =
    (getIntVars h) `Set.union` (getIntVarsFromHyps c)
getIntVarsFromHyps _ = Set.empty

getIntVars :: Form l -> Set.Set Int
getIntVars (And h1 h2) =
    (getIntVars h1) `Set.union` (getIntVars h2)
getIntVars (Or h1 h2) =
    (getIntVars h1) `Set.intersection` (getIntVars h2)
getIntVars (IsInt _ term) = getTermFreeVars term
getIntVars (IsIntRange _ term _ _) = getTermFreeVars term
getIntVars _ = Set.empty
    
scanHypotheses :: 
    (Eq l, HasDefaultValue l, Hashable l) =>
    Form l
    -> IMap.IntMap (Maybe Rational, Maybe Rational)
    -> IMap.IntMap (Maybe Rational, Maybe Rational)
scanHypotheses (Implies h c) =
    scanHypotheses c . scanHypothesis h 
scanHypotheses _ = id

scanHypothesis :: 
    (Eq l, HasDefaultValue l, Hashable l) =>
    Form l -> 
    IMap.IntMap (Maybe Rational, Maybe Rational) -> 
    IMap.IntMap (Maybe Rational, Maybe Rational)
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
    
scanHypothesis (IsRange lab t lower upper) intervals = 
    scanHypothesis ((Leq lab lower t) /\ (Leq lab t upper)) intervals
scanHypothesis (IsIntRange lab t lower upper) intervals =
    scanHypothesis (IsRange lab t lower upper) intervals
    
scanHypothesis (Eq _ _t1@(Term (Var v1 _, _)) _t2@(Term (Var v2 _, _))) intervals = 
    IMap.insert v1 val $
    IMap.insert v2 val $
    intervals
    where
    Just val1 = IMap.lookup v1 intervals
    Just val2 = IMap.lookup v2 intervals
    val = updateUpper val1 $ updateLower val1 $ val2
scanHypothesis (Eq _ (Term (Var v _, _)) t) intervals = 
    IMap.insertWith updateUpper v val $
    IMap.insertWith updateLower v val intervals
    where
    val = evalT intervals t
scanHypothesis (Eq _ t (Term (Var v _, _))) intervals = 
    IMap.insertWith updateUpper v val $
    IMap.insertWith updateLower v val intervals
    where
    val = evalT intervals t
    
scanHypothesis (Leq _ _t1@(Term (Var v1 _, _)) _t2@(Term (Var v2 _, _))) intervals = 
    IMap.insert v1 (updateUpper val2 val1) $
    IMap.insert v2 (updateLower val1 val2) $
    intervals
    where
    Just val1 = IMap.lookup v1 intervals
    Just val2 = IMap.lookup v2 intervals
scanHypothesis (Leq _ (Term (Var v _, _)) t) intervals = 
    IMap.insertWith updateUpper v (evalT intervals t) intervals
scanHypothesis (Leq _ t (Term (Var v _, _))) intervals = 
    IMap.insertWith updateLower v (evalT intervals t) intervals
-- reduce Le, Geq, Ge on equivalent Leq (note that we treat strict and non-strict the same way):
scanHypothesis (Le lab t1 t2) intervals = scanHypothesis (Leq lab t1 t2) intervals 
scanHypothesis (Geq lab t1 t2) intervals = scanHypothesis (Leq lab t2 t1) intervals
scanHypothesis (Ge lab t1 t2) intervals = scanHypothesis (Leq lab t2 t1) intervals

scanHypothesis _h@(ContainedIn _lab (Term (Var v _, _)) t) intervals =
--    unsafePrint
--    (
--        "scanHypothesis: " ++ showForm 100 False h
--        ++ "\n valV = " ++ show valV 
--        ++ "\n mvalTL = " ++ show mvalTL 
--        ++ "\n mvalTU = " ++ show mvalTU 
--    ) $
    IMap.insert v (updateLower (mvalTL, mvalTU) $ updateUpper (mvalTL, mvalTU) valV) $
    intervals
    where
    Just valV = IMap.lookup v intervals
    (mvalTL, mvalTU) = evalT intervals t
scanHypothesis _ intervals = intervals
    
evalT ::
    (HasDefaultValue l, Eq l, Hashable l) =>
    (IMap.IntMap (Maybe Rational, Maybe Rational)) ->
    Term  l ->
    (Maybe Rational, Maybe Rational)
evalT intervals term 
    | termVarsBounded = (ifBoundedDown l, ifBoundedUp r)
    | otherwise = (Nothing, Nothing)
    where
    termVarsBounded =
--        unsafePrint
--        ("PolyPaver.DeriveBounds: evalT:"
--         ++ "\n intervals = " ++ show intervals
--         ++ "\n term = " ++ show term
--         ++ "\n (l,r) = " ++ show (l,r)
--        ) $
        and $ map varBounded $ Set.toList termVars
        where
        varBounded v =
            case IMap.lookup v intervals of
                Just (Just _l, Just _r) -> True
                _ -> False
        
    termVars = getTermFreeVars term
    
    ((l,r),_) 
        = 
        RA.oiBounds $ fst $ snd $
        evalTerm 1 100 10 0 box False IMap.empty $ prepareTerm term
        where
        box = 
            ppBoxFromIntervals (IMap.map (const False) termVarNames) termVarNames $ 
                map removeJust $ filter varInTerm $ IMap.toList intervals
            where
            termVarNames = getTermVarNames term
            varInTerm (v, _) = Set.member v termVars
            removeJust (v, (Just l2, Just r2)) = (v, (l2, r2))
            removeJust _ = error "DeriveBounds: evalT: (l,r): removeJust failed"
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
    
    
updateUpper :: 
    Ord a =>
    (t, Maybe a) -> (t1, Maybe a) -> (t1, Maybe a)
updateUpper (_,Just u2) (l, Just u1) = (l, Just $ min u1 u2)
updateUpper (_,Just u2) (l, Nothing) = (l, Just $ u2)
updateUpper (_,Nothing) (l, Just u1) = (l, Just $ u1)
updateUpper (_,Nothing) (l, Nothing) = (l, Nothing)
--updateUpper _ _ = error "DeriveBounds: updateUpper failed"

updateLower :: 
    Ord a =>
    (Maybe a, t) -> (Maybe a, t1) -> (Maybe a, t1)
updateLower (Just l2,_) (Just l1,u) = (Just $ max l1 l2, u)
updateLower (Just l2,_) (Nothing,u) = (Just $ l2, u)
updateLower (Nothing,_) (Just l1,u) = (Just $ l1, u)
updateLower (Nothing,_) (Nothing,u) = (Nothing, u)
--updateLower _ _ = error "DeriveBounds: updateLower failed"

findRepeat :: (Eq a, Show a) => a -> [a] -> a
findRepeat prev (next:rest)
    | prev == next = prev
    | otherwise = findRepeat next rest
findRepeat prev [] = prev

    