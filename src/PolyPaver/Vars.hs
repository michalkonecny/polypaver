{-|
    Module      :  PolyPaver.Vars
    Description :  manipulation of variables in formulas 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Manipulation of variables in formulas.
-}
module PolyPaver.Vars 
(
    showVar,
    getFormVarNames,
    getFormFreeVars,
    getTermFreeVars,
    renameVarsForm,
    renameVarsTerm,
    normaliseVars,
    removeDisjointHypotheses
)
where

import PolyPaver.Form

import Numeric.ER.Misc

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IMap

showVar varNames var =
    case IMap.lookup var varNames of
        Nothing -> "x" ++ show var
        Just name -> name

getFormVarNames :: Form -> IMap.IntMap String
getFormVarNames form =
    case form of
        Not arg -> getFormVarNames arg
        Or left right ->
            (getFormVarNames left) `IMap.union` (getFormVarNames right)
        And left right ->
            (getFormVarNames left) `IMap.union` (getFormVarNames right)
        Implies left right ->
            (getFormVarNames left) `IMap.union` (getFormVarNames right)
        Le left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Leq left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Ge left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Geq left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Eq left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Neq left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Ni left right -> 
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        _ -> IMap.empty

getTermVarNames :: Term -> IMap.IntMap String
getTermVarNames term =
    case term of
        Var varid name -> IMap.singleton varid name
        Plus left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Minus left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Neg arg -> getTermVarNames arg
        Abs arg -> getTermVarNames arg
--          Min left right ->
--          Max left right ->
        Times left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Square arg -> getTermVarNames arg
        Recip arg -> getTermVarNames arg
        Over left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Sqrt arg -> getTermVarNames arg
        Exp arg -> getTermVarNames arg
        Sin arg -> getTermVarNames arg
        Cos arg -> getTermVarNames arg
        Atan arg -> getTermVarNames arg
        Hull left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Round arg -> getTermVarNames arg
        FPlus left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        FMinus left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        FTimes left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        FSquare arg -> getTermVarNames arg
        FSqrt arg -> getTermVarNames arg
        FOver left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        FExp arg -> getTermVarNames arg
        _ -> IMap.empty

getFormFreeVars :: Form -> Set.Set Int
getFormFreeVars form =
    case form of
        Not arg -> getFormFreeVars arg
        Or left right ->
            (getFormFreeVars left) `Set.union` (getFormFreeVars right)
        And left right ->
            (getFormFreeVars left) `Set.union` (getFormFreeVars right)
        Implies left right ->
            (getFormFreeVars left) `Set.union` (getFormFreeVars right)
        Le left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Leq left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Ge left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Geq left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Eq left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Neq left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Ni left right -> 
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        _ -> Set.empty

getTermFreeVars :: Term -> Set.Set Int
getTermFreeVars term =
    case term of
        Var varid _ -> Set.singleton varid
        Plus left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Minus left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Neg arg -> getTermFreeVars arg
        Abs arg -> getTermFreeVars arg
--          Min left right ->
--          Max left right ->
        Times left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Square arg -> getTermFreeVars arg
        Recip arg -> getTermFreeVars arg
        Over left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Sqrt arg -> getTermFreeVars arg
        Exp arg -> getTermFreeVars arg
        Sin arg -> getTermFreeVars arg
        Cos arg -> getTermFreeVars arg
        Atan arg -> getTermFreeVars arg
        Hull left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Round arg -> getTermFreeVars arg
        FPlus left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        FMinus left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        FTimes left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        FSquare arg -> getTermFreeVars arg
        FSqrt arg -> getTermFreeVars arg
        FOver left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        FExp arg -> getTermFreeVars arg
        _ -> Set.empty

renameVarsForm :: 
    (Int -> Int) -> Form -> Form  
renameVarsForm old2new = rnm
    where
    rnmT = renameVarsTerm old2new
    rnm form =
        case form of
            Not arg -> Not $ rnm arg
            Or left right ->
                Or (rnm left) (rnm right)
            And left right ->
                And (rnm left) (rnm right)
            Implies left right ->
                Implies (rnm left) (rnm right)
            Le left right ->
                Le (rnmT left) (rnmT right)
            Leq left right ->
                Leq (rnmT left) (rnmT right)
            Ge left right ->
                Ge (rnmT left) (rnmT right)
            Geq left right ->
                Geq (rnmT left) (rnmT right)
            Eq left right ->
                Eq (rnmT left) (rnmT right)
            Neq left right ->
                Neq (rnmT left) (rnmT right)
            Ni left right -> 
                Ni (rnmT left) (rnmT right)
            f -> f

renameVarsTerm :: 
    (Int -> Int) -> Term -> Term  
renameVarsTerm old2new = rnm
    where
    rnm term =
        case term of
            Var varid s -> Var (old2new varid) s 
            Plus left right ->
                Plus (rnm left) (rnm right)
            Minus left right ->
                Minus (rnm left) (rnm right)
            Neg arg -> Neg $ rnm arg
            Abs arg -> Abs $ rnm arg
    --          Min left right ->
    --          Max left right ->
            Times left right ->
                Times (rnm left) (rnm right)
            Square arg -> Square $ rnm arg
            Recip arg -> Recip $ rnm arg
            Over left right ->
                Over (rnm left) (rnm right)
            Sqrt arg -> Sqrt $ rnm arg
            Exp arg -> Exp $ rnm arg
            Sin arg -> Sin $ rnm arg
            Cos arg -> Cos $ rnm arg
            Atan arg -> Atan $ rnm arg
            Hull left right ->
                Hull (rnm left) (rnm right)
            Round arg -> Round $ rnm arg
            FPlus left right ->
                FPlus (rnm left) (rnm right)
            FMinus left right ->
                FMinus (rnm left) (rnm right)
            FTimes left right ->
                FTimes (rnm left) (rnm right)
            FSquare arg -> FSquare $ rnm arg
            FSqrt arg -> FSqrt $ rnm arg
            FOver left right ->
                FOver (rnm left) (rnm right)
            FExp arg -> FExp $ rnm arg
            t -> t

normaliseVars :: Form -> Form
normaliseVars form =
    renameVarsForm old2new form
    where
    old2new old =
        case Map.lookup old old2newMap of
            Just new -> new
            Nothing -> error "normaliseVars: internal error"
    old2newMap =
        Map.fromAscList $ zip (Set.toAscList varSet) [0..]
    varSet = getFormFreeVars form

removeDisjointHypotheses :: Form -> Form
removeDisjointHypotheses form
    =
    rmHyps form
    where
    rmHyps (Implies h c)
        | disjoint h = rmHyps c 
        | otherwise = (Implies (rmConj h) (rmHyps c))
    rmHyps f = f
    rmConj (And h1 h2)
        | disjoint h1 = rmConj h2
        | disjoint h2 = rmConj h1
        | otherwise = And (rmConj h1) (rmConj h2)
    rmConj f = f 
    disjoint h 
        = Set.null $ Set.intersection conclusionTransVars (getFormFreeVars h)
    conclusionTransVars
        =
        findFix $ iterate (addRelatedVarsHyps form) conclusionVars
        where
        findFix (a1: a2 : rest)
            | a1 == a2 = a1
            | otherwise = findFix (a2 : rest)
        addRelatedVarsHyps (Implies h c) vars
            = addRelatedVarsHyps c $ addRelatedVarsConj h vars
        addRelatedVarsHyps _ vars = vars
        addRelatedVarsConj (And h1 h2) vars
            = addRelatedVarsConj h2 $ addRelatedVarsConj h1 vars
        addRelatedVarsConj f vars 
            | Set.null $ Set.intersection fVars vars = vars -- disjoint, do not merge fVars
            | otherwise = Set.union vars fVars -- not disjoint, fVars are all related to vars by this hypothesis
            where
            fVars = getFormFreeVars f
    conclusionVars
        = getFormFreeVars conclusion
    conclusion
        = getConclusion form
        