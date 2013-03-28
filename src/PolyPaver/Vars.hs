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
    getTermVarNames,
    getFormFreeVars,
    getTermFreeVars,
    renameVarsForm,
    renameVarsTerm,
    normaliseVars,
    substituteVarsForm,
    substituteVarsTerm,
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
        Predicate term -> getTermVarNames term
        Not arg -> getFormVarNames arg
        Or left right ->
            (getFormVarNames left) `IMap.union` (getFormVarNames right)
        And left right ->
            (getFormVarNames left) `IMap.union` (getFormVarNames right)
        Implies left right ->
            (getFormVarNames left) `IMap.union` (getFormVarNames right)
        Le _ left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Leq _ left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Ge _ left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Geq _ left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Eq _ left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Neq _ left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Ni _ left right -> 
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
        IsInt arg -> getTermVarNames arg
        Hull left right ->
            (getTermVarNames left) `IMap.union` (getTermVarNames right)
        Integral lower upper ivarId ivarName integrand ->
            (getTermVarNames lower) `IMap.union` (getTermVarNames upper)
            `IMap.union`
            (IMap.delete ivarId $ getTermVarNames integrand)
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
        Predicate term -> getTermFreeVars term
        Not arg -> getFormFreeVars arg
        Or left right ->
            (getFormFreeVars left) `Set.union` (getFormFreeVars right)
        And left right ->
            (getFormFreeVars left) `Set.union` (getFormFreeVars right)
        Implies left right ->
            (getFormFreeVars left) `Set.union` (getFormFreeVars right)
        Le _ left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Leq _ left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Ge _ left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Geq _ left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Eq _ left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Neq _ left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Ni _ left right -> 
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
        IsInt arg -> getTermFreeVars arg
        Hull left right ->
            (getTermFreeVars left) `Set.union` (getTermFreeVars right)
        Integral lower upper ivarId ivarName integrand ->
            (getTermFreeVars lower) `Set.union` (getTermFreeVars upper)
            `Set.union`
            (Set.delete ivarId $ getTermFreeVars integrand)
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
            Predicate term -> Predicate $ rnmT term
            Not arg -> Not $ rnm arg
            Or left right ->
                Or (rnm left) (rnm right)
            And left right ->
                And (rnm left) (rnm right)
            Implies left right ->
                Implies (rnm left) (rnm right)
            Le lab left right ->
                Le lab (rnmT left) (rnmT right)
            Leq lab left right ->
                Leq lab (rnmT left) (rnmT right)
            Ge lab left right ->
                Ge lab (rnmT left) (rnmT right)
            Geq lab left right ->
                Geq lab (rnmT left) (rnmT right)
            Eq lab left right ->
                Eq lab (rnmT left) (rnmT right)
            Neq lab left right ->
                Neq lab (rnmT left) (rnmT right)
            Ni lab left right -> 
                Ni lab (rnmT left) (rnmT right)
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
            IsInt arg -> IsInt $ rnm arg
            Hull left right ->
                Hull (rnm left) (rnm right)
            Integral lower upper ivarId ivarName integrand ->
                Integral (rnm lower) (rnm upper) ivarId ivarName (rnmIV integrand)
                where
                rnmIV = renameVarsTerm old2newIV
                old2newIV id 
                    | id == ivarId = id
                    | otherwise = old2newIV id
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

substituteVarsForm :: 
    (Int -> Maybe Term) -> Form -> Form  
substituteVarsForm old2new = subst
    where
    substT = substituteVarsTerm old2new
    subst form =
        case form of
            Predicate term -> Predicate $ substT term
            Not arg -> Not $ subst arg
            Or left right ->
                Or (subst left) (subst right)
            And left right ->
                And (subst left) (subst right)
            Implies left right ->
                Implies (subst left) (subst right)
            Le lab left right ->
                Le lab (substT left) (substT right)
            Leq lab left right ->
                Leq lab (substT left) (substT right)
            Ge lab left right ->
                Ge lab (substT left) (substT right)
            Geq lab left right ->
                Geq lab (substT left) (substT right)
            Eq lab left right ->
                Eq lab (substT left) (substT right)
            Neq lab left right ->
                Neq lab (substT left) (substT right)
            Ni lab left right -> 
                Ni lab (substT left) (substT right)
            f -> f

substituteVarsTerm :: 
    (Int -> Maybe Term) -> Term -> Term  
substituteVarsTerm old2new = subst
    where
    subst term =
        case term of
            Var varid s -> case (old2new varid) of Just newTerm -> newTerm; _ -> term
            Plus left right ->
                Plus (subst left) (subst right)
            Minus left right ->
                Minus (subst left) (subst right)
            Neg arg -> Neg $ subst arg
            Abs arg -> Abs $ subst arg
    --          Min left right ->
    --          Max left right ->
            Times left right ->
                Times (subst left) (subst right)
            Square arg -> Square $ subst arg
            Recip arg -> Recip $ subst arg
            Over left right ->
                Over (subst left) (subst right)
            Sqrt arg -> Sqrt $ subst arg
            Exp arg -> Exp $ subst arg
            Sin arg -> Sin $ subst arg
            Cos arg -> Cos $ subst arg
            Atan arg -> Atan $ subst arg
            IsInt arg -> IsInt $ subst arg
            Hull left right ->
                Hull (subst left) (subst right)
            Integral lower upper ivarId ivarName integrand ->
                Integral (subst lower) (subst upper) ivarId ivarName (subst integrand)
            Round arg -> Round $ subst arg
            FPlus left right ->
                FPlus (subst left) (subst right)
            FMinus left right ->
                FMinus (subst left) (subst right)
            FTimes left right ->
                FTimes (subst left) (subst right)
            FSquare arg -> FSquare $ subst arg
            FSqrt arg -> FSqrt $ subst arg
            FOver left right ->
                FOver (subst left) (subst right)
            FExp arg -> FExp $ subst arg
            t -> t


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
        