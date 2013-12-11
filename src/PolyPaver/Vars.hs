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

--import Numeric.ER.Misc

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IMap

showVar :: IMap.IntMap [Char] -> IMap.Key -> [Char]
showVar varNames var =
    case IMap.lookup var varNames of
        Nothing -> "x" ++ show var
        Just name -> name

getFormVarNames :: Form l -> IMap.IntMap String
getFormVarNames form =
    case form of
        Not arg -> getFormVarNames arg
        Or left right -> getFormVarNames2 left right
        And left right -> getFormVarNames2 left right
        Implies left right -> getFormVarNames2 left right
        Le _ left right -> getTermVarNames2 left right
        Leq _ left right -> getTermVarNames2 left right
        Ge _ left right -> getTermVarNames2 left right
        Geq _ left right -> getTermVarNames2 left right
        Eq _ left right -> getTermVarNames2 left right
        Neq _ left right -> getTermVarNames2 left right
        ContainedIn _ left right -> getTermVarNames2 left right 
        IsRange _ t1 t2 t3 -> getTermVarNames3 t1 t2 t3
        IsIntRange _ t1 t2 t3 -> getTermVarNames3 t1 t2 t3
        IsInt _ t -> getTermVarNames t
--        _ -> IMap.empty

getFormVarNames2 :: Form l -> Form l -> IMap.IntMap String
getFormVarNames2 f1 f2 = 
    (getFormVarNames f1) `IMap.union` (getFormVarNames f2)

getTermVarNames :: Term l -> IMap.IntMap String
getTermVarNames (Term (term, _)) =
    case term of
        Var varid name -> IMap.singleton varid name
        Plus left right -> getTermVarNames2 left right
        Hull left right -> getTermVarNames2 left right
        Minus left right -> getTermVarNames2 left right
        Neg arg -> getTermVarNames arg
        Times left right -> getTermVarNames2 left right
        Square arg -> getTermVarNames arg
        IntPower left right -> getTermVarNames2 left right
        Recip arg -> getTermVarNames arg
        Over left right -> getTermVarNames2 left right
        Abs arg -> getTermVarNames arg
        Min left right -> getTermVarNames2 left right
        Max left right -> getTermVarNames2 left right
        Sqrt arg -> getTermVarNames arg
        Exp arg -> getTermVarNames arg
        Sin arg -> getTermVarNames arg
        Cos arg -> getTermVarNames arg
        Atan arg -> getTermVarNames arg
        Integral ivarId _ivarName lower upper integrand ->
            (getTermVarNames2 lower upper)
            `IMap.union`
            (IMap.delete ivarId $ getTermVarNames integrand)
        FRound _ _ arg -> getTermVarNames arg
        FPlus _ _ left right -> getTermVarNames2 left right
        FMinus _ _ left right -> getTermVarNames2 left right
        FTimes _ _ left right -> getTermVarNames2 left right
        FSquare _ _ arg -> getTermVarNames arg
        FSqrt _ _ arg -> getTermVarNames arg
        FSin _ _ arg -> getTermVarNames arg
        FCos _ _ arg -> getTermVarNames arg
        FOver _ _ left right -> getTermVarNames2 left right
        FExp _ _ arg -> getTermVarNames arg
        _ -> IMap.empty

getTermVarNames2 :: Term l -> Term l -> IMap.IntMap String
getTermVarNames2 t1 t2 = 
    (getTermVarNames t1) `IMap.union` (getTermVarNames t2)
getTermVarNames3 :: Term l -> Term l -> Term l -> IMap.IntMap String
getTermVarNames3 t1 t2 t3 = 
    (getTermVarNames t1) `IMap.union` (getTermVarNames t2) `IMap.union` (getTermVarNames t3)

getFormFreeVars :: Form l -> Set.Set Int
getFormFreeVars form =
    case form of
        Not arg -> getFormFreeVars arg
        Or left right -> getFormFreeVars2 left right
        And left right -> getFormFreeVars2 left right
        Implies left right -> getFormFreeVars2 left right
        Le _ left right -> getTermFreeVars2 left right
        Leq _ left right -> getTermFreeVars2 left right
        Ge _ left right -> getTermFreeVars2 left right
        Geq _ left right -> getTermFreeVars2 left right
        Eq _ left right -> getTermFreeVars2 left right
        Neq _ left right -> getTermFreeVars2 left right
        ContainedIn _ left right -> getTermFreeVars2 left right
        IsIntRange _ arg1 arg2 arg3 -> (getTermFreeVars3 arg1 arg2 arg3)
        IsRange _ arg1 arg2 arg3 -> (getTermFreeVars3 arg1 arg2 arg3)
        IsInt _ arg -> getTermFreeVars arg
--        _ -> Set.empty

getFormFreeVars2 :: Form l -> Form l -> Set.Set Int
getFormFreeVars2 f1 f2 = 
    (getFormFreeVars f1) `Set.union` (getFormFreeVars f2)

getTermFreeVars :: Term l -> Set.Set Int
getTermFreeVars (Term (term, _)) =
    case term of
        Var varid _ -> Set.singleton varid
        Hull left right -> getTermFreeVars2 left right
        Plus left right -> getTermFreeVars2 left right
        Minus left right -> getTermFreeVars2 left right
        Neg arg -> getTermFreeVars arg
        Times left right -> getTermFreeVars2 left right
        Square arg -> getTermFreeVars arg
        IntPower left right -> getTermFreeVars2 left right
        Recip arg -> getTermFreeVars arg
        Over left right -> getTermFreeVars2 left right
        Abs arg -> getTermFreeVars arg
        Min left right -> getTermFreeVars2 left right
        Max left right -> getTermFreeVars2 left right
        Sqrt arg -> getTermFreeVars arg
        Exp arg -> getTermFreeVars arg
        Sin arg -> getTermFreeVars arg
        Cos arg -> getTermFreeVars arg
        Atan arg -> getTermFreeVars arg
        Integral ivarId _ivarName lower upper integrand ->
            (getTermFreeVars2 lower upper)
            `Set.union`
            (Set.delete ivarId $ getTermFreeVars integrand)
        FRound _ _ arg -> getTermFreeVars arg
        FPlus _ _ left right -> getTermFreeVars2 left right
        FMinus _ _ left right -> getTermFreeVars2 left right
        FTimes _ _ left right -> getTermFreeVars2 left right
        FSquare _ _ arg -> getTermFreeVars arg
        FSqrt _ _ arg -> getTermFreeVars arg
        FSin _ _ arg -> getTermFreeVars arg
        FCos _ _ arg -> getTermFreeVars arg
        FOver _ _ left right -> getTermFreeVars2 left right
        FExp _ _ arg -> getTermFreeVars arg
        _ -> Set.empty

getTermFreeVars2 :: Term l -> Term l -> Set.Set Int
getTermFreeVars2 t1 t2 =
    (getTermFreeVars t1) `Set.union` (getTermFreeVars t2)

getTermFreeVars3 :: Term l -> Term l -> Term l -> Set.Set Int
getTermFreeVars3 t1 t2 t3 =
    (getTermFreeVars t1) `Set.union` (getTermFreeVars t2) `Set.union` (getTermFreeVars t3)

renameVarsForm :: 
    (Int -> Int) -> Form l -> Form l  
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
            ContainedIn lab left right -> 
                ContainedIn lab (rnmT left) (rnmT right)
            IsRange lab arg1 arg2 arg3 -> 
                IsRange lab (rnmT arg1) (rnmT arg2) (rnmT arg3)
            IsIntRange lab arg1 arg2 arg3 -> 
                IsIntRange lab (rnmT arg1) (rnmT arg2) (rnmT arg3)
            IsInt lab arg -> 
                IsInt lab $ rnmT arg

renameVarsTerm :: 
    (Int -> Int) -> Term l -> Term l  
renameVarsTerm old2new = rnm
    where
    rnm (Term (term, maybeRangeBounds)) =
        Term (rnm' term, maybeRangeBounds)
    rnm' term =
        case term of
            Var varid s -> Var (old2new varid) s 
            Hull left right -> Hull (rnm left) (rnm right)
            Plus left right -> Plus (rnm left) (rnm right)
            Minus left right -> Minus (rnm left) (rnm right)
            Neg arg -> Neg $ rnm arg
            Times left right -> Times (rnm left) (rnm right)
            Square arg -> Square $ rnm arg
            IntPower left right -> IntPower (rnm left) (rnm right)
            Recip arg -> Recip $ rnm arg
            Over left right -> Over (rnm left) (rnm right)
            Abs arg -> Abs $ rnm arg
            Min left right -> Min (rnm left) (rnm right)
            Max left right -> Max (rnm left) (rnm right)
            Sqrt arg -> Sqrt $ rnm arg
            Exp arg -> Exp $ rnm arg
            Sin arg -> Sin $ rnm arg
            Cos arg -> Cos $ rnm arg
            Atan arg -> Atan $ rnm arg
            Integral ivarId ivarName lower upper integrand ->
                Integral ivarId ivarName (rnm lower) (rnm upper) (rnmIV integrand)
                where
                rnmIV = renameVarsTerm old2newIV
                old2newIV varId
                    | varId == ivarId = varId
                    | otherwise = old2newIV varId
            FRound rel abse arg -> FRound rel abse $ rnm arg
            FPlus rel abse left right -> FPlus rel abse (rnm left) (rnm right)
            FMinus rel abse left right -> FMinus rel abse (rnm left) (rnm right)
            FTimes rel abse left right -> FTimes rel abse (rnm left) (rnm right)
            FSquare rel abse arg -> FSquare rel abse $ rnm arg
            FSqrt rel abse arg -> FSqrt rel abse $ rnm arg
            FSin rel abse arg -> FSin rel abse $ rnm arg
            FCos rel abse arg -> FCos rel abse $ rnm arg
            FOver rel abse left right -> FOver rel abse (rnm left) (rnm right)
            FExp rel abse arg -> FExp rel abse $ rnm arg
            t -> t

normaliseVars :: Form l -> Form l
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
    (Int -> Maybe (Term' l)) -> Form l -> Form l  
substituteVarsForm old2new = subst
    where
    substT = substituteVarsTerm old2new
    subst form =
        case form of
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
            ContainedIn lab left right -> 
                ContainedIn lab (substT left) (substT right)
            IsRange lab arg1 arg2 arg3 -> 
                IsRange lab (substT arg1) (substT arg2) (substT arg3)
            IsIntRange lab arg1 arg2 arg3 -> 
                IsIntRange lab (substT arg1) (substT arg2) (substT arg3)
            IsInt lab arg -> 
                IsInt lab $ substT arg

substituteVarsTerm :: 
    (Int -> Maybe (Term' l)) -> Term l -> Term l  
substituteVarsTerm old2new = subst
    where
    subst (Term (term, maybeRangeBounds)) =
        Term (subst' term, maybeRangeBounds)
    subst' term =
        case term of
            Var varid _s -> case (old2new varid) of Just newTerm -> newTerm; _ -> term
            Hull left right -> Hull (subst left) (subst right)
            Plus left right -> Plus (subst left) (subst right)
            Minus left right -> Minus (subst left) (subst right)
            Neg arg -> Neg $ subst arg
            Times left right -> Times (subst left) (subst right)
            Square arg -> Square $ subst arg
            IntPower left right -> IntPower (subst left) (subst right)
            Recip arg -> Recip $ subst arg
            Over left right -> Over (subst left) (subst right)
            Abs arg -> Abs $ subst arg
            Min left right -> Min (subst left) (subst right)
            Max left right -> Max (subst left) (subst right)
            Sqrt arg -> Sqrt $ subst arg
            Exp arg -> Exp $ subst arg
            Sin arg -> Sin $ subst arg
            Cos arg -> Cos $ subst arg
            Atan arg -> Atan $ subst arg
            Integral ivarId ivarName lower upper integrand ->
                Integral ivarId ivarName (subst lower) (subst upper) (subst integrand)
            FRound rel abse arg -> FRound rel abse $ subst arg
            FPlus rel abse left right -> FPlus rel abse (subst left) (subst right)
            FMinus rel abse left right -> FMinus rel abse (subst left) (subst right)
            FTimes rel abse left right -> FTimes rel abse (subst left) (subst right)
            FSquare rel abse arg -> FSquare rel abse $ subst arg
            FSqrt rel abse arg -> FSqrt rel abse $ subst arg
            FSin rel abse arg -> FSin rel abse $ subst arg
            FCos rel abse arg -> FCos rel abse $ subst arg
            FOver rel abse left right -> FOver rel abse (subst left) (subst right)
            FExp rel abse arg -> FExp rel abse $ subst arg
            t -> t

{- |
   Remove hypotheses that feature only variables that are not in the conclusion
   nor are connected to the conclusion indirectly via other hypotheses.
-}
removeDisjointHypotheses :: Form l -> Form l
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
        findFix _ = error "removeDisjointHypotheses: conclusionTransVars: findFix: inappropriate parameter"
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
        