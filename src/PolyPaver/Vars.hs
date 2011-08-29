{-|
    Module      :  PolyPaver.Vars
    Description :  manipulation of variables in formulas 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Manipulation of variables in formulas, including
    detecting domains of variables.
-}
module PolyPaver.Vars 
(
    getFormFreeVars,
    getTermFreeVars,
    renameVarsForm,
    renameVarsTerm,
    normaliseVars,
    getBox
)
where

import PolyPaver.Form

import qualified Data.Set as Set
import qualified Data.Map as Map

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
        Var varid -> Set.singleton varid
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
            Var varid -> Var $ old2new varid
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

getBox :: Form -> Either String [(Int, (Rational, Rational))]
getBox form =
    checkAllThere $ Map.toAscList boxMap
    where
    checkAllThere varRanges
        | allGood = Right $ map removeJust varRanges
        | otherwise = Left errorMessage
        where
        allGood = and $ map isGood varRanges
        isGood (v, (Just _,Just _)) = True
        isGood _ = False
        removeJust (v, (Just l, Just r)) = (v, (l,r))
        errorMessage =
            unlines $ map reportBadVar $ filter (not . isGood) varRanges
        reportBadVar (v, _) =
            "*** failed to derive a bound for variable " ++ show v ++ " in formula " ++ show form 
    varSet = getFormFreeVars form
    initBoxMap = Map.fromAscList $ zip (Set.toAscList varSet) (repeat (Nothing, Nothing))
    boxMap = findRepeat initBoxMap $ tail $ (iterate $ scanHypotheses form) initBoxMap
    scanHypotheses (Implies h c) =
        scanHypotheses c . scanHypothesis h 
    scanHypotheses _ = id
    scanHypothesis (And h1 h2) box = 
        (scanHypothesis h1 . scanHypothesis h2) box
    scanHypothesis (Le (Var v) t) box = 
        Map.insertWith updateUpper v (evalT box t) box
    scanHypothesis (Le t (Var v)) box = 
        Map.insertWith updateLower v (evalT box t) box
    scanHypothesis (Leq (Var v) t) box = 
        Map.insertWith updateUpper v (evalT box t) box
    scanHypothesis (Leq t (Var v)) box = 
        Map.insertWith updateLower v (evalT box t) box
    scanHypothesis (Ge (Var v) t) box = 
        Map.insertWith updateLower v (evalT box t) box
    scanHypothesis (Ge t (Var v)) box = 
        Map.insertWith updateUpper v (evalT box t) box
    scanHypothesis (Geq (Var v) t) box = 
        Map.insertWith updateLower v (evalT box t) box
    scanHypothesis (Geq t (Var v)) box = 
        Map.insertWith updateUpper v (evalT box t) box
    scanHypothesis _ box = box
    
    updateUpper (_,Just u2) (l, Just u1) = (l, Just $ min u1 u2)
    updateUpper (_,Just u2) (l, Nothing) = (l, Just $ u2)
    updateUpper (_,Nothing) (l, Just u1) = (l, Just $ u1)
    updateUpper (_,Nothing) (l, Nothing) = (l, Nothing)
    
    updateLower (Just l2,_) (Just l1,u) = (Just $ max l1 l2, u)
    updateLower (Just l2,_) (Nothing,u) = (Just $ l2, u)
    updateLower (Nothing,_) (Just l1,u) = (Just $ l1, u)
    updateLower (Nothing,_) (Nothing,u) = (Nothing, u)
    
    evalT _ (Lit val) = (Just val, Just val)
    evalT box (Var v) = 
        case Map.lookup v box of
            Nothing -> (Nothing, Nothing)
            Just v -> v
    evalT box (Neg a) =
        (fmap negate maR, fmap negate maL)
        where
        (maL, maR) = evalT box a
    evalT box (Over l r) 
        | or $ map (== Nothing) [mlL,mlR,mrL,mrR] = (Nothing, Nothing)
        | otherwise =
            case (lL > 0, lR < 0, rL > 0, rR < 0) of
                (True, _, True, _) -> -- both positive
                    (Just $ lL / rR, Just $ lR / rL)
                (_, True, _, True) -> -- both negative
                    (Just $ lR / rL, Just $ lL / rR)
                (True, _, _, True) -> -- positive, negative
                    (Just $ lR / rR, Just $ lL / rL)
                (_, True, True, _) -> -- negative, positive
                    (Just $ lL / rL, Just $ lR / rR)
                _ -> (Nothing, Nothing) -- ignore the difficult cases
        where
        (Just lL) = mlL
        (Just lR) = mlR
        (Just rL) = mrL
        (Just rR) = mrR
        (mlL, mlR) = evalT box l
        (mrL, mrR) = evalT box r
        
    evalT box (Sqrt a)
        | maL == Nothing = (Nothing, Nothing)
        | aL >= 0 = (Just $ sqrtR aL, fmap sqrtR maR)
        | otherwise = (Nothing, Nothing)
        where
        (maL, maR) = evalT box a
        (Just aL) = maL
    evalT _ _ = (Nothing, Nothing)
    
sqrtR :: Rational -> Rational
sqrtR =
    toRational . sqrt . fromRational
    
findRepeat :: (Eq a) => a -> [a] -> a
findRepeat prev (next:rest)
    | prev == next = prev
    | otherwise = findRepeat next rest
findRepeat prev [] = prev

    