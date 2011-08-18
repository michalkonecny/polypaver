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

getBox :: Form -> [(Int, (Rational, Rational))]
getBox form =
    Map.toAscList boxMap
    where
    varSet = getFormFreeVars form
    initBoxMap = Map.fromAscList $ zip (Set.toAscList varSet) (repeat floatRange)
    floatRange = (- 340282000000000000000000000000000000000, 340282000000000000000000000000000000000)
    boxMap = findRepeat initBoxMap $ tail $ (iterate $ scanHypotheses form) initBoxMap
    scanHypotheses (Implies h c) =
        scanHypotheses c . scanHypothesis h 
    scanHypotheses _ = id
    scanHypothesis (And h1 h2) box = 
        (scanHypothesis h1 . scanHypothesis h2) box
    scanHypothesis (Le (Var v) t) box = 
        case evalT box t of
            Just val -> Map.insertWith updateUpper v val box
            Nothing -> box
    scanHypothesis (Le t (Var v)) box = 
        case evalT box t of
            Just val -> Map.insertWith updateLower v val box
            Nothing -> box
    scanHypothesis (Leq (Var v) t) box = 
        case evalT box t of
            Just val -> Map.insertWith updateUpper v val box
            Nothing -> box
    scanHypothesis (Leq t (Var v)) box = 
        case evalT box t of
            Just val -> Map.insertWith updateLower v val box
            Nothing -> box
    scanHypothesis (Ge (Var v) t) box = 
        case evalT box t of
            Just val -> Map.insertWith updateLower v val box
            Nothing -> box
    scanHypothesis (Ge t (Var v)) box = 
        case evalT box t of
            Just val -> Map.insertWith updateUpper v val box
            Nothing -> box
    scanHypothesis (Geq (Var v) t) box = 
        case evalT box t of
            Just val -> Map.insertWith updateLower v val box
            Nothing -> box
    scanHypothesis (Geq t (Var v)) box = 
        case evalT box t of
            Just val -> Map.insertWith updateUpper v val box
            Nothing -> box
    scanHypothesis _ box = box
    updateUpper (_,u2) (l,u1) = (l, min u1 u2)
    updateLower (l2,_) (l1,u) = (max l1 l2, u)
    evalT _ (Lit val) = Just (val, val)
    evalT box (Var v) = Map.lookup v box
    evalT box (Neg a) =
        do
        (aL, aR) <- evalT box a
        return (- aR, -aL)
    evalT box (Over l r) = 
        do
        (lL, lR) <- evalT box l
        (rL, rR) <- evalT box r
        case (lL > 0, lR < 0, rL > 0, rR < 0) of
            (True, _, True, _) -> -- both positive
                return (lL / rR, lR / rL)
            (_, True, _, True) -> -- both negative
                return (lR / rL, lL / rR)
            (True, _, _, True) -> -- positive, negative
                return (lR / rR, lL / rL)
            (_, True, True, _) -> -- negative, positive
                return (lL / rL, lR / rR)
            _ -> Nothing -- ignore the difficult cases
    evalT box (Sqrt a) =
        do
        (aL, aR) <- evalT box a
        case (aL >= 0) of
            True -> return (sqrtR aL, sqrtR aR)
            _ -> Nothing
    evalT _ _ = Nothing
    
sqrtR :: Rational -> Rational
sqrtR =
    toRational . sqrt . fromRational
    
findRepeat :: (Eq a) => a -> [a] -> a
findRepeat prev (next:rest)
    | prev == next = prev
    | otherwise = findRepeat next rest
findRepeat prev [] = prev

    