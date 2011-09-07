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

import Numeric.ER.Misc

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IMap

getBox :: Form -> Either String [(Int, (Rational, Rational))]
getBox form =
    unsafePrint
    (
        "PolyPaver.Vars: getBox:"
        ++ "\n 4 scans:"
        ++ "\n" ++ (unlines $ map show $ take 4 boxMapSeq)
    ) $
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
            "*** failed to derive a bound for variable " ++ showVar varNames v ++ " in formula " ++ showForm form 
    varSet = getFormFreeVars form
    varNames = getFormVarNames form
    initBoxMap = Map.fromAscList $ zip (Set.toAscList varSet) (repeat (Nothing, Nothing))
    boxMap = findRepeat initBoxMap $ tail $ boxMapSeq 
    boxMapSeq = iterate (scanHypotheses form) initBoxMap
    
    scanHypotheses (Implies h c) =
        scanHypotheses c . scanHypothesis h 
    scanHypotheses _ = id
    
    scanHypothesis (And h1 h2) box = 
        (scanHypothesis h1 . scanHypothesis h2) box
    scanHypothesis (Or h1 h2) box = 
        Map.unionWith mergeWorse box1 box2
        where
        box1 = scanHypothesis h1 box 
        box2 = scanHypothesis h2 box
        mergeWorse (l1,r1) (l2,r2) = (minM l1 l2, maxM r1 r2)
        minM (Just a) (Just b) = Just $ min a b
        minM _ _ = Nothing
        maxM (Just a) (Just b) = Just $ max a b
        maxM _ _ = Nothing
    scanHypothesis (Eq (Var v _) t) box = 
        Map.insertWith updateUpper v val $
        Map.insertWith updateLower v val box
        where
        val = evalT box t
    scanHypothesis (Eq t (Var v _)) box = 
        Map.insertWith updateUpper v val $
        Map.insertWith updateLower v val box
        where
        val = evalT box t
    scanHypothesis (Le (Var v _) t) box = 
        Map.insertWith updateUpper v (evalT box t) box
    scanHypothesis (Le t (Var v _)) box = 
        Map.insertWith updateLower v (evalT box t) box
    scanHypothesis (Leq (Var v _) t) box = 
        Map.insertWith updateUpper v (evalT box t) box
    scanHypothesis (Leq t (Var v _)) box = 
        Map.insertWith updateLower v (evalT box t) box
    scanHypothesis (Ge (Var v _) t) box = 
        Map.insertWith updateLower v (evalT box t) box
    scanHypothesis (Ge t (Var v _)) box = 
        Map.insertWith updateUpper v (evalT box t) box
    scanHypothesis (Geq (Var v _) t) box = 
        Map.insertWith updateLower v (evalT box t) box
    scanHypothesis (Geq t (Var v _)) box = 
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
    evalT box (Var v _) = 
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

    