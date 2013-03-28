{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PolyPaver.Form where

import Data.Data
import Data.Ratio
import Data.List (intercalate, sortBy)

infixr 2 --->
infixl 3 \/
infixl 4 /\
infixl 5 |<|, |<=|, |>|, |>=|, |<-|, |=|, |==|
infixl 6 +:, -:
infixl 7 *:, /:

type Label = String

data Form
  = Verum
  | Falsum
  | Predicate Term -- Boolean-valued term
  | Not Form
  | Or Form Form
  | And Form Form
  | Implies Form Form
  | Le Label Term Term
  | Leq Label Term Term
  | Ge Label Term Term
  | Geq Label Term Term
  | Eq Label Term Term
  | Neq Label Term Term
  | Ni Label Term Term
  deriving (Eq,Show,Read,Data,Typeable)

isAtomicForm :: Form -> Bool
isAtomicForm (Not f) = False
isAtomicForm (Or _ _) = False
isAtomicForm (And _ _) = False
isAtomicForm (Implies _ _) = False
isAtomicForm _ = True

getConclusion :: Form -> Form
getConclusion (Implies f1 f2) = getConclusion f2
getConclusion f = f

splitConclusion :: Form -> [Form]
splitConclusion (Implies f1 f2) =
    map (Implies f1) $ splitConclusion f2
splitConclusion f = splitFormula f

splitFormula :: Form -> [Form]
splitFormula (And f1 f2) =
    splitFormula f1 ++ splitFormula f2
splitFormula f = [f] 

getFormulaSize :: Form -> Int
getFormulaSize form =
    case form of
        Verum -> 1
        Falsum -> 1
        Predicate t -> 1 + (getTermSize t)
        Not f -> 1 + (getFormulaSize f)
        Or f1 f2 -> 1 + (getFormulaSize f1) + (getFormulaSize f2)
        And f1 f2 -> 1 + (getFormulaSize f1) + (getFormulaSize f2)
        Implies f1 f2 -> 1 + (getFormulaSize f1) + (getFormulaSize f2)
        Le _ t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Leq _ t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Ge _ t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Geq _ t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Eq _ t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Neq _ t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Ni _ t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)

sortFormulasBySize :: [Form] -> [Form]
sortFormulasBySize formulas =
    map snd $ sortBy (\a b -> compare (fst a) (fst b)) $ map addSize formulas
    where
    addSize formula = (getFormulaSize formula, formula)

(/\) = And
(\/) = Or
(--->) = Implies
(|=|) = Eq "anon"
(|==|) = Eq "anon"
(|<|) = Le "anon"
(|<=|) = Leq "anon"
(|>|) = Ge "anon"
(|>=|) = Geq "anon"
(|<-|) = Ni "anon"

notVerum Verum = False
notVerum _ = True

data Term
  = EpsAbs
  | EpsRel
  | Pi
  | Lit Rational
  | Var Int String -- numeric identifier, string only for printing
  | Plus Term Term
  | Minus Term Term
  | Neg Term
  | Abs Term
  | Min Term Term
  | Max Term Term
  | Times Term Term
  | Square Term
  | Recip Term
  | Over Term Term
  | Sqrt Term
  | Exp Term
  | Sin Term
  | Cos Term
  | Atan Term
  | Hull Term Term
  | Integral Term Term Int String Term -- eg: Integral lower upper ivarId ivarName integrand
  | EpsiAbs
  | EpsiRel
  | Round Term
  | FPlus Term Term
  | FMinus Term Term
  | FTimes Term Term
  | FOver Term Term
  | FSquare Term
  | FSqrt Term
  | FExp Term
  | IsInt Term
  deriving (Eq,Show,Read,Data,Typeable) 

isAtomicTerm :: Term -> Bool
isAtomicTerm EpsAbs = True
isAtomicTerm EpsRel = True
isAtomicTerm (Neg t) = isAtomicTerm t
isAtomicTerm Pi = True
isAtomicTerm (Lit _) = True
isAtomicTerm (Var _ _) = True
isAtomicTerm _ = False

getTermSize :: Term -> Int
getTermSize term =
    case term of
        EpsAbs -> 1
        EpsRel -> 1
        Pi -> 1
        Lit r -> 1 
        Var n s -> 1
        Plus t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Minus t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Neg t -> 1 + (getTermSize t)
        Abs t -> 1 + (getTermSize t)
        Min t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Max t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Times t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Square t -> 1 + (getTermSize t)
        Recip t -> 1 + (getTermSize t)
        Over t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Sqrt t -> 10 + (getTermSize t)
        Exp t -> 10 + (getTermSize t)
        Sin t -> 10 + (getTermSize t)
        Cos t -> 10 + (getTermSize t)
        Atan t -> 10 + (getTermSize t)
        Hull t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        IsInt t -> 1
        Integral lower upper ivarId ivarName integrand -> 
            2 + (getTermSize lower) + (getTermSize upper) + (getTermSize integrand)
        EpsiAbs -> 1
        EpsiRel -> 1
        Round t -> 1 + (getTermSize t)
        FPlus t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        FMinus t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        FTimes t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        FOver t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        FSquare t -> 1 + (getTermSize t)
        FSqrt t -> 1 + (getTermSize t)
        FExp t -> 1 + (getTermSize t)

instance Num Term
  where
  fromInteger = Lit . fromInteger
  negate = Neg
  (+) = Plus
  (*) = Times

instance Fractional Term
  where
  fromRational = Lit
  recip = Recip
  (/) = Over

instance Floating Term
  where
  sqrt = Sqrt
  exp = Exp
  cos = Cos
  sin = Sin
  atan = Atan

(+:) = FPlus
(-:) = FMinus
(*:) = FTimes
(/:) = FOver

plusMinus a = Hull (-a) a

showForm :: Form -> String
showForm form = sf (Just 0) form
    where
    st maybeIndentLevel = showTermIL maybeIndentLevel
    sf Nothing form = sf2 Nothing form
    sf maybeIndentLevel form
        | length oneLineForm <= 60 = oneLineForm
        | otherwise = sf2 maybeIndentLevel form
        where
        oneLineForm = sf2 Nothing form
    sf2 maybeIndentLevel form
        = 
        case form of
            Verum -> "T"
            Falsum -> "F"
            Predicate t -> st maybeIndentLevel t
            Not f -> "¬" ++ (indentedBracketsF f)
            Or f1 f2 -> showOpF "∨" f1 f2
            And f1 f2 -> showOpF "∧" f1 f2
            Implies f1 f2 -> showOpF "⇒" f1 f2
            Le lab t1 t2 -> showOpT "<" lab t1 t2
            Leq lab t1 t2 -> showOpT "≤" lab t1 t2
            Ge lab t1 t2 -> showOpT ">" lab t1 t2
            Geq lab t1 t2 -> showOpT "≥" lab t1 t2
            Eq lab t1 t2 -> showOpT "=" lab t1 t2
            Neq lab t1 t2 -> showOpT "≠" lab t1 t2
            Ni lab t1 t2 -> showOpT "⊆" lab t1 t2
        where
        sfNext = sf maybeNextIndentLevel
        stNext = st maybeNextIndentLevel
        indent = 
            case maybeIndentLevel of 
                Just indentLevel -> "\n" ++ replicate indentLevel ' '
                _ -> ""
        indentNext = 
            case maybeNextIndentLevel of
                Just indentLevel -> "\n" ++ replicate indentLevel ' '
                _ -> ""
        showOpF op f1 f2 =
            indentedBracketsF f1 
            ++ indent ++ padIfInline op ++ indent ++
            indentedBracketsF f2
        showOpT op lab t1 t2 =
            labelIfInline lab
            ++ st maybeIndentLevel t1 
            ++ indent ++ padIfInline op ++ labelIfSeparateLine lab 
            ++ indent ++ st maybeIndentLevel t2
        padIfInline op = 
            case maybeIndentLevel of 
                Nothing -> " " ++ op ++ " "
                _ -> op 
        labelIfInline lab =
            case maybeIndentLevel of
                Nothing -> "[" ++ show lab ++ "] "
                _ -> ""
        labelIfSeparateLine lab =
            case maybeIndentLevel of
                Nothing -> ""
                _ -> " [" ++ show lab ++ "] "
        indentedBracketsF form
            | isAtomicForm form = sf maybeIndentLevel form
            | otherwise = "(" ++ indentNext ++ sfNext form ++ indent ++ ")"
        maybeNextIndentLevel = fmap (+ 2) maybeIndentLevel

showTerm :: Term -> String
showTerm term = showTermIL (Just 0) term

showTermIL :: (Maybe Int) -> Term -> String
showTermIL = st
    where
    st Nothing term = st2 Nothing term
    st maybeIndentLevel term
        | length oneLineTerm<= 60 = oneLineTerm
        | otherwise = st2 maybeIndentLevel term
        where
        oneLineTerm = st2 Nothing term

    st2 maybeIndentLevel term
        = 
        case term of
            EpsAbs -> "εabs"
            EpsRel -> "εrel"
            Pi -> "π"
            Lit r -> 
                if floor r == ceiling r 
                    then show (floor r) 
                    else show (numerator r) ++ "/" ++ show (denominator r)
            Var n s -> s
            Plus t1 t2 -> showOpT "+" t1 t2
            Minus t1 t2 -> showOpT "-" t1 t2
            Neg t -> showFnT "-" [t]
            Abs t -> indentedOpenCloseT "|" "|" False t
            Min t1 t2 -> showFnT "min" [t1, t2]
            Max t1 t2 -> showFnT "max" [t1, t2]
            Times t1 t2 -> showOpT "*" t1 t2
            Square t -> indentedOpenCloseT "(" ")^2" False t
            Recip t -> st2 maybeIndentLevel $ Over (Lit 1) t
            Over t1 t2 -> showOpT "/" t1 t2
            Sqrt t -> showFnT "sqrt" [t]
            Exp t -> showFnT "exp" [t]
            Sin t -> showFnT "sin" [t]
            Cos t -> showFnT "cos" [t]
            Atan t -> showFnT "atan" [t]
            Hull t1 t2 -> showOpT ".." t1 t2
            IsInt t -> showFnT "isint" [t]
            Integral lower upper ivarId ivarName integrand -> 
                showFnT "∫" [lower, upper, Var ivarId ivarName, integrand]
            EpsiAbs -> "εabsI"
            EpsiRel -> "εrelI"
            Round t -> showFnT "rnd" [t]
            FPlus t1 t2 -> showOpT "⊕" t1 t2
            FMinus t1 t2 -> showOpT "⊖" t1 t2
            FTimes t1 t2 -> showOpT "⊛" t1 t2
            FOver t1 t2 -> showOpT "⊘" t1 t2
            FSquare t -> showFnT "fsquare" [t]
            FSqrt t -> showFnT "fsqrt" [t]
            FExp t -> showFnT "fexp" [t]
        where
        stNext = st maybeNextIndentLevel
        indent = 
            case maybeIndentLevel of 
                Just indentLevel -> "\n" ++ replicate indentLevel ' '
                _ -> ""
        indentNext = 
            case maybeNextIndentLevel of
                Just indentLevel -> "\n" ++ replicate indentLevel ' '
                _ -> ""
        showOpT op t1 t2 =
            indentedBracketsT t1 
            ++ indent ++ padIfInline op ++ indent ++
            indentedBracketsT t2
        showFnT fn ts =
            fn ++ "("
            ++ (intercalate (indent ++ ", ") $ map (\t -> indentNext ++ stNext t) ts)
            ++ indent ++ ")"
        padIfInline op = case maybeIndentLevel of Nothing -> " " ++ op ++ " "; _ -> op 
        indentedBracketsT = indentedOpenCloseT "(" ")" True
        indentedOpenCloseT open close optional term
            | optional && isAtomicTerm term = st maybeIndentLevel term
            | otherwise = open ++ indentNext ++ stNext term ++ indent ++ close
        maybeNextIndentLevel = fmap (+ 2) maybeIndentLevel

