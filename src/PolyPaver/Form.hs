{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveDataTypeable #-}
module PolyPaver.Form where

import Data.Data
import Data.Ratio
import Data.List (intercalate)

infixr 2 --->
infixl 3 \/
infixl 4 /\
infixl 5 |<|, |<=|, |>|, |>=|, |<-|, |=|, |==|
infixl 6 +:, -:
infixl 7 *:, /:

data Form
  = Verum
  | Falsum
  | Predicate Term -- Boolean-valued term
  | Not Form
  | Or Form Form
  | And Form Form
  | Implies Form Form
  | Le Term Term
  | Leq Term Term
  | Ge Term Term
  | Geq Term Term
  | Eq Term Term
  | Neq Term Term
  | Ni Term Term
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

(/\) = And
(\/) = Or
(--->) = Implies
(|=|) = Eq
(|==|) = Eq
(|<|) = Le
(|<=|) = Leq
(|>|) = Ge
(|>=|) = Geq
(|<-|) = Ni

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
showTerm :: Term -> String
(showForm, showTerm) =
    (sf (Just 0), st (Just 0)) 
    where
    sf Nothing form = sf2 Nothing form
    sf maybeIndentLevel form
        | length oneLineForm <= 60 = oneLineForm
        | otherwise = sf2 maybeIndentLevel form
        where
        oneLineForm = sf2 Nothing form
    st Nothing form = st2 Nothing form
    st maybeIndentLevel term
        | length oneLineTerm<= 60 = oneLineTerm
        | otherwise = st2 maybeIndentLevel term
        where
        oneLineTerm = st2 Nothing term
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
            Le t1 t2 -> showOpT "<" t1 t2
            Leq t1 t2 -> showOpT "≤" t1 t2
            Ge t1 t2 -> showOpT ">" t1 t2
            Geq t1 t2 -> showOpT "≥" t1 t2
            Eq t1 t2 -> showOpT "=" t1 t2
            Neq t1 t2 -> showOpT "≠" t1 t2
            Ni t1 t2 -> showOpT "⊆" t1 t2
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
        showOpT op t1 t2 =
            st maybeIndentLevel t1 
            ++ indent ++ padIfInline op ++ indent ++
            st maybeIndentLevel t2
        padIfInline op = case maybeIndentLevel of Nothing -> " " ++ op ++ " "; _ -> op 
        indentedBracketsF form
            | isAtomicForm form = sf maybeIndentLevel form
            | otherwise = "(" ++ indentNext ++ sfNext form ++ indent ++ ")"
        maybeNextIndentLevel = fmap (+ 2) maybeIndentLevel
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

