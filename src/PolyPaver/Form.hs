{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PolyPaver.Form where

import Numeric.ER.Real.DefaultRepr

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
  | ContainedIn Label Term Term
  | IsRange Label Term Term Term 
  | IsIntRange Label Term Term Term 
  | IsInt Label Term
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
        ContainedIn _ t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        IsRange _ t1 t2 t3 -> 1 + (getTermSize t1) + (getTermSize t2) + (getTermSize t3)
        IsIntRange _ t1 t2 t3 -> 1 + (getTermSize t1) + (getTermSize t2) + (getTermSize t3)
        IsInt _ t -> 1 + (getTermSize t)

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
(|<-|) = ContainedIn "anon"

notVerum Verum = False
notVerum _ = True

newtype Term = Term (Term', Maybe (IRA BM))
  deriving (Eq,Show,Data,Typeable)
data Term'
  = Lit Rational
  | Var Int String -- numeric identifier, string only for printing
  | Hull Term Term -- interval covering both values
  | Plus Term Term
  | Minus Term Term
  | Neg Term
  | Times Term Term
  | Square Term
  | Recip Term
  | Over Term Term
  | Abs Term
  | Min Term Term
  | Max Term Term
  | Pi
  | Sqrt Term
  | Exp Term
  | Sin Term
  | Cos Term
  | Atan Term
  | Integral Int String Term Term Term -- eg: Integral ivarId ivarName lower upper integrand
-- the following term constructors depend on a specific floating-point arithmetic: 
  | FEpsAbs
  | FEpsRel
  | FEpsiAbs
  | FEpsiRel
  | FRound Term
  | FPlus Term Term
  | FMinus Term Term
  | FTimes Term Term
  | FOver Term Term
  | FSquare Term
  | FSqrt Term
  | FExp Term
  deriving (Eq,Show,Read,Data,Typeable)

instance Read Term where
    readsPrec p s =
        map addTN $ readsPrec p s
        where
        addTN (t, cont) =
            (Term (t, Nothing), cont) 

getTermSize :: Term -> Int
getTermSize (Term (term, _)) =
    case term of
        Lit r -> 1 
        Var n s -> 1
        Hull t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Plus t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Minus t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Neg t -> 1 + (getTermSize t)
        Times t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Square t -> 1 + (getTermSize t)
        Recip t -> 1 + (getTermSize t)
        Over t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Abs t -> 1 + (getTermSize t)
        Min t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Max t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Pi -> 1
        Sqrt t -> 10 + (getTermSize t)
        Exp t -> 10 + (getTermSize t)
        Sin t -> 10 + (getTermSize t)
        Cos t -> 10 + (getTermSize t)
        Atan t -> 10 + (getTermSize t)
        Integral ivarId ivarName lower upper integrand -> 
            2 + (getTermSize lower) + (getTermSize upper) + (getTermSize integrand)
        FEpsAbs -> 1
        FEpsRel -> 1
        FEpsiAbs -> 1
        FEpsiRel -> 1
        FRound t -> 1 + (getTermSize t)
        FPlus t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        FMinus t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        FTimes t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        FOver t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        FSquare t -> 1 + (getTermSize t)
        FSqrt t -> 1 + (getTermSize t)
        FExp t -> 1 + (getTermSize t)


{--- Operations for convenient encoding of literal values of type Term  ---}

termOp0 op = Term (op, Nothing)
termOp1 op t1 = Term (op t1, Nothing)
termOp2 op t1 t2 = Term (op t1 t2, Nothing)
termOp3 op t1 t2 t3 = Term (op t1 t2 t3, Nothing)

termVar id name = termOp0 $ Var id name

hull = termOp2 Hull
plusMinus a = hull (negate a) a

instance Ord Term
  where
  min = termOp2 Min
  max = termOp2 Max
  compare = error "compare not implemented for datatype Term"
  
instance Num Term
  where
  fromInteger n = termOp1 Lit $ fromInteger n
  negate = termOp1 Neg
  (+) = termOp2 Plus
  (*) = termOp2 Times
  abs = termOp1 Abs

square = termOp1 Square

instance Fractional Term
  where
  fromRational = termOp1 Lit
  recip = termOp1 Recip
  (/) = termOp2 Over

instance Floating Term
  where
  sqrt = termOp1 Sqrt
  exp = termOp1 Exp
  cos = termOp1 Cos
  sin = termOp1 Sin
  atan = termOp1 Atan

integral ivarId ivarName = termOp3 $ Integral ivarId ivarName

fepsAbs = termOp0 FEpsAbs
fepsRel = termOp0 FEpsRel
fepsiAbs = termOp0 FEpsiAbs
fepsiRel = termOp0 FEpsiRel

fround = termOp1 FRound

(+:) = termOp2 FPlus
(-:) = termOp2 FMinus
(*:) = termOp2 FTimes
(/:) = termOp2 FOver

showForm :: Bool -> Form -> String
showForm shouldShowRanges form = sf (Just 0) form
    where
    st maybeIndentLevel = showTermIL shouldShowRanges maybeIndentLevel
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
            ContainedIn lab t1 t2 -> showOpT "⊆" lab t1 t2
            IsRange lab t1 t2 t3 -> showPred lab "isrange" [t1, t2, t3]
            IsIntRange lab t1 t2 t3 -> showPred lab "isintrange" [t1, t2, t3]
            IsInt lab t -> showPred lab "isint" [t]
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
        showPred lab pred ts =
            labelIfInline lab
            ++ pred ++ "("
            ++ (intercalate (indent ++ ", ") $ map (\t -> indentNext ++ stNext t) ts)
            ++ indent ++ ")"
        showOpT op lab t1 t2 =
            "[" ++ show lab ++ "] "
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

showTerm :: Bool -> Term -> String
showTerm shouldShowRanges term = showTermIL shouldShowRanges (Just 0) term

showTermIL :: Bool -> (Maybe Int) -> Term -> String
showTermIL shouldShowRanges = st
    where
    st Nothing term = st2 Nothing term
    st maybeIndentLevel term
        | length oneLineTerm<= 60 = oneLineTerm
        | otherwise = st2 maybeIndentLevel term
        where
        oneLineTerm = st2 Nothing term

    st2 maybeIndentLevel (Term (term,maybeRangeBounds))
        =
        case term of
            Lit r -> 
                if floor r == ceiling r 
                    then maybeAddBounds $ show (floor r) 
                    else maybeAddBounds $ show (numerator r) ++ "/" ++ show (denominator r)
            Var n s -> maybeAddBounds s
            Hull t1 t2 -> showOpT ".." "hull" t1 t2
            Plus t1 t2 -> showOpT "+" "sum" t1 t2
            Minus t1 t2 -> showOpT "-" "diff" t1 t2
            Neg t -> showFnT "-" [t]
            Times t1 t2 -> showOpT "*" "prod" t1 t2
            Square t -> indentedOpenCloseT "(" ")^2" False t
            Recip t -> st2 maybeIndentLevel $ Term (Over 1 t, maybeRangeBounds)
            Over t1 t2 -> showOpT "/" "div" t1 t2
            Abs t -> indentedOpenCloseT "|" "|" False t
            Min t1 t2 -> showFnT "min" [t1, t2]
            Max t1 t2 -> showFnT "max" [t1, t2]
            Pi -> maybeAddBounds "π"
            Sqrt t -> showFnT "sqrt" [t]
            Exp t -> showFnT "exp" [t]
            Sin t -> showFnT "sin" [t]
            Cos t -> showFnT "cos" [t]
            Atan t -> showFnT "atan" [t]
            Integral ivarId ivarName lower upper integrand -> 
                showFnT "∫" [lower, upper, Term (Var ivarId ivarName, Nothing), integrand]
            FEpsAbs -> maybeAddBounds "εabs"
            FEpsRel -> maybeAddBounds "εrel"
            FEpsiAbs -> maybeAddBounds "εabsI"
            FEpsiRel -> maybeAddBounds "εrelI"
            FRound t -> showFnT "rnd" [t]
            FPlus t1 t2 -> showOpT "⊕" "sum" t1 t2
            FMinus t1 t2 -> showOpT "⊖" "diff" t1 t2
            FTimes t1 t2 -> showOpT "⊛" "prod" t1 t2
            FOver t1 t2 -> showOpT "⊘" "div" t1 t2
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
        maybeAddBounds s =
            case (shouldShowRanges, maybeRangeBounds) of
                (True, Just rangeBounds) ->
                    s ++ "∊" ++ show rangeBounds
                _ -> s
        showOpT op opname t1 t2 =
            case maybeNextIndentLevel of
                Just _ ->
                    indent ++ rangeIfIndented
                    ++ indent ++ indentedBracketsT t1
                    ++ indent ++ op
                    ++ indent ++ indentedBracketsT t2
                _ -> 
                    rangeIfInlineOpen
                    ++ indentedBracketsT t1 
                    ++ padIfInline op
                    ++ indentedBracketsT t2
                    ++ rangeIfInlineClose
            where
            rangeIfInlineOpen = 
                case (shouldShowRanges, maybeRangeBounds) of
                    (True, Just _) -> "("
                    _ -> ""
            rangeIfInlineClose = 
                case (shouldShowRanges, maybeRangeBounds) of
                    (True, Just rangeBounds) -> ")∊" ++ show rangeBounds
                    _ -> ""
            rangeIfIndented =
                case (shouldShowRanges, maybeRangeBounds) of
                    (True, Just rangeBounds) -> "{" ++ opname ++ "∊" ++ show rangeBounds ++ "}"
                    _ -> ""
        showFnT fn ts =
            fn ++ rangeIfIndented ++ "("
            ++ (intercalate (indent ++ ", ") $ map (\t -> indentNext ++ stNext t) ts)
            ++ indent ++ ")"
            ++ range
            where
            rangeIfIndented =
                case (maybeNextIndentLevel, shouldShowRanges, maybeRangeBounds) of
                    (Just _, True, Just rangeBounds) ->
                        "{res∊" ++ show rangeBounds ++ "}"
                    _ -> ""
            range =
                case (shouldShowRanges, maybeRangeBounds) of
                    (True, Just rangeBounds) -> "∊" ++ show rangeBounds
                    _ -> ""
        padIfInline op = case maybeIndentLevel of Nothing -> " " ++ op ++ " "; _ -> op 
        indentedBracketsT = indentedOpenCloseT "(" ")" True
        indentedOpenCloseT open close optional term
            | optional && isAtomicTerm term = st maybeIndentLevel term
            | otherwise = open ++ indentNext ++ stNext term ++ indent ++ close
        maybeNextIndentLevel = fmap (+ 2) maybeIndentLevel
        isAtomicTerm :: Term -> Bool
        isAtomicTerm (Term (term, _)) = 
            case term of
                (Lit _) -> True
                (Var _ _) -> True
                (Neg t) -> isAtomicTerm t
                Pi -> True
                FEpsAbs -> True
                FEpsRel -> True
                FEpsiAbs -> True
                FEpsiRel -> True
                _ -> False


