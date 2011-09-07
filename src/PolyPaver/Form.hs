{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveDataTypeable #-}
module PolyPaver.Form where

import Data.Data

infixr 2 --->
infixl 3 \/
infixl 4 /\
infixl 5 |<|, |<=|, |>|, |>=|, |<-|
infixl 6 +:, -:
infixl 7 *:, /:

data Form
  = Verum
  | Falsum
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

(/\) = And
(\/) = Or
(--->) = Implies
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
  deriving (Eq,Show,Read,Data,Typeable) 


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
        | length oneLineForm <= 15 = oneLineForm
        | otherwise = sf2 maybeIndentLevel form
        where
        oneLineForm = sf2 Nothing form
    sf2 maybeIndentLevel form
        = 
        case form of
            Verum -> "T"
            Falsum -> "F"
            Not f -> "¬" ++ (indentedBrackets f)
            Or f1 f2 ->
                indentedBrackets f1 
                ++ indent ++ " ∨ " ++ indent ++
                indentedBrackets f2 
            And f1 f2 ->
                indentedBrackets f1 
                ++ indent ++ " ∧ " ++ indent ++
                indentedBrackets f2 
            Implies f1 f2 ->
                indentedBrackets f1 
                ++ indent ++ " ⇒ " ++ indent ++
                indentedBrackets f2 
            Le t1 t2 -> st maybeIndentLevel t1 ++ " < " ++ st maybeIndentLevel t2
            Leq t1 t2 -> st maybeIndentLevel t1 ++ " ≤ " ++ st maybeIndentLevel t2
            Ge t1 t2 -> st maybeIndentLevel t1 ++ " > " ++ st maybeIndentLevel t2
            Geq t1 t2 -> st maybeIndentLevel t1 ++ " ≥ " ++ st maybeIndentLevel t2
            Eq t1 t2 -> st maybeIndentLevel t1 ++ " = " ++ st maybeIndentLevel t2
            Neq t1 t2 -> st maybeIndentLevel t1 ++ " ≠ " ++ st maybeIndentLevel t2
            Ni t1 t2 -> st maybeIndentLevel t1 ++ " ⊆ " ++ st maybeIndentLevel t2
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
        indentedBrackets form
            | isAtomicForm form = " " ++ between ++ " "
            | otherwise = "(" ++ indentNext ++ between ++ indent ++ ")"
            where
            between = sfNext form
        maybeNextIndentLevel = fmap (+ 2) maybeIndentLevel
    st indentLevel term
        = 
        case term of
            EpsAbs -> "εabs"
            EpsRel -> "εrel"
            Pi -> "π"
            Lit r -> show r 
            Var n s -> s
            Plus t1 t2 -> "(" ++ st indentLevel t1 ++ ") + (" ++ st indentLevel t2 ++ ")"
            Minus t1 t2 -> "(" ++ st indentLevel t1 ++ ")- (" ++ st indentLevel t2 ++ ")"
            Neg t -> "-(" ++ st indentLevel t ++ ")"
            Abs t -> "|" ++ st indentLevel t ++ "|"
            Min t1 t2 -> "min(" ++ st indentLevel t1 ++ "," ++ st indentLevel t2 ++ ")"
            Max t1 t2 -> "max(" ++ st indentLevel t1 ++ "," ++ st indentLevel t2 ++ ")"
            Times t1 t2 -> "(" ++ st indentLevel t1 ++ ") * (" ++ st indentLevel t2 ++ ")"
            Square t -> "(" ++ st indentLevel t ++ ")^2"
            Recip t -> "1/(" ++ st indentLevel t ++ ")"
            Over t1 t2 -> "(" ++ st indentLevel t1 ++ ") / (" ++ st indentLevel t2 ++ ")"
            Sqrt t -> "sqrt(" ++ st indentLevel t ++ ")"
            Exp t -> "exp(" ++ st indentLevel t ++ ")"
            Sin t -> "sin(" ++ st indentLevel t ++ ")"
            Cos t -> "cos(" ++ st indentLevel t ++ ")"
            Atan t -> "atan(" ++ st indentLevel t ++ ")"
            Hull t1 t2 -> "(" ++ st indentLevel t1 ++ ")..(" ++ st indentLevel t2 ++ ")"
            EpsiAbs -> "εabsI"
            EpsiRel -> "εrelI"
            Round t -> "rnd(" ++ st indentLevel t ++ ")"
            FPlus t1 t2 -> "(" ++ st indentLevel t1 ++ ") ⊕ (" ++ st indentLevel t2 ++ ")"
            FMinus t1 t2 -> "(" ++ st indentLevel t1 ++ ") ⊖ (" ++ st indentLevel t2 ++ ")"
            FTimes t1 t2 -> "(" ++ st indentLevel t1 ++ ") ⊛ (" ++ st indentLevel t2 ++ ")"
            FOver t1 t2 -> "(" ++ st indentLevel t1 ++ ") ⊘ (" ++ st indentLevel t2 ++ ")"
            FSquare t -> "fsquare(" ++ st indentLevel t ++ ")"
            FSqrt t -> "fsqrt(" ++ st indentLevel t ++ ")"
            FExp t -> "fexp(" ++ st indentLevel t ++ ")"

