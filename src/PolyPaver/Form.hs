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

showForm :: Form -> String
showForm form
    = case form of
          Verum -> "T"
          Falsum -> "F"
          Not f -> "¬(" ++ showForm f ++ ")" 
          Or f1 f2 -> "(" ++ showForm f1 ++ ") ∨ (" ++ showForm f2 ++ ")"
          And f1 f2 -> "(" ++ showForm f1 ++ ") ∧ (" ++ showForm f2 ++ ")"
          Implies f1 f2 -> "(" ++ showForm f1 ++ ") ⇒ (" ++ showForm f2 ++ ")"
          Le t1 t2 -> showTerm t1 ++ " < " ++ showTerm t2
          Leq t1 t2 -> showTerm t1 ++ " ≤ " ++ showTerm t2
          Ge t1 t2 -> showTerm t1 ++ " > " ++ showTerm t2
          Geq t1 t2 -> showTerm t1 ++ " ≥ " ++ showTerm t2
          Eq t1 t2 -> showTerm t1 ++ " = " ++ showTerm t2
          Neq t1 t2 -> showTerm t1 ++ " ≠ " ++ showTerm t2
          Ni t1 t2 -> showTerm t1 ++ " ⊆ " ++ showTerm t2


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
--  | Min Term Term
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

showTerm :: Term -> String
showTerm term
    = case term of
          EpsAbs -> "εabs"
          EpsRel -> "εrel"
          Pi -> "π"
          Lit r -> show r 
          Var n s -> s
          Plus t1 t2 -> "(" ++ showTerm t1 ++ ") + (" ++ showTerm t2 ++ ")"
          Minus t1 t2 -> "(" ++ showTerm t1 ++ ")- (" ++ showTerm t2 ++ ")"
          Neg t -> "-(" ++ showTerm t ++ ")"
          Abs t -> "|" ++ showTerm t ++ "|"
        --  Min t1 t2
          Max t1 t2 -> "max(" ++ showTerm t1 ++ "," ++ showTerm t2 ++ ")"
          Times t1 t2 -> "(" ++ showTerm t1 ++ ") * (" ++ showTerm t2 ++ ")"
          Square t -> "(" ++ showTerm t ++ ")^2"
          Recip t -> "1/(" ++ showTerm t ++ ")"
          Over t1 t2 -> "(" ++ showTerm t1 ++ ") / (" ++ showTerm t2 ++ ")"
          Sqrt t -> "sqrt(" ++ showTerm t ++ ")"
          Exp t -> "exp(" ++ showTerm t ++ ")"
          Sin t -> "sin(" ++ showTerm t ++ ")"
          Cos t -> "cos(" ++ showTerm t ++ ")"
          Atan t -> "atan(" ++ showTerm t ++ ")"
          Hull t1 t2 -> "(" ++ showTerm t1 ++ ")..(" ++ showTerm t2 ++ ")"
          EpsiAbs -> "εabsI"
          EpsiRel -> "εrelI"
          Round t -> "rnd(" ++ showTerm t ++ ")"
          FPlus t1 t2 -> "(" ++ showTerm t1 ++ ") ⊕ (" ++ showTerm t2 ++ ")"
          FMinus t1 t2 -> "(" ++ showTerm t1 ++ ") ⊖ (" ++ showTerm t2 ++ ")"
          FTimes t1 t2 -> "(" ++ showTerm t1 ++ ") ⊛ (" ++ showTerm t2 ++ ")"
          FOver t1 t2 -> "(" ++ showTerm t1 ++ ") ⊘ (" ++ showTerm t2 ++ ")"
          FSquare t -> "fsquare(" ++ showTerm t ++ ")"
          FSqrt t -> "fsqrt(" ++ showTerm t ++ ")"
          FExp t -> "fexp(" ++ showTerm t ++ ")"

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
