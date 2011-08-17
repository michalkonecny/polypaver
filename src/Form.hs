{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Form where

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
  deriving (Eq,Show,Read)

(/\) = And
(\/) = Or
(--->) = Implies
(|<|) = Le
(|<=|) = Leq
(|>|) = Ge
(|>=|) = Geq
(|<-|) = Ni

data Term
  = EpsAbs
  | EpsRel
  | Pi
  | Lit Rational
  | Var Int
  | Plus Term Term
  | Minus Term Term
  | Neg Term
  | Abs Term
--  | Min Term Term
--  | Max Term Term
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
  deriving (Eq,Show,Read) 

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
