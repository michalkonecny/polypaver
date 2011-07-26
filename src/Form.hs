{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Form where

data Form
  = Not Form
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
  | Times Term Term
  | Square Term
  | Recip Term
  | Over Term Term
  | Sqrt Term
  | Exp Term
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
  t1 + t2 = Plus t1 t2
  t1 * t2 = Times t1 t2

instance Fractional Term
  where
  fromRational = Lit
  recip = Recip
