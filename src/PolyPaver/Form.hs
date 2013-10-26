{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PolyPaver.Form where

import System.Info (os)

import Data.Data (Data, Typeable)
import Data.Ratio (denominator, numerator)
import Data.List (intercalate, sortBy)

infixr 2 --->
infixl 3 \/
infixl 4 /\
infixl 5 |<|, |<=|, |>|, |>=|, |<-|, |=|, |==|
infixl 6 +:, -:
infixl 7 *:, /:

type FormLabel = String

data Form l
  = Not (Form l)
  | Or (Form l) (Form l)
  | And (Form l) (Form l)
  | Implies (Form l) (Form l)
  | Le FormLabel (Term l) (Term l)
  | Leq FormLabel (Term l) (Term l)
  | Ge FormLabel (Term l) (Term l)
  | Geq FormLabel (Term l) (Term l)
  | Eq FormLabel (Term l) (Term l)
  | Neq FormLabel (Term l) (Term l)
  | ContainedIn FormLabel (Term l) (Term l)
  | IsRange FormLabel (Term l) (Term l) (Term l)
  | IsIntRange FormLabel (Term l) (Term l) (Term l)
  | IsInt FormLabel (Term l)
  deriving (Eq,Show,Read,Data,Typeable)

verum :: (HasDefaultValue l) => Form l
verum = IsInt "Verum" (Term (Lit 0, defaultValue))

isAtomicForm :: Form l -> Bool
isAtomicForm (Not _f) = False
isAtomicForm (Or _ _) = False
isAtomicForm (And _ _) = False
isAtomicForm (Implies _ _) = False
isAtomicForm _ = True

getConclusion :: Form l -> Form l
getConclusion (Implies _f1 f2) = getConclusion f2
getConclusion f = f

splitConclusion :: Form l -> [Form l]
splitConclusion (Implies f1 f2) =
    map (Implies f1) $ splitConclusion f2
splitConclusion f = splitFormula f

splitFormula :: Form l -> [Form l]
splitFormula (And f1 f2) =
    splitFormula f1 ++ splitFormula f2
splitFormula f = [f] 

getFormulaSize :: Form l -> Int
getFormulaSize form =
    case form of
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

sortFormulasBySize :: [Form l] -> [Form l]
sortFormulasBySize formulas =
    map snd $ sortBy (\a b -> compare (fst a) (fst b)) $ map addSize formulas
    where
    addSize formula = (getFormulaSize formula, formula)

(/\) :: Form l -> Form l -> Form l
(/\) = And
(\/) :: Form l -> Form l -> Form l
(\/) = Or
(--->) :: Form l -> Form l -> Form l
(--->) = Implies
(|=|) :: Term l -> Term l -> Form l
(|=|) = Eq "anon"
(|==|) :: Term l -> Term l -> Form l
(|==|) = Eq "anon"
(|<|) :: Term l -> Term l -> Form l
(|<|) = Le "anon"
(|<=|) :: Term l -> Term l -> Form l
(|<=|) = Leq "anon"
(|>|) :: Term l -> Term l -> Form l
(|>|) = Ge "anon"
(|>=|) :: Term l -> Term l -> Form l
(|>=|) = Geq "anon"
(|<-|) :: Term l -> Term l -> Form l
(|<-|) = ContainedIn "anon"

newtype Term l = Term (Term' l, l)
  deriving (Eq,Data,Typeable)
data Term' l
  = Lit Rational
  | MinusInfinity
  | PlusInfinity
  | Var Int String -- numeric identifier, string only for printing
  | Hull (Term l) (Term l) -- interval covering both values
  | Plus (Term l) (Term l)
  | Minus (Term l) (Term l)
  | Neg (Term l)
  | Times (Term l) (Term l)
  | IntPower (Term l) (Term l)
  | Recip (Term l)
  | Over (Term l) (Term l)
  | Abs (Term l)
  | Min (Term l) (Term l)
  | Max (Term l) (Term l)
  | Pi
  | Sqrt (Term l)
  | Exp (Term l)
  | Sin (Term l)
  | Cos (Term l)
  | Atan (Term l)
  | Integral Int String (Term l) (Term l) (Term l) -- eg: Integral ivarId ivarName lower upper integrand
-- the following term constructors depend on a specific floating-point arithmetic: 
  | FEpsAbs Int Int
  | FEpsRel Int Int
  | FEpsiAbs Int Int
  | FEpsiRel Int Int
  | FRound Int Int (Term l)
  | FPlus Int Int (Term l) (Term l)
  | FMinus Int Int (Term l) (Term l)
  | FTimes Int Int (Term l) (Term l)
  | FOver Int Int (Term l) (Term l)
  | FSquare Int Int (Term l)
  | FSqrt Int Int (Term l)
  | FSin Int Int (Term l)
  | FCos Int Int (Term l)
  | FExp Int Int (Term l)
  deriving (Eq,Show,Read,Data,Typeable)

instance Show (Term l) where
    show (Term (term', _)) = show term'

class HasDefaultValue t
    where
    defaultValue :: t
    
instance HasDefaultValue (Maybe a) where
    defaultValue = Nothing

instance HasDefaultValue [a] where
    defaultValue = []

instance HasDefaultValue () where
    defaultValue = ()

instance HasDefaultValue l => Read (Term l) where
    readsPrec p s =
        map addTN $ readsPrec p s
        where
        addTN (t, cont) =
            (Term (t, defaultValue), cont) 

getTermSize :: (Term l) -> Int
getTermSize (Term (term, _)) =
    case term of
        Lit _r -> 1
        MinusInfinity -> 1
        PlusInfinity -> 1 
        Var _n _s -> 1
        Hull t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Plus t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Minus t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        Neg t -> 1 + (getTermSize t)
        Times t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        IntPower t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
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
        Integral _ivarId _ivarName lower upper integrand -> 
            2 + (getTermSize lower) + (getTermSize upper) + (getTermSize integrand)
        FEpsAbs _ _ -> 1
        FEpsRel _ _ -> 1
        FEpsiAbs _ _ -> 1
        FEpsiRel _ _ -> 1
        FRound _ _ t -> 1 + (getTermSize t)
        FPlus _ _ t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        FMinus _ _ t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        FTimes _ _ t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        FOver _ _ t1 t2 -> 1 + (getTermSize t1) + (getTermSize t2)
        FSquare _ _ t -> 1 + (getTermSize t)
        FSqrt _ _ t -> 1 + (getTermSize t)
        FSin _ _ t -> 1 + (getTermSize t)
        FCos _ _ t -> 1 + (getTermSize t)
        FExp _ _ t -> 1 + (getTermSize t)


{--- Operations for convenient encoding of literal values of type Term  ---}

termOp0 :: HasDefaultValue l => Term' l -> Term l
termOp0 op = Term (op, defaultValue)
termOp1 :: HasDefaultValue l => (t -> Term' l) -> t -> Term l
termOp1 op t1 = Term (op t1, defaultValue)
termOp2 :: HasDefaultValue l => (t -> t1 -> Term' l) -> t -> t1 -> Term l
termOp2 op t1 t2 = Term (op t1 t2, defaultValue)
termOp3 :: HasDefaultValue l => (t -> t1 -> t2 -> Term' l) -> t -> t1 -> t2 -> Term l
termOp3 op t1 t2 t3 = Term (op t1 t2 t3, defaultValue)

termVar :: HasDefaultValue l => Int -> String -> Term l
termVar varid name = termOp0 $ Var varid name

hull :: (HasDefaultValue l) => Term l -> Term l -> Term l
hull = termOp2 Hull

plusMinus :: (Eq l, HasDefaultValue l) => Term l -> Term l
plusMinus a = hull (negate a) a

instance (HasDefaultValue l, Eq l) => Ord (Term l)
  where
  min = termOp2 Min
  max = termOp2 Max
  compare (Term (Lit r1,_)) (Term (Lit r2,_)) = compare r1 r2
  compare (Term (MinusInfinity, _)) (Term (MinusInfinity, _)) = EQ
  compare (Term (PlusInfinity, _)) (Term (PlusInfinity, _)) = EQ
  compare (Term (MinusInfinity, _)) _ = LT
  compare _ (Term (MinusInfinity, _))= GT
  compare _ (Term (PlusInfinity, _))= LT
  compare (Term (PlusInfinity, _)) _ = GT
  compare _ _ = error "compare not implemented for datatype Term except for numeric literals"
  
instance (HasDefaultValue l, Eq l) => Num (Term l)
  where
  fromInteger n = termOp1 Lit $ fromInteger n
  negate = termOp1 Neg
  (+) = termOp2 Plus
  (*) = termOp2 Times
  abs = termOp1 Abs

plusInfinityTerm, minusInfinityTerm :: (HasDefaultValue l) => Term l
plusInfinityTerm = termOp0 PlusInfinity
minusInfinityTerm = termOp0 MinusInfinity

intPower :: (HasDefaultValue l) => Term l -> Term l -> Term l
intPower = termOp2 IntPower
square :: (HasDefaultValue l, Eq l) => Term l -> Term l
square = (`intPower` 2) 

instance (HasDefaultValue l, Eq l) => Real (Term l)
    where
    toRational (Term (Lit r, _)) = r
    toRational term = error $ "toRational undefined on term " ++ show term

--instance Enum Term
--    where    
--    
--instance Integral Term
--    where
    

instance (HasDefaultValue l, Eq l) => Fractional (Term l)
  where
  fromRational = termOp1 Lit
  recip = termOp1 Recip
  (/) = termOp2 Over

instance (HasDefaultValue l, Eq l) => Floating (Term l)
  where
  sqrt = termOp1 Sqrt
  exp = termOp1 Exp
  cos = termOp1 Cos
  sin = termOp1 Sin
  atan = termOp1 Atan

integral :: 
    HasDefaultValue l =>
    Int -> String -> Term l -> Term l -> Term l -> Term l
integral ivarId ivarName = termOp3 $ Integral ivarId ivarName

fepsAbs, fepsRel, fepsiAbs, fepsiRel :: (HasDefaultValue l) =>  Term l
depsAbs, depsRel, depsiAbs, depsiRel :: (HasDefaultValue l) =>  Term l
fepsAbs = termOp0 $ FEpsAbs 24 126
fepsRel = termOp0 $ FEpsRel 24 126
fepsiAbs = termOp0 $ FEpsiAbs 24 126
fepsiRel = termOp0 $ FEpsiRel 24 126
depsAbs = termOp0 $ FEpsAbs 53 1022
depsRel = termOp0 $ FEpsRel 53 1022
depsiAbs = termOp0 $ FEpsiAbs 53 1022
depsiRel = termOp0 $ FEpsiRel 53 1022

fround, dround :: (HasDefaultValue l) =>  Term l -> Term l
fround = termOp1 $ FRound 24 126
dround = termOp1 $ FRound 53 1022

(+:),(-:),(*:),(/:) :: (HasDefaultValue l) => Term l -> Term l -> Term l
(+:) = termOp2 $ FPlus 24 126
(-:) = termOp2 $ FMinus 24 126
(*:) = termOp2 $ FTimes 24 126
(/:) = termOp2 $ FOver 24 126

(+::),(-::),(*::),(/::) :: (HasDefaultValue l) => Term l -> Term l -> Term l
(+::) = termOp2 $ FPlus 53 1022
(-::) = termOp2 $ FMinus 53 1022
(*::) = termOp2 $ FTimes 53 1022
(/::) = termOp2 $ FOver 53 1022

uniascii :: String -> String -> String
uniascii unicode ascii 
    | useUnicode = unicode
    | otherwise = ascii
useUnicode :: Bool
useUnicode = (os /= "mingw32")

showForm :: 
    (HasDefaultValue l, Eq l) => 
    Int -> 
    (String -> l -> String) -> 
    (Form l) -> 
    String
showForm maxLength showLabel origForm = 
    shorten $ sf (Just 0) origForm
    where
    shorten s 
        | extraCharCount < 100 = s
        | otherwise = sStart ++ "\n...(cut " ++ show extraCharCount ++ " characters)...\n" ++ sEnd
        where
        extraCharCount = (length s) - (2 * maxLengthHalf) 
        sStart = take maxLengthHalf s
        sEnd = drop (length s - maxLengthHalf) s
        maxLengthHalf = maxLength `div` 2
    st maybeIndentLevel = showTermIL showLabel maybeIndentLevel
    sf Nothing form = sf2 Nothing form
    sf maybeIndentLevel form
        | length oneLineForm <= 60 = oneLineForm
        | otherwise = sf2 maybeIndentLevel form
        where
        oneLineForm = sf2 Nothing form
    sf2 maybeIndentLevel form
        = 
        case form of
            Not f -> (uniascii "¬" "~") ++ (indentedBracketsF f)
            Or f1 f2 -> showOpF (uniascii "∨" "\\/") f1 f2
            And f1 f2 -> showOpF (uniascii "∧" "/\\") f1 f2
            Implies f1 f2 -> showOpF (uniascii "⇒" "==>") f1 f2
            Le lab t1 t2 -> showOpT "<" lab t1 t2
            Leq lab t1 t2 -> showOpT (uniascii "≤" "<=") lab t1 t2
            Ge lab t1 t2 -> showOpT ">" lab t1 t2
            Geq lab t1 t2 -> showOpT (uniascii "≥" ">=") lab t1 t2
            Eq lab t1 t2 -> showOpT "=" lab t1 t2
            Neq lab t1 t2 -> showOpT (uniascii "≠" "!=") lab t1 t2
            ContainedIn lab t1 t2 -> showOpT (uniascii "⊆" "subseteq") lab t1 t2
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
        showPred lab predicate ts =
            labelIfInline lab
            ++ predicate ++ "("
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
        indentedBracketsF form2
            | isAtomicForm form2 = sf maybeIndentLevel form2
            | otherwise = "(" ++ indentNext ++ sfNext form2 ++ indent ++ ")"
        maybeNextIndentLevel = fmap (+ 2) maybeIndentLevel

showTerm :: (HasDefaultValue l, Eq l) => (String -> l -> String) -> (Term l) -> String
showTerm showLabel term = showTermIL showLabel (Just 0) term

showTermIL :: 
    (HasDefaultValue l, Eq l) => (String -> l -> String) -> (Maybe Int) -> (Term l) -> String
showTermIL showLabel = st
    where
    st Nothing term = st2 Nothing term
    st maybeIndentLevel term
        | length oneLineTerm<= 60 = oneLineTerm
        | otherwise = st2 maybeIndentLevel term
        where
        oneLineTerm = st2 Nothing term

    st2 maybeIndentLevel (Term (term2, label2))
        =
        case term2 of
            Lit r -> 
                if floor r == (ceiling r :: Integer) 
                    then addLabel $ show (floor r :: Integer) 
                    else addLabel $ show (numerator r) ++ "/" ++ show (denominator r)
            PlusInfinity -> uniascii "+∞" "+oo"
            MinusInfinity -> uniascii "-∞" "-oo"
            Var _n s -> addLabel s
            Hull t1 t2 -> showOpT ".." "hull" t1 t2
            Plus t1 t2 -> showOpT "+" "sum" t1 t2
            Minus t1 t2 -> showOpT "-" "diff" t1 t2
            Neg t -> showFnT "-" [t]
            Times t1 t2 -> showOpT "*" "prod" t1 t2
            IntPower t1 t2 -> formatIntPower t1 t2
            Recip t -> st2 maybeIndentLevel $ Term (Over 1 t, label2)
            Over t1 t2 -> showOpT "/" "div" t1 t2
            Abs t -> indentedOpenCloseT "|" "|" False t
            Min t1 t2 -> showFnT "min" [t1, t2]
            Max t1 t2 -> showFnT "max" [t1, t2]
            Pi -> addLabel (uniascii "π" "pi")
            Sqrt t -> showFnT "sqrt" [t]
            Exp t -> showFnT "exp" [t]
            Sin t -> showFnT "sin" [t]
            Cos t -> showFnT "cos" [t]
            Atan t -> showFnT "atan" [t]
            Integral ivarId ivarName lower upper integrand -> 
                showFnT "∫" [lower, upper, Term (Var ivarId ivarName, defaultValue), integrand]
            FEpsAbs _ _ -> addLabel $ uniascii "εabs" "fepsAbs"
            FEpsRel _ _ -> addLabel $ uniascii "εrel" "fepsRel"
            FEpsiAbs _ _ -> addLabel $ uniascii "εabsI" "fepsiAbs"
            FEpsiRel _ _ -> addLabel $ uniascii "εrelI" "fepsiRel"
            FRound _ _ t -> showFnT "rnd" [t]
            FPlus _ _ t1 t2 -> showOpT (uniascii "⊕" "(+)") "sum" t1 t2
            FMinus _ _ t1 t2 -> showOpT (uniascii "⊖" "(-)") "diff" t1 t2
            FTimes _ _ t1 t2 -> showOpT (uniascii "⊛" "(*)") "prod" t1 t2
            FOver _ _ t1 t2 -> showOpT (uniascii "⊘" "(/)") "div" t1 t2
            FSquare _ _ t -> showFnT "fsquare" [t]
            FSqrt _ _ t -> showFnT "fsqrt" [t]
            FSin _ _ t -> showFnT "fsin" [t]
            FCos _ _ t -> showFnT "fcos" [t]
            FExp _ _ t -> showFnT "fexp" [t]
        where
        formatIntPower t1 t2 =
            case t2 of
                (Term (Lit n, _)) | denominator n == 1 -> 
                    indentedOpenCloseT "(" (")^" ++ show (numerator n)) False t1 
                _ ->
                    (indentedOpenCloseT "(" ")^" False t1)
                    ++
                    (indentedOpenCloseT "(" ")" False t2)
        stNext = st maybeNextIndentLevel
        indent = 
            case maybeIndentLevel of 
                Just indentLevel -> "\n" ++ replicate indentLevel ' '
                _ -> ""
        indentNext = 
            case maybeNextIndentLevel of
                Just indentLevel -> "\n" ++ replicate indentLevel ' '
                _ -> ""
        addLabel s = showLabel s label2
        showOpT op _opname t1 t2 =
            case maybeNextIndentLevel of
                Just _ ->
                    indent ++ showLabel "" label2
                    ++ indent ++ indentedBracketsT t1
                    ++ indent ++ op
                    ++ indent ++ indentedBracketsT t2
                _ -> 
                    tIfInlineOpen
                    ++ indentedBracketsT t1 
                    ++ padIfInline op
                    ++ indentedBracketsT t2
                    ++ tIfInlineClose
            where
            tIfInlineOpen = "(" 
            tIfInlineClose = ")" ++ showLabel "" label2 
        showFnT fn ts =
            fn ++ tIfIndented ++ "("
            ++ (intercalate (indent ++ ", ") $ map (\t -> indentNext ++ stNext t) ts)
            ++ indent ++ ")"
            ++ showLabel "" label2
            where
            tIfIndented =
                case (maybeNextIndentLevel) of
                    (Just _) -> showLabel "res" label2
                    _ -> ""
        padIfInline op = case maybeIndentLevel of Nothing -> " " ++ op ++ " "; _ -> op 
        indentedBracketsT = indentedOpenCloseT "(" ")" True
        indentedOpenCloseT open close optional term
            | optional && isAtomicTerm term = st maybeIndentLevel term
            | optional = 
                open ++ indentNext ++ stNext term ++ indent ++ close
            | otherwise = 
                open ++ tIfIndented ++ indentNext ++ stNext term ++ indent ++ addLabel close
            where
            tIfIndented =
                case (maybeNextIndentLevel) of
                    (Just _) -> showLabel "res" label2
                    _ -> ""
        maybeNextIndentLevel = fmap (+ 2) maybeIndentLevel
        isAtomicTerm :: Term l -> Bool
        isAtomicTerm (Term (term, _)) = 
            case term of
                (Lit _) -> True
                PlusInfinity -> True
                MinusInfinity -> True
                (Var _ _) -> True
                (Neg t) -> isAtomicTerm t
                Pi -> True
                FEpsAbs _ _ -> True
                FEpsRel _ _ -> True
                FEpsiAbs _ _ -> True
                FEpsiRel _ _ -> True
                _ -> False


