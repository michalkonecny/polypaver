module PolyPaver.Subterms 
(
    addHashesInForm,
    addHashesInTerm,
    TermHash
)
where

import PolyPaver.Form

import Data.Hashable

--import Data.Data (Data, Typeable)
--import Data.List (intercalate, sortBy)

type TermHash = Int

addHashesInForm :: 
    (Hashable l) =>
    Form l -> Form TermHash
addHashesInForm form =
    case form of
        Not arg -> expOp1 Not arg
        Or left right -> expOp2 Or left right 
        And left right -> expOp2 And left right
        Implies left right -> expOp2 Implies left right
        Le lab left right -> expT2 Le lab left right 
        Leq lab left right -> expT2 Leq lab left right
        Ge lab left right -> expT2 Ge lab right left
        Geq lab left right -> expT2 Geq lab right left
        Eq lab left right -> expT2 Eq lab left right
        Neq lab left right -> expT2 Neq lab left right
        ContainedIn lab left right -> expT2 ContainedIn lab left right
        IsRange lab t lower upper -> expT3 IsRange lab t lower upper
        IsIntRange lab t lower upper -> expT3 IsIntRange lab t lower upper 
        IsInt lab t -> expT1 IsInt lab t
    where
    expOp1 op arg = op (addHashesInForm arg)
    expOp2 op arg1 arg2 = op (addHashesInForm arg1) (addHashesInForm arg2)
    expT1 op lab t = op lab (addHashesInTerm t)
    expT2 op lab t1 t2 = op lab (addHashesInTerm t1) (addHashesInTerm t2)
    expT3 op lab t1 t2 t3 = op lab (addHashesInTerm t1) (addHashesInTerm t2) (addHashesInTerm t3)

addHashesInTerm ::
    Hashable l =>
    Term l -> Term TermHash
addHashesInTerm (Term (term', _l)) =
    (Term (termWithHashes, hash term'))
    where
    termWithHashes =
        case term' of
            Lit r -> Lit r
            MinusInfinity -> MinusInfinity
            PlusInfinity -> PlusInfinity 
            Var n s -> Var n s
            Hull t1 t2 -> addHashes2 Hull t1 t2
            Plus t1 t2 -> addHashes2 Plus t1 t2
            Minus t1 t2 -> addHashes2 Minus t1 t2
            Neg t -> Neg (addHashesInTerm t)
            Times t1 t2 -> addHashes2 Times t1 t2
            Square t -> Square (addHashesInTerm t)
            IntPower t1 t2 -> addHashes2 IntPower t1 t2
            Recip t -> Recip (addHashesInTerm t)
            Over t1 t2 -> addHashes2 Over t1 t2
            Abs t -> Abs (addHashesInTerm t)
            Min t1 t2 -> addHashes2 Min t1 t2
            Max t1 t2 -> addHashes2 Max t1 t2
            Pi -> Pi
            Sqrt t -> Sqrt (addHashesInTerm t)
            Exp t -> Exp (addHashesInTerm t)
            Sin t -> Sin (addHashesInTerm t)
            Cos t -> Cos (addHashesInTerm t)
            Atan t -> Atan (addHashesInTerm t)
            Integral ivarId ivarName lower upper integrand ->
                Integral ivarId ivarName (addHashesInTerm lower) (addHashesInTerm upper) (addHashesInTerm integrand) 
            FEpsAbs a r -> FEpsAbs a r
            FEpsRel a r -> FEpsRel a r
            FEpsiAbs a r -> FEpsiAbs a r
            FEpsiRel a r -> FEpsiRel a r
            FRound a r t -> FRound a r (addHashesInTerm t)
            FPlus a r t1 t2 -> addHashes2 (FPlus a r) t1 t2
            FMinus a r t1 t2 -> addHashes2 (FMinus a r) t1 t2
            FTimes a r t1 t2 -> addHashes2 (FTimes a r) t1 t2
            FOver a r t1 t2 -> addHashes2 (FOver a r) t1 t2
            FSquare a r t -> FSquare a r (addHashesInTerm t)
            FSqrt a r t -> FSqrt a r(addHashesInTerm t)
            FSin a r t -> FSin a r (addHashesInTerm t)
            FCos a r t -> FCos a r (addHashesInTerm t)
            FExp a r t -> FExp a r (addHashesInTerm t)
    addHashes2 op t1 t2 =
        op t1H t2H
        where
        t1H = addHashesInTerm t1
        t2H = addHashesInTerm t2            
