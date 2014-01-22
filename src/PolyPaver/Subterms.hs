module PolyPaver.Subterms 
(
    addHashesInForm,
    addHashesInTerm,
    TermHash
)
where

import PolyPaver.Form

import qualified Data.IntMap as IMap

import Data.Hashable

--import Data.Data (Data, Typeable)
--import Data.List (intercalate, sortBy)

type TermHash = Int

addHashesInForm :: 
    (Hashable l, Eq l) =>
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
    (Hashable l, Eq l) =>
    Term l -> Term TermHash
addHashesInTerm term =
    fst $ addHashesInTermAux term 
    where
    addHashesInTermAux (Term (term', _l)) =
        (Term (termWithHashes, theHash), IMap.insert theHash term' termIndex)
        where
        theHash = hash term'
        (termWithHashes, termIndex) = addHashesInTerm' term'
    addHashesInTerm' term' =
        case term' of
            Lit r -> addHashes0 (Lit r)
            MinusInfinity -> addHashes0 MinusInfinity
            PlusInfinity -> addHashes0 PlusInfinity 
            Var n s -> addHashes0 (Var n s)
            Hull t1 t2 -> addHashes2 Hull t1 t2
            Plus t1 t2 -> addHashes2 Plus t1 t2
            Minus t1 t2 -> addHashes2 Minus t1 t2
            Neg t -> addHashes1 Neg t
            Times t1 t2 -> addHashes2 Times t1 t2
            Square t -> addHashes1 Square t
            IntPower t1 t2 -> addHashes2 IntPower t1 t2
            Recip t -> addHashes1 Recip t
            Over t1 t2 -> addHashes2 Over t1 t2
            Abs t -> addHashes1 Abs t
            Min t1 t2 -> addHashes2 Min t1 t2
            Max t1 t2 -> addHashes2 Max t1 t2
            Pi -> addHashes0 Pi
            Sqrt t -> addHashes1 Sqrt t
            Exp t -> addHashes1 Exp t
            Sin t -> addHashes1 Sin t
            Cos t -> addHashes1 Cos t
            Atan t -> addHashes1 Atan t
            Integral ivarId ivarName lower upper integrand ->
                addHashes3 (Integral ivarId ivarName) lower upper integrand 
            FEpsAbs a r -> addHashes0 $ FEpsAbs a r
            FEpsRel a r -> addHashes0 $ FEpsRel a r
            FEpsiAbs a r -> addHashes0 $ FEpsiAbs a r
            FEpsiRel a r -> addHashes0 $ FEpsiRel a r
            FRound a r t -> addHashes1 (FRound a r) t
            FPlus a r t1 t2 -> addHashes2 (FPlus a r) t1 t2
            FMinus a r t1 t2 -> addHashes2 (FMinus a r) t1 t2
            FTimes a r t1 t2 -> addHashes2 (FTimes a r) t1 t2
            FOver a r t1 t2 -> addHashes2 (FOver a r) t1 t2
            FSquare a r t -> addHashes1 (FSquare a r) t
            FSqrt a r t -> addHashes1 (FSqrt a r) t
            FSin a r t -> addHashes1 (FSin a r) t
            FCos a r t -> addHashes1 (FCos a r) t
            FExp a r t -> addHashes1 (FExp a r) t
    addHashes0 op = 
        (op, IMap.empty)
    addHashes1 op t1 =
        (op t1H, termIndex1)
        where
        (t1H, termIndex1) = addHashesInTermAux t1
    addHashes2 op t1 t2 =
        (op t1H t2H, termIndex)
        where
        (t1H, termIndex1) = addHashesInTermAux t1
        (t2H, termIndex2) = addHashesInTermAux t2
        termIndex = IMap.unionWith checkSame termIndex1 termIndex2
    addHashes3 op t1 t2 t3 =
        (op t1H t2H t3H, termIndex)
        where
        (t1H, termIndex1) = addHashesInTermAux t1
        (t2H, termIndex2) = addHashesInTermAux t2
        (t3H, termIndex3) = addHashesInTermAux t3
        termIndex =
            IMap.unionWith checkSame termIndex1 $
                IMap.unionWith checkSame termIndex2 termIndex3
    checkSame t1 t2 
        | t1 == t2 = t1
        | otherwise = 
            error $ "A very rare internal error has occurred.  Two different terms have the same hash."             
