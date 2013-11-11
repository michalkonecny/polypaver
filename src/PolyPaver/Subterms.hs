{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PolyPaver.Subterms where

import PolyPaver.Form

import Data.Hashable

--import Data.Data (Data, Typeable)
--import Data.List (intercalate, sortBy)

addHashes ::
    Hashable a =>
    Term a -> Term Int
addHashes (Term (term', _l)) =
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
            Neg t -> Neg (addHashes t)
            Times t1 t2 -> addHashes2 Times t1 t2
            Square t -> Square (addHashes t)
            IntPower t1 t2 -> addHashes2 IntPower t1 t2
            Recip t -> Recip (addHashes t)
            Over t1 t2 -> addHashes2 Over t1 t2
            Abs t -> Abs (addHashes t)
            Min t1 t2 -> addHashes2 Min t1 t2
            Max t1 t2 -> addHashes2 Max t1 t2
            Pi -> Pi
            Sqrt t -> Sqrt (addHashes t)
            Exp t -> Exp (addHashes t)
            Sin t -> Sin (addHashes t)
            Cos t -> Cos (addHashes t)
            Atan t -> Atan (addHashes t)
            Integral ivarId ivarName lower upper integrand ->
                Integral ivarId ivarName (addHashes lower) (addHashes upper) (addHashes integrand) 
            FEpsAbs a r -> FEpsAbs a r
            FEpsRel a r -> FEpsRel a r
            FEpsiAbs a r -> FEpsiAbs a r
            FEpsiRel a r -> FEpsiRel a r
            FRound a r t -> FRound a r (addHashes t)
            FPlus a r t1 t2 -> addHashes2 (FPlus a r) t1 t2
            FMinus a r t1 t2 -> addHashes2 (FMinus a r) t1 t2
            FTimes a r t1 t2 -> addHashes2 (FTimes a r) t1 t2
            FOver a r t1 t2 -> addHashes2 (FOver a r) t1 t2
            FSquare a r t -> FSquare a r (addHashes t)
            FSqrt a r t -> FSqrt a r(addHashes t)
            FSin a r t -> FSin a r (addHashes t)
            FCos a r t -> FCos a r (addHashes t)
            FExp a r t -> FExp a r (addHashes t)
    addHashes2 op t1 t2 =
        op t1H t2H
        where
        t1H = addHashes t1
        t2H = addHashes t2            
